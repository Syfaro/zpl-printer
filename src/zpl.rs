use std::io::Write;

use base64::Engine;
use flate2::{Compression, write::ZlibEncoder};
use image::{DynamicImage, ImageBuffer, Luma, LumaA, buffer::ConvertBuffer, imageops};
use itertools::Itertools;
use nom::{
    Parser,
    branch::alt,
    bytes::complete::{is_not, tag_no_case, take},
    character::complete::{char, multispace0},
    combinator::opt,
    multi::many0,
};
use tracing::{debug, instrument, trace};

#[derive(Clone, Copy, Debug, PartialEq, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DitheringType {
    Ordered,
    FloydSteinberg,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BinaryEncodingMethod {
    #[default]
    Hex,
    Z64,
}

/// Convert an image to a GF instruction.
///
/// This also handles the grayscale conversion, including dithering.
#[instrument(skip(im))]
pub fn image_to_gf(
    im: &DynamicImage,
    dithering: Option<DitheringType>,
    encoding: BinaryEncodingMethod,
) -> String {
    let (cleaned_image, height, width_padded) = process_image(im, dithering);
    let (field_data, line_size, total_size) =
        encode_image(height, width_padded, encoding, &cleaned_image);

    format!("^GFA,{total_size},{total_size},{line_size},{field_data}")
}

/// Convert an image for ZPL printing.
pub fn process_image(
    im: &DynamicImage,
    dithering: Option<DitheringType>,
) -> (ImageBuffer<Luma<u8>, Vec<u8>>, u32, u32) {
    // Convert image to grayscale with alpha channel. We need to make sure that
    // transparent pixels are set to white so they aren't printed.
    let im = im.to_luma_alpha8();

    let (width, height) = im.dimensions();
    debug!(width, height, "got image dimensions");

    // ZPL requires that all image widths are padded to a multiple of 8.
    let padding = width % 8;
    let width_padded = width + padding;
    debug!(padding, "calculated needed padding");

    // Create a new image with the padded width, then pull pixels from the
    // original image.
    let mut cleaned_image = ImageBuffer::<Luma<u8>, _>::from_fn(width_padded, height, |x, y| {
        let pixel = im.get_pixel_checked(x, y).unwrap_or(&LumaA([255, 255]));

        // If the pixel is mostly transparent, make it white.
        let val = if pixel.0[1] > 127 { pixel.0[0] } else { 255 };
        Luma([val])
    });
    debug!(width = width_padded, height, "generated image");

    // If desired, apply dithering to the image to improve the black and white
    // conversion.
    match dithering {
        Some(DitheringType::FloydSteinberg) => {
            imageops::dither(&mut cleaned_image, &imageops::BiLevel)
        }
        Some(DitheringType::Ordered) => {
            let map = dithord::ThresholdMap::level(2);
            let width = cleaned_image.width() as usize;

            let test_pixel = |luma, x, y| luma > map.sample(x, y);

            let mut copy: ImageBuffer<Luma<f32>, _> = cleaned_image.convert();
            copy.pixels_mut().enumerate().for_each(|(i, pixel)| {
                pixel.0[0] = test_pixel(pixel.0[0], i % width, i / width) as u32 as f32;
            });
            cleaned_image = copy.convert();
        }
        None => (),
    }

    (cleaned_image, height, width_padded)
}

/// Encode image using the provided encoding method.
pub fn encode_image(
    height: u32,
    width_padded: u32,
    encoding: BinaryEncodingMethod,
    cleaned_image: &ImageBuffer<Luma<u8>, Vec<u8>>,
) -> (String, usize, usize) {
    // Number of bytes that each line uses.
    let line_size = (width_padded / 8) as usize;
    // Total size of uncompressed data.
    let total_size = line_size * height as usize;

    let field_data = match encoding {
        BinaryEncodingMethod::Hex => hex_encoding(line_size, height, width_padded, cleaned_image),
        BinaryEncodingMethod::Z64 => z64_encoding(line_size, height, width_padded, cleaned_image),
    };

    (field_data, line_size, total_size)
}

// Create an iterator of byte data for each line of an image.
fn image_to_lines(
    line_size: usize,
    height: u32,
    width_padded: u32,
    cleaned_image: &ImageBuffer<Luma<u8>, Vec<u8>>,
) -> impl Iterator<Item = Vec<u8>> {
    // A buffer for holding bytes for the current line.
    let mut buf = Vec::with_capacity(line_size);
    // A work in progress byte that gets updated with each pixel.
    let mut byte = 0u8;

    (0..height).map(move |y| {
        buf.clear();

        for x in 0..width_padded {
            let pos = x % 8;
            let pixel = cleaned_image.get_pixel(x, y).0[0];

            // Need to compress the u8 value into a single bit, so use half of
            // the maximum value as a threshold and shift it into the
            // appropriate bit of our buffer byte.
            if pixel < 128 {
                byte |= 1 << (7 - pos);
            }

            // After processing a complete byte, push it to the buffer. We know
            // this will happen every loop because our image's width was padded
            // to be a multiple of 8.
            if pos == 7 {
                buf.push(byte);
                byte = 0;
            }
        }

        buf.clone()
    })
}

// Perform hex encoding for image.
fn hex_encoding(
    line_size: usize,
    height: u32,
    width_padded: u32,
    cleaned_image: &ImageBuffer<Luma<u8>, Vec<u8>>,
) -> String {
    // The image data, after compression.
    let mut field_data = String::new();

    // Used to keep track of the value from the last line so we can compress it
    // when they are the same.
    let mut last_line = String::new();

    for line in image_to_lines(line_size, height, width_padded, cleaned_image) {
        // Now that we're done with the line, we can encode it into a hex
        // representation and compress that value.
        let hex_buf = hex::encode_upper(&line);
        let compressed_line = zpl_compress_line(&hex_buf);

        // ZPL compression says we can use a colon instead of needing to include
        // the entire line again if they were the same.
        if compressed_line == last_line {
            field_data.push(':');
        } else {
            field_data.push_str(&compressed_line);
            last_line = compressed_line;
        }
    }

    debug!(compressed_size = field_data.len(), "got total bytes");

    field_data
}

/// Compress an entire line of data using the ZPL ASCII compression system.
fn zpl_compress_line(inp: &str) -> String {
    let mut line = String::new();

    let mut chars = inp.chars().peekable();

    while let Some(c) = chars.next() {
        // Advance the iterator to consume all of the following characters that
        // are the same as the current character.
        let following_count = chars.peeking_take_while(|n| *n == c).count();

        // While compression only makes a difference for more than 1 following
        // character, it makes the code cleaner to always use it.
        if following_count > 0 {
            // If we end on one of these repeating characters, use the special
            // case compression.
            if (c == '0' || c == 'F') && chars.peek().is_none() {
                match c {
                    '0' => line.push(','),
                    'F' => line.push('!'),
                    _ => unreachable!(),
                }
            } else {
                line.push_str(&zpl_repeat_code(c, following_count + 1));
            }
        } else {
            line.push(c);
        }
    }

    line
}

/// Compress a repeating character using the ZPL ASCII compression scheme.
fn zpl_repeat_code(c: char, mut count: usize) -> String {
    let mut s = String::new();

    // Lookup tables for characters. Each character is positioned such that
    // index 0 is 1 instance and is 'g,' index 1 is 2 instances and is 'h,' etc.
    const HIGH_CHAR: &[u8; 20] = b"ghijklmnopqrstuvwxyz";
    const LOW_CHAR: &[u8; 19] = b"GHIJKLMNOPQRSTUVWXY";

    // This scheme has a maximum value, so we just repeat the process if there
    // were somehow that many repeating characters in a row.
    const MAX_REPEAT: usize = 419;
    while count > MAX_REPEAT {
        s.push_str(&zpl_repeat_code(c, MAX_REPEAT));
        count -= MAX_REPEAT;
    }

    // The compression schemed is organized into two sets, the high and low
    // values. These then can be added together to create any value from
    // 1 to 419.

    let high = count / 20;
    let low = count % 20;

    if high > 0 {
        s.push(HIGH_CHAR[high - 1] as char);
    }

    if low > 0 {
        s.push(LOW_CHAR[low - 1] as char);
    }

    format!("{s}{c}")
}

#[instrument(skip(cleaned_image))]
fn z64_encoding(
    line_size: usize,
    height: u32,
    width_padded: u32,
    cleaned_image: &ImageBuffer<Luma<u8>, Vec<u8>>,
) -> String {
    // Get all image lines as a single buffer.
    let mut buf = Vec::new();
    for line in image_to_lines(line_size, height, width_padded, cleaned_image) {
        buf.extend(line);
    }

    // Compress all image data.
    let mut compressor = ZlibEncoder::new(Vec::new(), Compression::best());
    compressor
        .write_all(&buf)
        .expect("should always be able to write image bytes");
    let data = compressor
        .finish()
        .expect("should always be able to finish compressor");
    trace!("compressed data was {} bytes", data.len());

    // Convert compressed data to base64 string.
    let mut image_data = String::new();
    base64::prelude::BASE64_STANDARD.encode_string(data, &mut image_data);
    trace!("base64 data was {} bytes", image_data.len());

    // And take the CRC of that data.
    let crc = crc16::State::<crc16::XMODEM>::calculate(image_data.as_bytes());
    let crc = hex::encode(crc.to_be_bytes());
    trace!("base64 data crc was {crc}");

    // Finally, format into the expected format.
    format!(":Z64:{image_data}:{crc}")
}

#[derive(Debug)]
pub struct ZplParser {
    caret: char,
    tilde: char,
    in_label: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ZplCommand<'a> {
    pub start: char,
    pub name: &'a str,
    pub args: &'a str,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ZplLabel<'a> {
    pub commands: &'a [ZplCommand<'a>],
}

#[derive(Clone, Debug)]
pub struct ZplHostVerification<'a> {
    pub field_no: u16,
    pub length: usize,
    pub prefix: Option<&'a str>,
    pub suffix: Option<&'a str>,
    pub applies_to: Option<char>,
}

impl Default for ZplParser {
    fn default() -> Self {
        Self {
            caret: '^',
            tilde: '~',
            in_label: false,
        }
    }
}

impl ZplParser {
    pub fn parse<'a>(input: &'a str) -> eyre::Result<Vec<ZplCommand<'a>>> {
        let mut parser = Self::default();

        many0(|s| parser.parse_command(s))
            .parse(input)
            .map(|(_, commands)| commands)
            .map_err(|err| eyre::eyre!("could not parse zpl: {err}"))
    }

    pub fn split_labels<'a>(mut commands: &'a [ZplCommand<'a>]) -> Vec<ZplLabel<'a>> {
        let mut labels = Vec::new();

        while !commands.is_empty() {
            let (label_commands, remaining) =
                match commands.iter().position(|command| command.is('^', "XZ")) {
                    Some(end_index) => commands
                        .split_at_checked(end_index + 1)
                        .map(|(label_commands, remaining)| (label_commands, Some(remaining)))
                        .unwrap_or_else(|| (commands, None)),
                    None => (commands, None),
                };

            labels.push(ZplLabel {
                commands: label_commands,
            });

            if let Some(remaining) = remaining {
                commands = remaining;
            } else {
                break;
            }
        }

        labels
    }

    #[instrument]
    fn parse_command<'a>(&mut self, input: &'a str) -> nom::IResult<&'a str, ZplCommand<'a>> {
        let (input, _) = multispace0(input)?;

        let (input, command) = opt(alt((
            (char(self.caret), tag_no_case([b'A', b'@'].as_slice())),
            (char(self.caret), tag_no_case([b'A'].as_slice())),
        )))
        .parse(input)?;

        let (input, start, name) = if let Some((start, name)) = command {
            (input, start, name)
        } else {
            let (input, (caret, name)) =
                (alt((char(self.caret), char(self.tilde))), take(2usize)).parse(input)?;
            (input, caret, name)
        };
        trace!(%start, name, "identified command");
        let normalized_command_start = if start == self.caret { '^' } else { '~' };

        let (input, args) = if start == self.caret && name.eq_ignore_ascii_case("CC") {
            let (input, arg) = take(1usize)(input)?;
            trace!("changing caret symbol to {arg}");
            self.caret = arg.chars().next().unwrap();
            (input, arg)
        } else if start == self.caret && name.eq_ignore_ascii_case("CT") {
            let (input, arg) = take(1usize)(input)?;
            trace!("changing tilde symbol to {arg}");
            self.tilde = arg.chars().next().unwrap();
            (input, arg)
        } else {
            let (input, args) = opt(is_not([self.caret, self.tilde].as_slice())).parse(input)?;
            let args = args.unwrap_or_default();
            trace!(args, "got command arguments");
            (input, args)
        };

        let command = ZplCommand {
            start: normalized_command_start,
            name,
            args: args.trim(),
        };

        if command.is('^', "XA") {
            self.in_label = true;
        } else if command.is('^', "XZ") {
            self.in_label = false;
        }

        Ok((input, command))
    }
}

impl<'a> ZplCommand<'a> {
    pub fn is(&self, start: char, name: &str) -> bool {
        debug_assert!(start == '^' || start == '~', "unknown start character");
        self.start == start && self.name == name
    }
}

impl<'a> ZplLabel<'a> {
    pub fn quantity(&self) -> usize {
        self.commands
            .iter()
            .rfind(|command| command.is('^', "PQ"))
            .and_then(|command| command.args.parse().ok())
            .unwrap_or(1)
    }

    pub fn iter_host_verifications(
        &self,
        host_verifications: &'a [ZplHostVerification<'a>],
    ) -> ZplLabelHostVerificationIter<'a> {
        ZplLabelHostVerificationIter::new(host_verifications, self.quantity())
    }
}

impl<'a> ZplHostVerification<'a> {
    pub fn from_commands(commands: &[ZplCommand<'a>]) -> Vec<ZplHostVerification<'a>> {
        commands
            .iter()
            .filter(|command| command.is('^', "HV"))
            .filter_map(|hv| {
                let mut args = hv.args.splitn(5, ',');
                let field_no = args.next().unwrap_or("0").parse().ok()?;
                let length = args.next().unwrap_or("64").parse().ok()?;
                let prefix = args.next().filter(|s| !s.is_empty());
                let suffix = args.next().filter(|s| !s.is_empty());
                let applies_to = args.next().and_then(|applies_to| applies_to.chars().next());

                Some(ZplHostVerification {
                    field_no,
                    length,
                    prefix,
                    suffix,
                    applies_to,
                })
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct ZplLabelHostVerificationIter<'a> {
    quantity: usize,
    host_verifications: &'a [ZplHostVerification<'a>],
    label_verifications: Vec<&'a ZplHostVerification<'a>>,

    current_label: usize,
    current_verification: usize,
}

impl<'a> ZplLabelHostVerificationIter<'a> {
    fn new(host_verifications: &'a [ZplHostVerification<'a>], quantity: usize) -> Self {
        let label_verifications = host_verifications
            .iter()
            .filter(|hv| hv.applies_to == Some('L'))
            .collect();

        Self {
            quantity,
            host_verifications,
            label_verifications,

            current_label: 1,
            current_verification: 0,
        }
    }
}

impl<'a> Iterator for ZplLabelHostVerificationIter<'a> {
    type Item = (usize, &'a ZplHostVerification<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        // If we're on the last label, all host verifications are valid.
        // Otherwise, we need to only consider label-level verifications.
        let current_verification = if self.current_label == self.quantity {
            self.host_verifications.get(self.current_verification)
        } else {
            self.label_verifications
                .get(self.current_verification)
                .copied()
        };

        // When the value exists we can directly use it. If it doesn't we need
        // to reset positions, advance to the next label, and try again.
        if let Some(verification) = current_verification {
            self.current_verification += 1;
            Some((self.current_label, verification))
        } else if self.current_label < self.quantity {
            self.current_label += 1;
            self.current_verification = 0;
            self.next()
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::zpl::{ZplCommand, ZplHostVerification, ZplLabel, ZplParser};

    #[test]
    fn test_parse_basic_commands() {
        let input = "  ^XA\n\n^FDtest^XZ";
        let commands: Vec<_> = ZplParser::parse(input).unwrap();

        assert_eq!(
            commands,
            vec![
                ZplCommand {
                    start: '^',
                    name: "XA",
                    args: ""
                },
                ZplCommand {
                    start: '^',
                    name: "FD",
                    args: "test",
                },
                ZplCommand {
                    start: '^',
                    name: "XZ",
                    args: ""
                }
            ]
        );
    }

    #[test]
    fn test_parse_a_command() {
        let input = "^A@test ^AU";
        let commands = ZplParser::parse(input).unwrap();

        assert_eq!(
            commands,
            vec![
                ZplCommand {
                    start: '^',
                    name: "A@",
                    args: "test"
                },
                ZplCommand {
                    start: '^',
                    name: "A",
                    args: "U",
                },
            ]
        );
    }

    #[test]
    fn test_parse_changes() {
        let input = "^CC[[FDtest^FDtest";
        let commands = ZplParser::parse(input).unwrap();

        assert_eq!(
            commands,
            vec![
                ZplCommand {
                    start: '^',
                    name: "CC",
                    args: "["
                },
                ZplCommand {
                    start: '^',
                    name: "FD",
                    args: "test^FDtest"
                },
            ]
        );
    }

    #[test]
    fn test_split_labels() {
        let input = "^XA^FS^XZ";
        let commands = ZplParser::parse(input).unwrap();
        let labels = ZplParser::split_labels(&commands);

        assert_eq!(
            labels,
            vec![ZplLabel {
                commands: &[
                    ZplCommand {
                        start: '^',
                        name: "XA",
                        args: ""
                    },
                    ZplCommand {
                        start: '^',
                        name: "FS",
                        args: ""
                    },
                    ZplCommand {
                        start: '^',
                        name: "XZ",
                        args: ""
                    }
                ]
            }]
        );

        let input = "^XA^FS^XZ^XA^FD";
        let commands = ZplParser::parse(input).unwrap();
        let labels = ZplParser::split_labels(&commands);

        assert_eq!(
            labels,
            vec![
                ZplLabel {
                    commands: &[
                        ZplCommand {
                            start: '^',
                            name: "XA",
                            args: ""
                        },
                        ZplCommand {
                            start: '^',
                            name: "FS",
                            args: ""
                        },
                        ZplCommand {
                            start: '^',
                            name: "XZ",
                            args: ""
                        }
                    ]
                },
                ZplLabel {
                    commands: &[
                        ZplCommand {
                            start: '^',
                            name: "XA",
                            args: ""
                        },
                        ZplCommand {
                            start: '^',
                            name: "FD",
                            args: ""
                        },
                    ]
                }
            ]
        );
    }

    #[test]
    fn test_iter_host_verifications() {
        let input = "^XA^HV1,20,[,],F^FS^HV2,10,START(,),L^FS^PQ2^XZ";
        let commands = ZplParser::parse(input).unwrap();
        let label = ZplParser::split_labels(&commands).pop().unwrap();
        let host_verifications = ZplHostVerification::from_commands(&commands);
        let hvs: Vec<_> = label.iter_host_verifications(&host_verifications).collect();
        println!("{hvs:?}");
    }
}
