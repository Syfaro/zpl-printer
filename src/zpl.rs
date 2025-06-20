use std::collections::HashMap;

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
use tracing::{debug, instrument};

#[derive(Clone, Copy, Debug, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DitheringType {
    Ordered,
    FloydSteinberg,
}

/// Convert an image to a GF instruction.
///
/// This also handles the grayscale conversion, including dithering.
#[instrument(skip(im))]
pub fn image_to_gf(im: &DynamicImage, dithering: Option<DitheringType>) -> String {
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

    // Number of bytes that each line uses.
    let line_size = (width_padded / 8) as usize;

    // The image data, after compression.
    let mut field_data = String::new();

    // Used to keep track of the value from the last line so we can compress it
    // when they are the same.
    let mut last_line = String::new();

    // A buffer for holding bytes for the current line.
    let mut buf = Vec::with_capacity(line_size);
    // A work in progress byte that gets updated with each pixel.
    let mut byte = 0u8;

    for y in 0..height {
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

        // Now that we're done with the line, we can encode it into a hex
        // representation and compress that value.
        let hex_buf = hex::encode_upper(&buf);
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

    let total_size = line_size * height as usize;
    debug!(
        total_size,
        compressed_size = field_data.len(),
        "got total bytes"
    );

    format!("^GFA,{total_size},{total_size},{line_size},{field_data}")
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

pub struct ZplParser {
    caret: char,
    tilde: char,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ZplCommand<'a> {
    pub start: char,
    pub name: &'a str,
    pub args: &'a str,
}

#[derive(Clone, Debug)]
pub struct ZplHostVerification<'a> {
    pub length: usize,
    pub prefix: Option<&'a str>,
    pub suffix: Option<&'a str>,
    pub applies_to: Option<char>,
}

impl<'a> ZplHostVerification<'a> {
    pub fn from_commands(commands: &[ZplCommand<'a>]) -> HashMap<u16, ZplHostVerification<'a>> {
        commands
            .iter()
            .filter(|command| command.start == '^' && command.name == "HV")
            .filter_map(|hv| {
                let mut args = hv.args.splitn(5, ',');
                let field_no = args.next().unwrap_or("0").parse().ok()?;
                let length = args.next().unwrap_or("64").parse().ok()?;
                let prefix = args.next();
                let suffix = args.next();
                let applies_to = args.next().and_then(|applies_to| applies_to.chars().next());

                Some((
                    field_no,
                    ZplHostVerification {
                        length,
                        prefix,
                        suffix,
                        applies_to,
                    },
                ))
            })
            .collect()
    }
}

impl Default for ZplParser {
    fn default() -> Self {
        Self {
            caret: '^',
            tilde: '~',
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

    #[tracing::instrument(skip(self))]
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
        tracing::trace!(%start, name, "identified command");
        let normalized_command_start = if start == self.caret { '^' } else { '~' };

        let (input, args) = if start == self.caret && name.eq_ignore_ascii_case("CC") {
            let (input, arg) = take(1usize)(input)?;
            tracing::trace!("changing caret symbol to {arg}");
            self.caret = arg.chars().next().unwrap();
            (input, arg)
        } else if start == self.caret && name.eq_ignore_ascii_case("CT") {
            let (input, arg) = take(1usize)(input)?;
            tracing::trace!("changing tilde symbol to {arg}");
            self.tilde = arg.chars().next().unwrap();
            (input, arg)
        } else {
            let (input, args) = opt(is_not([self.caret, self.tilde].as_slice())).parse(input)?;
            let args = args.unwrap_or_default();
            tracing::trace!(args, "got command arguments");
            (input, args)
        };

        Ok((
            input,
            ZplCommand {
                start: normalized_command_start,
                name,
                args: args.trim(),
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::zpl::{ZplCommand, ZplParser};

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
}
