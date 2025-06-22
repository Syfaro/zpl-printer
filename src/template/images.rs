use std::{borrow::Cow, collections::HashMap};

use image::{DynamicImage, GenericImage, GenericImageView, imageops::FilterType::Lanczos3};
use tera::Value;

use crate::zpl;

pub struct ImageFilter;

impl tera::Filter for ImageFilter {
    fn filter(&self, value: &Value, args: &HashMap<String, Value>) -> tera::Result<Value> {
        let bytes = match value {
            tera::Value::String(val) => hex::decode(val)
                .map_err(|err| tera::Error::msg(format!("could not decode hex: {err}")))?,
            tera::Value::Array(arr) => match super::arr_as_byte_vec(arr) {
                Ok(bytes) => bytes,
                Err(msg) => {
                    return Err(tera::Error::msg(format!(
                        "could not decode value as bytes: {msg}"
                    )));
                }
            },
            _ => return Err(tera::Error::msg("cannot convert value to image")),
        };

        let im = image::load_from_memory(&bytes)
            .map_err(|err| tera::Error::msg(format!("data was not valid image: {err}")))?;

        let dithering: Option<zpl::DitheringType> = args
            .get("dithering")
            .and_then(|val| tera::from_value(val.clone()).ok());

        let exact = args
            .get("exact")
            .and_then(|val| val.as_bool())
            .unwrap_or_default();
        let center = args
            .get("center")
            .and_then(|val| val.as_bool())
            .unwrap_or(true);
        let expand = args
            .get("expand")
            .and_then(|val| val.as_bool())
            .unwrap_or(true);
        let height = args
            .get("height")
            .and_then(|val| val.as_u64())
            .and_then(|val| val.try_into().ok());
        let width = args
            .get("width")
            .and_then(|val| val.as_u64())
            .and_then(|val| val.try_into().ok());

        let (im_width, im_height) = im.dimensions();

        let im = match (height, width) {
            (Some(height), Some(width)) => {
                if (im_width > width || im_height > height) || expand {
                    if exact {
                        im.resize_exact(width, height, Lanczos3)
                    } else {
                        im.resize(width, height, Lanczos3)
                    }
                } else {
                    im
                }
            }
            (Some(height), _) => {
                if im_height > height || expand {
                    im.resize(im.width(), height, Lanczos3)
                } else {
                    im
                }
            }
            (_, Some(width)) => {
                if im_width > width || expand {
                    im.resize(width, im.height(), Lanczos3)
                } else {
                    im
                }
            }
            _ => im,
        };

        let im = if center {
            place_image(&im, height, width)
        } else {
            Cow::Borrowed(&im)
        };

        Ok(tera::Value::String(zpl::image_to_gf(&im, dithering)))
    }

    fn is_safe(&self) -> bool {
        true
    }
}

#[tracing::instrument(skip(im))]
fn place_image<'a>(
    im: &'a DynamicImage,
    height: Option<u32>,
    width: Option<u32>,
) -> Cow<'a, DynamicImage> {
    let (im_width, im_height) = im.dimensions();
    tracing::trace!(
        width = im_width,
        height = im_height,
        "got resized image dimensions"
    );

    let (height, width, placement_x, placement_y) = match (height, width) {
        (Some(height), Some(width)) => {
            if im_width == width && im_height == height {
                tracing::trace!(
                    "requested height and width exactly matched resized height and width"
                );
                return Cow::Borrowed(im);
            }

            (
                height,
                width,
                (width - im_width) / 2,
                (height - im_height) / 2,
            )
        }
        (Some(height), _) => {
            if im_height == height {
                tracing::trace!("requested height exactly matched resized height");
                return Cow::Borrowed(im);
            }

            (height, im_width, 0, (height - im_height) / 2)
        }
        (_, Some(width)) => {
            if im_width == width {
                tracing::trace!("requested width exactly matched resized width");
                return Cow::Borrowed(im);
            }

            (im_height, width, (width - im_width) / 2, 0)
        }
        (None, None) => {
            tracing::trace!("no requested height and width");
            return Cow::Borrowed(im);
        }
    };

    tracing::trace!(
        height,
        width,
        placement_x,
        placement_y,
        "calculated new size and placements"
    );

    let mut new_im = image::ImageBuffer::from_pixel(width, height, image::Rgba([255, 255, 255, 0]));

    new_im
        .copy_from(im, placement_x, placement_y)
        .expect("copied image should always fit");

    Cow::Owned(DynamicImage::from(new_im))
}
