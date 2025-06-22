use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap, HashSet},
};

use image::{DynamicImage, GenericImage, GenericImageView, imageops::FilterType::Lanczos3};
use tera::{
    Context, Tera, Value,
    ast::{Expr, ExprVal, FunctionCall, In, LogicExpr, MacroCall, MathExpr, Node},
};

use crate::zpl;

pub fn get_tera() -> Tera {
    let mut tera = Tera::default();

    tera.autoescape_on(vec![".zpl"]);
    tera.set_escape_fn(escape_fn);

    tera.register_filter("hex", hex_filter);
    tera.register_filter("tohex", hex_filter);

    tera.register_filter("image", ImageFilter);

    tera
}

fn arr_as_byte_vec(arr: &[tera::Value]) -> Result<Vec<u8>, &'static str> {
    arr.iter()
        .map(|val| match val {
            tera::Value::Number(num) => num
                .as_u64()
                .and_then(|num| u8::try_from(num).ok())
                .ok_or("non-byte number value in array"),
            _ => Err("non-number type in array"),
        })
        .collect()
}

fn hex_filter(value: &Value, args: &HashMap<String, Value>) -> tera::Result<Value> {
    let upper = matches!(args.get("upper"), Some(Value::Bool(upper)) if *upper);

    match value {
        tera::Value::String(val) => Ok(Value::String(if upper {
            hex::encode_upper(val)
        } else {
            hex::encode(val)
        })),
        tera::Value::Array(arr) => match arr_as_byte_vec(arr) {
            Ok(bytes) => Ok(Value::String(if upper {
                hex::encode_upper(bytes)
            } else {
                hex::encode(bytes)
            })),
            Err(msg) => Err(tera::Error::msg(format!("could not encode to hex: {msg}"))),
        },
        _ => Err(tera::Error::msg(
            "can only encode strings or byte arrays to hex",
        )),
    }
}

struct ImageFilter;

impl tera::Filter for ImageFilter {
    fn filter(&self, value: &Value, args: &HashMap<String, Value>) -> tera::Result<Value> {
        let bytes = match value {
            tera::Value::String(val) => hex::decode(val)
                .map_err(|err| tera::Error::msg(format!("could not decode hex: {err}")))?,
            tera::Value::Array(arr) => match arr_as_byte_vec(arr) {
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

pub fn render_label(content: &str, context: &Context) -> tera::Result<String> {
    let mut tera = get_tera();
    tera.add_raw_template("label.zpl", content)?;

    tera.render("label.zpl", context)
}

fn escape_fn(input: &str) -> String {
    input
        .replace('_', "_5f")
        .replace('^', "_5e")
        .replace('º', "_f8")
}

#[derive(Default)]
pub struct VariableExtractor {
    variables: BTreeSet<String>,
    exclusions: HashSet<String>,
}

impl VariableExtractor {
    pub fn extract(content: &str) -> tera::Result<BTreeSet<String>> {
        let mut tera = get_tera();
        tera.add_raw_template("label.zpl", content)?;

        let mut extractor = VariableExtractor::default();

        for template in tera.templates.into_values() {
            extractor.expand_node(template.ast);
        }

        Ok(extractor.variables)
    }

    fn expand_node(&mut self, nodes: impl IntoIterator<Item = Node>) {
        for node in nodes {
            match node {
                Node::VariableBlock(_, expr) => self.expand_expr(expr),
                Node::MacroDefinition(_, block, _) => self.expand_node(block.body),
                Node::Set(_, set) => {
                    let _span = tracing::trace_span!("set node", key = set.key).entered();

                    if self.variables.contains(&set.key) {
                        tracing::trace!("variable was already known");
                    } else {
                        tracing::trace!("variables did not contain key, adding to exclusions");
                        self.exclusions.insert(set.key);
                    }

                    self.expand_expr(set.value);
                }
                Node::FilterSection(_, block, _) => self.expand_node(block.body),
                Node::Forloop(_, block, _) => self.expand_node(block.body),
                Node::If(stmt, _) => stmt.conditions.into_iter().for_each(|(_, expr, nodes)| {
                    self.expand_expr(expr);
                    self.expand_node(nodes);
                }),
                Node::Block(_, block, _) => self.expand_node(block.body),
                _ => (),
            }
        }
    }

    fn expand_expr(&mut self, expr: Expr) {
        self.expand_expr_val(expr.val)
    }

    fn expand_expr_val(&mut self, expr_val: ExprVal) {
        match expr_val {
            ExprVal::Ident(name) => {
                let _span = tracing::trace_span!("expr ident", name).entered();

                if self.exclusions.contains(&name) {
                    tracing::trace!("name was in exclusions");
                } else {
                    tracing::trace!("name was not in exclusions");
                    self.variables.insert(name);
                }
            }
            ExprVal::Math(MathExpr { lhs, rhs, .. }) => {
                self.expand_expr(*lhs);
                self.expand_expr(*rhs);
            }
            ExprVal::Logic(LogicExpr { lhs, rhs, .. }) => {
                self.expand_expr(*lhs);
                self.expand_expr(*rhs);
            }
            ExprVal::MacroCall(MacroCall { args, .. }) => {
                args.into_values().for_each(|value| self.expand_expr(value))
            }
            ExprVal::FunctionCall(FunctionCall { args, .. }) => {
                args.into_values().for_each(|value| self.expand_expr(value))
            }
            ExprVal::Array(exprs) => exprs.into_iter().for_each(|value| self.expand_expr(value)),
            ExprVal::StringConcat(concat) => concat
                .values
                .into_iter()
                .for_each(|value| self.expand_expr_val(value)),
            ExprVal::In(In { lhs, rhs, .. }) => {
                self.expand_expr(*lhs);
                self.expand_expr(*rhs);
            }
            _ => (),
        }
    }
}
