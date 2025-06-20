use std::collections::{BTreeSet, HashMap, HashSet};

use image::imageops::FilterType::Lanczos3;
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

        let height = args.get("height").and_then(|val| val.as_u64());
        let width = args.get("width").and_then(|val| val.as_u64());

        let im = match (height, width) {
            (Some(height), Some(width)) => im.resize_exact(width as u32, height as u32, Lanczos3),
            (Some(height), _) => im.resize(im.width(), height as u32, Lanczos3),
            (_, Some(width)) => im.resize(width as u32, im.height(), Lanczos3),
            _ => im,
        };

        Ok(tera::Value::String(zpl::image_to_gf(&im, dithering)))
    }

    fn is_safe(&self) -> bool {
        true
    }
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
