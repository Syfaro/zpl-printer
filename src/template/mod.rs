use std::collections::{BTreeSet, HashMap, HashSet};

use tera::{
    Context, Tera, Value,
    ast::{Expr, ExprVal, FunctionCall, In, LogicExpr, MacroCall, MathExpr, Node},
};

mod images;

pub fn get_tera() -> Tera {
    let mut tera = Tera::default();

    tera.autoescape_on(vec![".zpl"]);
    tera.set_escape_fn(escape_fn);

    tera.register_filter("hex", hex_filter);
    tera.register_filter("tohex", hex_filter);

    tera.register_filter("image", images::ImageFilter);

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
    const LOOP_VARIABLES: [&str; 4] = ["loop.index", "loop.index0", "loop.first", "loop.last"];

    pub fn extract(content: &str) -> tera::Result<BTreeSet<String>> {
        let mut tera = get_tera();
        tera.add_raw_template("label.zpl", content)?;

        let mut extractor = VariableExtractor::default();

        for template in tera.templates.into_values() {
            extractor.expand_node(template.ast, false);
        }

        Ok(extractor.variables)
    }

    fn expand_node(&mut self, nodes: impl IntoIterator<Item = Node>, in_loop: bool) {
        for node in nodes {
            match node {
                Node::VariableBlock(_, expr) => self.expand_expr(expr, in_loop),
                Node::MacroDefinition(_, block, _) => self.expand_node(block.body, in_loop),
                Node::Set(_, set) => {
                    let _span = tracing::trace_span!("set node", key = set.key).entered();

                    self.add_variable(set.key, in_loop);
                    self.expand_expr(set.value, in_loop);
                }
                Node::FilterSection(_, block, _) => self.expand_node(block.body, in_loop),
                Node::Forloop(_, block, _) => {
                    self.add_variable(block.value, true);
                    if let Some(key) = block.key {
                        self.add_variable(key, true);
                    }

                    self.expand_node(block.body, true);
                }
                Node::If(stmt, _) => stmt.conditions.into_iter().for_each(|(_, expr, nodes)| {
                    self.expand_expr(expr, in_loop);
                    self.expand_node(nodes, in_loop);
                }),
                Node::Block(_, block, _) => self.expand_node(block.body, in_loop),
                _ => (),
            }
        }
    }

    #[tracing::instrument(skip(self))]
    fn add_variable(&mut self, name: String, in_loop: bool) {
        if self.variables.contains(&name) {
            tracing::trace!("variable was already known");
        } else {
            tracing::trace!("variables did not contain key, adding to exclusions");
            self.exclusions.insert(name);
        }
    }

    fn expand_expr(&mut self, expr: Expr, in_loop: bool) {
        self.expand_expr_val(expr.val, in_loop)
    }

    fn expand_expr_val(&mut self, expr_val: ExprVal, in_loop: bool) {
        match expr_val {
            ExprVal::Ident(name) => {
                let _span = tracing::trace_span!("expr ident", name).entered();

                if in_loop && Self::LOOP_VARIABLES.contains(&name.as_str()) {
                    tracing::trace!("in loop and variable was loop.");
                    return;
                }

                if self.exclusions.contains(&name) {
                    tracing::trace!("name was in exclusions");
                } else {
                    tracing::trace!("name was not in exclusions");
                    self.variables.insert(name);
                }
            }
            ExprVal::Math(MathExpr { lhs, rhs, .. }) => {
                self.expand_expr(*lhs, in_loop);
                self.expand_expr(*rhs, in_loop);
            }
            ExprVal::Logic(LogicExpr { lhs, rhs, .. }) => {
                self.expand_expr(*lhs, in_loop);
                self.expand_expr(*rhs, in_loop);
            }
            ExprVal::MacroCall(MacroCall { args, .. }) => args
                .into_values()
                .for_each(|value| self.expand_expr(value, in_loop)),
            ExprVal::FunctionCall(FunctionCall { args, .. }) => args
                .into_values()
                .for_each(|value| self.expand_expr(value, in_loop)),
            ExprVal::Array(exprs) => exprs
                .into_iter()
                .for_each(|value| self.expand_expr(value, in_loop)),
            ExprVal::StringConcat(concat) => concat
                .values
                .into_iter()
                .for_each(|value| self.expand_expr_val(value, in_loop)),
            ExprVal::In(In { lhs, rhs, .. }) => {
                self.expand_expr(*lhs, in_loop);
                self.expand_expr(*rhs, in_loop);
            }
            _ => (),
        }
    }
}
