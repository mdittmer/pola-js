use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use swc_atoms::JsWord;
use swc_common::collections::{AHashMap, AHashSet};
use swc_common::Span;
use swc_ecma_ast::Id;

pub enum Eval {
    Unchanged,
    Changed(Value),
}

// TODO: Plumb unique module IDs when doing anlysis over multiple modules.
#[derive(Debug, Default, Eq, Hash, PartialEq)]
pub struct Module(u64);

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Source {
    module: Module,
    span: Span,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct ObjectValue {
    source: Source,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Object {
    FirstReference(ObjectValue),
    AllocationSite(ObjectValue),
}

impl Object {
    pub fn eval(&self) -> Eval {
        Eval::Unchanged
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct ConstantValue<V: Debug + Eq + Hash + PartialEq, X: Debug + Eq + Hash + PartialEq> {
    value: V,
    extra: X,
}

impl ConstantValue<JsWord, ()> {
    pub fn str(value: JsWord) -> Self {
        Self { value, extra: () }
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Constant {
    String(ConstantValue<JsWord, ()>),
}

impl Constant {
    pub fn str(value: JsWord) -> Self {
        Self::String(ConstantValue::str(value))
    }
}

impl Constant {
    pub fn eval(&self) -> Eval {
        Eval::Unchanged
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum NonConstant {
    Object(Object),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum SingleValueData {
    NonConstant(NonConstant),
    Constant(Constant),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SingleValue(Rc<SingleValueData>);

impl SingleValue {
    pub fn eval(&self) -> Eval {
        Eval::Unchanged
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Single(SingleValue),
    Multiple(AHashSet<SingleValue>),
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Single(single_value) => {
                "Single".hash(state);
                single_value.hash(state);
            }
            Self::Multiple(multiple_values) => {
                "Multiple".hash(state);
                for single_value in multiple_values.iter() {
                    single_value.hash(state);
                }
            }
        }
    }
}

impl Value {
    pub fn eval(&self) -> Eval {
        match self {
            Self::Single(single_value) => single_value.eval(),
            Self::Multiple(multiple_values) => eval_multiple_values(multiple_values),
        }
    }
}

fn eval_multiple_values(multiple_values: &AHashSet<SingleValue>) -> Eval {
    let mut changes: AHashMap<&SingleValue, Value> = Default::default();
    for single_value in multiple_values.iter() {
        match single_value.eval() {
            Eval::Changed(new_value) => {
                changes.insert(single_value, new_value);
            }
            _ => {}
        }
    }
    if changes.len() == 0 {
        return Eval::Unchanged;
    }

    let mut new_values: AHashSet<SingleValue> = multiple_values
        .into_iter()
        .filter_map(|single_value| {
            if !changes.contains_key(single_value) {
                Some(single_value.clone())
            } else {
                None
            }
        })
        .collect();
    for (_, new_value) in changes.into_iter() {
        match new_value {
            Value::Single(single_value) => {
                new_values.insert(single_value);
            }
            Value::Multiple(multiple_values) => {
                new_values.extend(multiple_values.into_iter());
            }
        }
    }

    assert!(new_values.len() != 0);
    if new_values.len() == 1 {
        Eval::Changed(Value::Single(
            new_values
                .into_iter()
                .next()
                .expect("first element of len=1 collection"),
        ))
    } else {
        Eval::Changed(Value::Multiple(new_values))
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Variable(Id);

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum HeapEdge {
    ConstantName(ConstantValue<JsWord, ()>),
    MultiName(NonConstant),
    Any,
    Epsilon,
}
