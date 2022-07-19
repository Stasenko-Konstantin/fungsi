use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::string::String;
use crate::object::ObjectType::*;
use crate::evaluator::eval_obj;
use crate::evaluator::make_void;

pub enum ObjectType {
    Void,
    Num,
    Name,
    String,
    Lambda,
    List,
    Builtin,
}

pub struct Object {
    pub(crate) object_type: ObjectType,
    pub(crate) span: (i64, i64),
    pub(crate) content: RefCell<Box<dyn Any>>,
}

impl Object {
    fn get_content(&self) -> String {
        todo!()
    }
}

pub struct Env<'a> {
    pub(crate) parent: Option<&'a Env<'a>>,
    pub(crate) defs: HashMap<&'a str, RefCell<Object>>,
}

pub fn make_builtins() -> HashMap<&'static str, RefCell<Object>> {
    let mut defs = HashMap::new();
    let print = RefCell::new(Object {
        object_type: Builtin,
        span: (0, 0),
        content: RefCell::new(Box::new(|obj: &'static Object, env: &mut Env| -> RefCell<Object> {
            print!("{}", eval_obj(obj, env).get_content());
            make_void(obj)
        })),
    });
    defs.insert("print", print);
    defs
}