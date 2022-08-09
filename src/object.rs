use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::string::String;
use crate::object::ObjectType::*;
use crate::evaluator::eval_obj;
use crate::evaluator::make_void;

pub enum ObjectType<T> {
    Void,
    Num,
    Name,
    String,
    Lambda,
    List,
    Builtin,
    Custom(T),
}

pub struct Object<T> {
    pub(crate) object_type: ObjectType<T>,
    pub(crate) span: (i64, i64),
    pub(crate) content: RefCell<Box<dyn Any>>,
}

impl<T> Object<T> {
    pub fn get_content(&self) -> String {
        todo!()
    }
}

pub struct Env<'a, T> {
    pub(crate) parent: Option<&'a Env<'a, T>>,
    pub(crate) defs: HashMap<&'a str, RefCell<Object<T>>>,
}

pub fn make_builtins<T>() -> HashMap<&'static str, RefCell<Object<T>>> {
    let mut defs = HashMap::new();
    let print = RefCell::new(Object {
        object_type: Builtin,
        span: (0, 0),
        content: RefCell::new(
            Box::new(|obj: &'static Object<T>, env: &mut Env<T>| -> RefCell<Object<T>> {
                print!("{}", eval_obj(obj, env).get_content());
                make_void(obj)
            })),
    });
    defs.insert("print", print);
    defs
}