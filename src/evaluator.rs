use std::cell::RefCell;
use crate::object::{Env, Object};
use crate::object::ObjectType::Void;

pub fn eval_obj<'a, T>(obj: &'a Object<T>, env: &mut Env<T>) -> &'a Object<T> {
    return obj;
}

pub fn make_void<T>(obj: &'static Object<T>) -> RefCell<Object<T>> {
    RefCell::new(Object { object_type: Void, content: RefCell::new(Box::new(obj)), span: obj.span })
}