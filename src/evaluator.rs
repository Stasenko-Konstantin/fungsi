use std::cell::RefCell;
use crate::object::{Env, Object};
use crate::object::ObjectType::Void;

pub fn eval_obj<'a>(obj: &'a Object, env: &mut Env) -> &'a Object {
    return obj
}

pub fn make_void(obj: &'static Object) -> RefCell<Object> {
    RefCell::new(Object{object_type: Void, content: RefCell::new(Box::new(obj)), span: obj.span })
}