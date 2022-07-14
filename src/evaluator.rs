use crate::object::{Env, Object};
use crate::object::ObjectType::Void;

pub fn eval_obj<'a>(obj: &'a Object, env: &Env) -> &'a Object {
    return obj
}

pub fn make_void<'a>(obj: &Object) -> &'static Object {
    return &Object{object_type: Void, content: Box::new(obj), span: obj.span }
}