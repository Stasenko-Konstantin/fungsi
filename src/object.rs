use std::any::Any;
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use std::env::Args;
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
    pub(crate) content: Box<dyn Any>,
}

impl Object {
    fn get_content(&self) -> String {
        todo!()
    }
}

pub struct Env<'a> {
    parent: &'a Env<'a>,
    defs: HashMap<&'a str, &'a Object>
}

fn make_builtins(defs: &mut HashMap<& str, & Object>) {
    let print = &Object{
        object_type: Builtin, span: (0, 0),
        content: Box::new(move |obj: &Object, env: &Env| -> &Object {
            print!("{}", eval_obj(obj, env).get_content());
            return make_void(obj);
        })};
    defs.insert("print", print);
}