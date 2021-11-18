
use std::collections::BTreeMap;    

use itertools::join;
use std::sync::Arc;



#[derive(Debug,Clone,PartialEq)]
pub enum Value {
    Text(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Array(Vec<String>),
    Null,
    Geometry,
}

impl Value {
    pub fn sql(&self) -> String {
        match self {
            Value::Text(s) => format!("'{}'", s),
            Value::Integer(i) => format!("{}", i),
            Value::Float(f) => format!("{}", f),
            Value::Bool(b) => String::from(if *b { "true"} else {"false"}),
            Value::Array(a) => format!("[{}]", join(a.iter().map(|s| format!("'{}'",s)),", ")),
            Value::Null => String::from("NULL"),
            Value::Geometry => String::from("Geometry()"),
        }
    }
}



pub trait Rower {
    fn pick(&self, col: &str) -> Value;
}



#[derive(Debug,Clone)]
pub enum Row<T: Rower> {
    Original(Arc<T>),
    Extended(ExtendedRow<T>)
}

impl<T> Row<T> where T: Rower  {
    pub fn pick(&self, col: &str) -> Value {
        match &self {
            Row::Original(p) => p.pick(col),
            Row::Extended(p) => p.pick(col)
        }
    }
    
    pub fn original_row<'a>(&'a self) -> &'a T {
        match &self {
            Row::Original(p) => p,
            Row::Extended(p) => &p.original
        }
    }
    
    pub fn extend(&self) -> ExtendedRow<T> {
        match &self {
            Row::Original(p) => ExtendedRow{original: p.clone(), fields: BTreeMap::new()},
            Row::Extended(e) => ExtendedRow{original: e.original.clone(), fields: BTreeMap::new()},
        }
    }
}


#[derive(Debug,Clone)]
pub struct ExtendedRow<T: Rower> {
    pub original: Arc<T>,
    pub fields: BTreeMap<String, Value>
}

impl<T: Rower> Rower for ExtendedRow<T> {
    fn pick(&self, col: &str) -> Value {
        match self.fields.get(col) {
            Some(v) => v.clone(),
            None => Value::Null
        }
    }
}


impl<T: Rower> ExtendedRow<T> {
    /*pub fn pick(&self, col: &str) -> Value {
        match self.fields.get(col) {
            Some(v) => { return v.clone(); },
            None => {}
        }
        
        self.orig_geometry.pick(col)
    }*/
    
    pub fn add(&mut self, col: String, val: Value) {
        self.fields.insert(col, val);
    }
}
        
