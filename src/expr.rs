
use crate::{Row,Value,Rower};
use itertools::join;
use regex::Regex;

#[derive(Debug)]
pub enum DataType {
    Null,Text,Integer,Float,Bool
}

#[derive(Debug)]
pub enum Operator {
    NumericalOperator(NumericalOperator),
    LogicalOperator(LogicalOperator),
    SpecialOperator(SpecialOperator)
}

#[derive(Debug)]
pub enum NumericalOperator {
    Plus, Minus, Multiply, Divide, Power, Modulo
}

#[derive(Debug)]
pub enum LogicalOperator {
    And, Or, Eq, NotEq, Gt, Ge, Lt, Le
}

#[derive(Debug)]
pub enum SpecialOperator {
    HStoreAccess, HStoreContainsKey, HStoreContainsKeyValue, HStoreContainsKeyValueReverse, GeometryOverlaps, StringConcat, RegExpMatch
}

#[derive(Debug)]
pub enum UnaryOperator {
    Not
}


#[derive(Debug)]
pub enum Expr {
    Pick{name: String},
    Alias{expr: Box<Expr>, alias: String},
    Case{when_clauses: Vec<(Expr,Expr)>, else_clause: Option<Box<Expr>>},
    Function{function: String, arguments: Vec<Expr>},
    BinaryOperation{left: Box<Expr>, operator: Operator, right: Box<Expr>},
    UnaryOperation{expr: Box<Expr>, operator: UnaryOperator},
    IsNull{expr: Box<Expr>},
    NotIsNull{expr: Box<Expr>},
    InList{expr: Box<Expr>, expr_list: Vec<Expr>, negated:bool},
    Cast{expr: Box<Expr>, data_type: DataType},
    Substring{expr: Box<Expr>, from_expr: Option<Box<Expr>>, for_expr: Option<Box<Expr>>},
    Value{value: Value},
    Between{expr: Box<Expr>, negated: bool, low: Box<Expr>, high: Box<Expr>}
}


impl Expr {
    pub fn is_condition(&self) -> bool {
        match self {
            Expr::Pick{..} => false,
            Expr::Alias{..} => false,
            Expr::Case{..} => false,
            Expr::Function{ref function,..} => is_condition_function(&function),
            Expr::BinaryOperation{ref operator, ..} => is_condition_operator(&operator),
            Expr::UnaryOperation{ref operator, ..} => is_condition_unary_operator(&operator),
            Expr::IsNull{..} => true,
            Expr::NotIsNull{..} => true,
            Expr::InList{..} => true,
            Expr::Cast{..} => false,
            Expr::Substring{..} => false,
            Expr::Value{..} => false,
            Expr::Between{..} => true,
        }
    }
    
    pub fn field_name(&self) -> Option<String> {
        match self {
            Expr::Pick{name} => Some(name.clone()),
            Expr::Alias{alias, ..} => Some(alias.clone()),
            _ => None
        }
    }
    
    pub fn execute<T: Rower>(&self, row: &Row<T>, zoom: f64) -> Value {
        match &self {
            Expr::Pick{name} => row.pick(name),
            Expr::Alias{expr, ..} => expr.execute(row, zoom),
            Expr::Case{when_clauses, else_clause} => case_execute(when_clauses, else_clause, row, zoom),
            Expr::Function{function, arguments} => function_execute(function, arguments, row, zoom),
            Expr::BinaryOperation{left, operator, right} => binary_operation_execute(left, operator, right, row, zoom),
            Expr::UnaryOperation{expr, operator} => unary_operation_execute(expr, operator, row, zoom),
            Expr::IsNull{expr} => match expr.execute(row,zoom) {Value::Null => Value::Bool(true), _ => Value::Bool(false) },
            Expr::NotIsNull{expr} => match expr.execute(row,zoom) {Value::Null => Value::Bool(false), _ => Value::Bool(true) },
            Expr::InList{expr,expr_list,negated} => match in_list_execute(expr,expr_list,*negated,row,zoom) {None=> Value::Null, Some(b) => Value::Bool(b) },
            Expr::Cast{expr, data_type} => cast_execute(expr, data_type, row, zoom),
            Expr::Substring{expr, from_expr, for_expr} => substring_execute(expr, from_expr, for_expr, row, zoom),
            Expr::Value{value} => value.clone(),
            Expr::Between{expr,negated,low,high} => match between_execute(expr,*negated,low,high, row, zoom) {None=> Value::Null, Some(b) => Value::Bool(b) },
        }
    }
    
    pub fn condition_execute<T: Rower>(&self, row: &Row<T>, zoom: f64) -> Option<bool> {
        match &self {
            Expr::Function{function, arguments} => function_condition_execute(function, arguments, row, zoom),
            Expr::BinaryOperation{left, operator, right} => binary_operation_condition_execute(left, operator, right, row, zoom),
            Expr::UnaryOperation{expr, operator} => unary_operation_condition_execute(expr, operator, row, zoom),
            Expr::IsNull{expr} => match expr.execute(row,zoom) {Value::Null => Some(true), _ => Some(false) },
            Expr::NotIsNull{expr} => match expr.execute(row,zoom) {Value::Null => Some(false), _ => Some(true) },
            Expr::InList{expr,expr_list,negated} => in_list_execute(expr,expr_list,*negated,row,zoom),
            Expr::Between{expr,negated,low,high} => between_execute(expr,*negated,low,high, row, zoom),
            _ => None
        }
    }
    
    pub fn sql(&self) -> String {
        format!("?? {:?}", self)
    }
}









fn is_condition_function(function_name: &str) -> bool {
    match function_name {
        "geometry_in_box" => true,
        _ => false
    }
}

fn is_condition_operator(operator: &Operator) -> bool {
    match operator {
        Operator::NumericalOperator(_) => false,
        Operator::LogicalOperator(_) => true,
        Operator::SpecialOperator(s) => {
            match s {
                SpecialOperator::HStoreContainsKey | SpecialOperator::HStoreContainsKeyValue => true,
                SpecialOperator::HStoreContainsKeyValueReverse => true,
                SpecialOperator::RegExpMatch | SpecialOperator::GeometryOverlaps => true,
                _ => false
            }
        }
    }
}

fn is_condition_unary_operator(operator: &UnaryOperator) -> bool {
    match operator {
        UnaryOperator::Not => true
    }
}

fn case_execute<T: Rower>(when_clauses: &Vec<(Expr,Expr)>, else_clause: &Option<Box<Expr>>, row: &Row<T>, zoom: f64) -> Value {
    for (when,then) in when_clauses {
        match when.condition_execute(row,zoom) {
            None => {}, // return Value::Null; }
            Some(true) => {return then.execute(row,zoom); },
            Some(false) => {},
        }
    }
    match else_clause {
        Some(expr) => expr.execute(row,zoom),
        None => Value::Null
    }
}

fn cast_execute<T: Rower>(expr: &Expr, data_type: &DataType, row: &Row<T>, zoom: f64) -> Value {
    let val = expr.execute(row, zoom);
    match val {
        Value::Null | Value::Array(_) | Value::Geometry => Value::Null,
        Value::Integer(i) => {
            match data_type {
                DataType::Text => Value::Text(format!("{}", i)),
                DataType::Float => Value::Float(i as f64),
                DataType::Integer => Value::Integer(i),
                DataType::Bool => Value::Bool(i!=0),
                DataType::Null => Value::Null,
            }
        },
        Value::Float(f) => {
            match data_type {
                DataType::Text => Value::Text(format!("{}", f)),
                DataType::Float => Value::Float(f),
                DataType::Integer => Value::Integer(f as i64),
                DataType::Bool => Value::Bool(f!=0.0),
                DataType::Null => Value::Null,
            }
        },
        Value::Bool(b) => {
            match data_type {
                DataType::Text => Value::Text(String::from(if b { "t" } else { "f" })),
                DataType::Float => Value::Float(if b { 1.0 } else {0.0 }),
                DataType::Integer => Value::Integer(if b { 1 } else {0 }),
                DataType::Bool => Value::Bool(b),
                DataType::Null => Value::Null,
            }
        },
        Value::Text(t) => {
            match data_type {
                DataType::Text => Value::Text(t),
                DataType::Float => {
                    match t.parse::<f64>() {
                        Ok(f) => Value::Float(f),
                        _ => Value::Null
                    }
                },
                DataType::Integer => {
                    match t.parse::<i64>() {
                        Ok(i) => Value::Integer(i),
                        _ => Value::Null
                    }
                },
                
                DataType::Bool => Value::Bool( match t.as_str() { "t" | "T" | "true" | "TRUE" => true, _ => false}),
                DataType::Null => Value::Null,
            }
        }
    }
}
    


pub fn check_function(function_name: &str, num_fields: usize) -> bool {
    match function_name {
        "pixel_area" => num_fields==0,
        "scale_denominator" => num_fields==0,
        "nullif" => num_fields==2,
        "pow" => num_fields==2,
        "coalesce" => num_fields>=1,
        "length" => num_fields==1,
        "char_length" => num_fields==1,
        "array_length" => num_fields==2,
        "substr" => num_fields==3,
        "geometry_in_box"=> num_fields==0 || num_fields==1,
        "max_array_length" => num_fields == 1,
        "array_to_string" => num_fields == 2,
        "string_to_array" => num_fields == 2,
        "round" => num_fields == 1 || num_fields == 2,
        "concat" => num_fields>0,
        "left_trim_from" => num_fields==2,
        "right_trim_from" => num_fields==2,
        "both_trim_from" => num_fields==2,
        "trim" => num_fields==1,
        "ltrim" => num_fields==1,
        "rtrim" => num_fields==1,
        
        _ => false
    }
}

fn pixel_size(z: f64) -> f64 {    
    2.0*20037508.342789244 / 256.0 / 2.0_f64.powf(z)
}


fn function_execute<T: Rower>(function_name: &str, fields: &Vec<Expr>, row: &Row<T>, zoom: f64) -> Value {
    
    match function_name {
        
        "pixel_area" => Value::Float(pixel_size(zoom).powf(2.0)),
        "scale_denominator" => Value::Float(pixel_size(zoom) / 0.00028),
        "nullif" => {
            let v0 = fields[0].execute(row,zoom);
            let v1 = fields[1].execute(row,zoom);
            match logical_binary_operation_condition_execute(v0.clone(),&LogicalOperator::Eq,v1) {
                Some(true) | None => Value::Null,
                Some(false) => v0
            }
        },
        "pow" => {
            let v0 = fields[0].execute(row,zoom);
            let v1 = fields[1].execute(row,zoom);
            numerical_binary_operation_execute(v0,&NumericalOperator::Power,v1)
        },
        "coalesce" => {
            for f in fields {
                let v = f.execute(row,zoom);
                match v {
                    Value::Null => {},
                    vi => { return vi; }
                }
            }
            Value::Null
        },
        "char_length" | "length" => {
            let v0 = fields[0].execute(row,zoom);
            match v0 {
                Value::Text(t) => Value::Integer(t.len() as i64),
                _ => Value::Null
            }
        },
        "array_length" => {
            let v0 = fields[0].execute(row,zoom);
            let v1 = fields[1].execute(row,zoom);
            if v1 != Value::Integer(1) {
                return Value::Null;
            }
            match v0 {
                Value::Array(t) => Value::Integer(t.len() as i64),
                _ => Value::Null
            }
        },
        "substr" => {
            let v0 = fields[0].execute(row,zoom);
            let v1 = fields[1].execute(row,zoom);
            let v2 = fields[2].execute(row,zoom);
            match (v0,v1,v2) {
                (Value::Text(s),Value::Integer(f),Value::Integer(t)) => {
                    if f as usize >= s.len() || (f+t-1) as usize > s.len() {
                        Value::Null
                    } else {
                        Value::Text(s[(f-(if f==0 { 0 } else {1})) as usize .. (f+t-1) as usize].to_string())
                    }
                },
                _ => Value::Null
            }
        },
        "substring_regex" => {
            let v0 = fields[0].execute(row,zoom);
            let v1 = fields[1].execute(row,zoom);
            
            match (v0,v1) {
                (Value::Text(l), Value::Text(r)) => {
                    match Regex::new(&r) {
                        Ok(m) => {
                            match m.find(&l) {
                                Some(c) => Value::Text(c.as_str().to_string()),
                                None => Value::Null,
                            }
                        },
                        Err(e) => {
                            println!("didn't understand regex {} {:?}", r, e);
                            Value::Null
                        }
                    }
                },
                _ => Value::Null
            }
        },  
        "geometry_in_box" => Value::Bool(true),
        "array_to_string" => {
            let v0 = fields[0].execute(row, zoom);
            let v1 = fields[1].execute(row, zoom);
                        
            match (v0,v1) {
                (Value::Array(aa), Value::Text(jj)) => Value::Text(join(aa.iter(), &jj)),
                _ => Value::Null
            }
        },
        "max_array_length" => {
            let v0 = fields[0].execute(row, zoom);
                        
            match v0 {
                Value::Array(aa) => Value::Integer( aa.iter().map(|a| a.len()).max().unwrap_or(0) as i64),
                _ => Value::Null
            }
        },
        "string_to_array" => {
            let v0 = fields[0].execute(row, zoom);
            let v1 = fields[1].execute(row, zoom);  
            match (v0,v1) {
                (Value::Text(text), Value::Text(sep)) => Value::Array( text.split(&sep).map(|a| a.to_string()).collect() ),
                _ => Value::Null
            }
        },
        "round" => {
            let v0 = fields[0].execute(row, zoom);
            let mut ndp = 0_f64;
            if fields.len() == 2 {
                let v1 = fields[1].execute(row, zoom);
                match v1 {
                    Value::Integer(n) => { ndp = n as f64; },
                    _ => {}
                }
            }
            
            match v0 {
                Value::Integer(i) => Value::Integer(i),
                Value::Float(f) => Value::Float(f64::round(f * 10.0_f64.powf(ndp)) / 10.0_f64.powf(ndp)),
                _ => Value::Null
            }
        },
        "concat" => {
            let mut res = String::new();
            for f in fields {
                let v = f.execute(row,zoom);
                match v {
                    Value::Text(t) => res.push_str(&t),
                    _ => {}
                }
            }
            Value::Text(res)
            /*if !res.is_empty() {
                Value::Text(res)
            } else {
                Value::Null
            }*/
        },
        
        "trim" => {
            match fields[0].execute(row, zoom) {
                Value::Text(t) => Value::Text(t.trim().to_string()),
                _ => Value::Null
            }
        },
        
        "left_trim_from" => {
            let t = fields[0].execute(row, zoom);
            let p = fields[1].execute(row, zoom);
            
            match (t,p) {
                (Value::Text(t), Value::Text(p)) => Value::Text(t.trim_start_matches(p.as_str()).to_string()),
                _ => Value::Null
            }
        },
        
        "right_trim_from" => {
            let t = fields[0].execute(row, zoom);
            let p = fields[1].execute(row, zoom);
            
            match (t,p) {
                (Value::Text(t), Value::Text(p)) => Value::Text(t.trim_end_matches(p.as_str()).to_string()),
                _ => Value::Null
            }
        },
        
        "both_trim_from" => {
            let t = fields[0].execute(row, zoom);
            let p = fields[1].execute(row, zoom);
            
            match (t,p) {
                (Value::Text(t), Value::Text(p)) => {
                    //Value::Text(t.trim_matches(p.as_str()).to_string()),
                    Value::Text(t.trim_start_matches(p.as_str()).trim_end_matches(p.as_str()).to_string())
                }
                _ => Value::Null
            }
        },
        
        "ltrim" => {
            match fields[0].execute(row, zoom) {
                Value::Text(t) => Value::Text(t.trim_start().to_string()),
                _ => Value::Null
            }
        },
        
        "rtrim" => {
            match fields[0].execute(row, zoom) {
                Value::Text(t) => Value::Text(t.trim_end().to_string()),
                _ => Value::Null
            }
        },
        
        
        _ =>  Value::Null
    }
}

fn function_condition_execute<T: Rower>(function_name: &str, _fields: &Vec<Expr>, _row: &Row<T>, _zoom: f64) -> Option<bool> {
    match function_name {
        "geometry_in_box" => Some(true),
        _ => None
    }
}


fn integer_numerical_binary_operation_execute(left: i64, num_op: &NumericalOperator, right: i64) -> Value {
    match num_op {
        NumericalOperator::Plus => Value::Integer(left+right),
        NumericalOperator::Minus => Value::Integer(left-right),
        NumericalOperator::Multiply => Value::Integer(left*right),
        NumericalOperator::Divide => Value::Integer(left/right),
        NumericalOperator::Power => Value::Float((left as f64).powf(right as f64)),
        NumericalOperator::Modulo => Value::Integer(left % right),
    }
}

fn float_numerical_binary_operation_execute(left: f64, num_op: &NumericalOperator, right: f64) -> Value {
    match num_op {
        NumericalOperator::Plus => Value::Float(left+right),
        NumericalOperator::Minus => Value::Float(left-right),
        NumericalOperator::Multiply => Value::Float(left*right),
        NumericalOperator::Divide => Value::Float(left/right),
        NumericalOperator::Power => Value::Float(left.powf(right)),
        NumericalOperator::Modulo => Value::Float(left % right),
    }
}
fn numerical_binary_operation_execute(left_value: Value, num_op: &NumericalOperator, right_value: Value) -> Value {
    match (left_value, right_value) {
        (Value::Integer(li), Value::Integer(ri)) => integer_numerical_binary_operation_execute(li, num_op, ri),
        (Value::Float(lf), Value::Integer(ri)) => float_numerical_binary_operation_execute(lf, num_op, ri as f64),
        (Value::Integer(li), Value::Float(rf)) => float_numerical_binary_operation_execute(li as f64, num_op, rf),
        (Value::Float(lf), Value::Float(rf)) => float_numerical_binary_operation_execute(lf, num_op, rf),
        _ => Value::Null
    }
}

fn integer_logical_binary_operation_condition_execute(left: i64, log_op: &LogicalOperator, right: i64) -> Option<bool> {
    match log_op {
        LogicalOperator::And | LogicalOperator::Or => None,
        LogicalOperator::Eq => Some(left==right),
        LogicalOperator::NotEq => Some(left!=right),
        LogicalOperator::Gt => Some(left > right),
        LogicalOperator::Ge => Some(left >= right),
        LogicalOperator::Lt => Some(left < right),
        LogicalOperator::Le => Some(left <= right)
    }
}
fn float_logical_binary_operation_condition_execute(left: f64, log_op: &LogicalOperator, right: f64) -> Option<bool> {
    match log_op {
        LogicalOperator::And | LogicalOperator::Or => None,
        LogicalOperator::Eq => Some(left==right),
        LogicalOperator::NotEq => Some(left != right),
        LogicalOperator::Gt => Some(left > right),
        LogicalOperator::Ge => Some(left >= right),
        LogicalOperator::Lt => Some(left < right),
        LogicalOperator::Le => Some(left <= right)
    }
}

fn string_logical_binary_operation_condition_execute(left: String, log_op: &LogicalOperator, right: String) -> Option<bool> {
    match log_op {
        LogicalOperator::And | LogicalOperator::Or => None,
        LogicalOperator::Eq => Some(left==right),
        LogicalOperator::NotEq => Some(left != right),
        LogicalOperator::Gt => Some(left > right),
        LogicalOperator::Ge => Some(left >= right),
        LogicalOperator::Lt => Some(left < right),
        LogicalOperator::Le => Some(left <= right)
    }
}
fn bool_logical_binary_operation_condition_execute(left: bool, log_op: &LogicalOperator, right: bool) -> Option<bool> {
    match log_op {
        LogicalOperator::And => Some(left && right),
        LogicalOperator::Or => Some(left || right),
        LogicalOperator::Eq => Some(left==right),
        LogicalOperator::NotEq => Some(left != right),
        LogicalOperator::Gt => Some(left > right),
        LogicalOperator::Ge => Some(left >= right),
        LogicalOperator::Lt => Some(left < right),
        LogicalOperator::Le => Some(left <= right),
    }
}

fn logical_binary_operation_condition_execute(left_value: Value, log_op: &LogicalOperator, right_value: Value) -> Option<bool> {
    match (left_value, right_value) {
        (Value::Null, Value::Null) => Some(false),
        (Value::Integer(l), Value::Integer(r)) => integer_logical_binary_operation_condition_execute(l, log_op, r),
        (Value::Float(l), Value::Float(r)) => float_logical_binary_operation_condition_execute(l, log_op, r),
        (Value::Text(l), Value::Text(r)) => string_logical_binary_operation_condition_execute(l, log_op, r),
        (Value::Bool(l), Value::Bool(r)) => bool_logical_binary_operation_condition_execute(l, log_op, r),
        (_, _) => Some(false)
    }
    
}

fn logical_binary_operation_execute(left_value: Value, log_op: &LogicalOperator, right_value: Value) -> Value {
    match logical_binary_operation_condition_execute(left_value, log_op, right_value) {
        Some(b) => Value::Bool(b),
        None => Value::Null
    }
}

fn special_binary_operation_execute<T: Rower>(left_value: Value, spec_op: &SpecialOperator, right_value: Value, row: &Row<T>) -> Value {
    match spec_op {
        SpecialOperator::GeometryOverlaps => Value::Bool(true),
        SpecialOperator::HStoreAccess => {
            match right_value {
                Value::Text(t) => row.pick(&t),
                _ => Value::Null
            }
        },      
        SpecialOperator::HStoreContainsKey => {
            match right_value {
                Value::Text(t) => Value::Bool(row.pick(&t)!=Value::Null),
                _ => Value::Null
            }
        },
        SpecialOperator::HStoreContainsKeyValue => {
            match right_value {
                Value::Text(t) => {
                    let kv = t.split("=>").collect::<Vec<&str>>();
                    if kv.len()==2 {
                        Value::Bool(row.pick(kv[0])==Value::Text(kv[1].to_string()))
                    } else {
                        Value::Null
                    }
                },
                _ => Value::Null
            }
        },
        SpecialOperator::HStoreContainsKeyValueReverse => {
            match left_value {
                Value::Text(t) => {
                    let kv = t.split("=>").collect::<Vec<&str>>();
                    if kv.len()==2 {
                        Value::Bool(row.pick(kv[0])==Value::Text(kv[1].to_string()))
                    } else {
                        Value::Null
                    }
                },
                _ => Value::Null
            }
        },
        SpecialOperator::RegExpMatch => {
            match (left_value, right_value) {
                (Value::Text(l),Value::Text(r)) => {
                    match Regex::new(&r) {
                        Ok(m) => Value::Bool(m.is_match(&l)),
                        Err(e) => {
                            println!("don't understand regex {}, {:?}", r, e);
                            Value::Null
                        }
                    }
                },
                _ => Value::Null,
            }
        },
        SpecialOperator::StringConcat => {
            match (left_value, right_value) {
                (Value::Text(lv), Value::Text(rv)) => Value::Text(lv+&rv),
                _ => Value::Null
            }
        }
    }
        
    
}

fn binary_operation_execute<T: Rower>(left: &Expr, operator: &Operator, right: &Expr, row: &Row<T>, zoom: f64) -> Value {
    
    let left_value = left.execute(row, zoom);
    let right_value = right.execute(row, zoom);
    
    match operator {
        Operator::NumericalOperator(num_op) => numerical_binary_operation_execute(left_value, num_op, right_value),
        Operator::LogicalOperator(log_op) => logical_binary_operation_execute(left_value, log_op, right_value),
        Operator::SpecialOperator(spec_op) => special_binary_operation_execute(left_value, spec_op, right_value, row),
    }
}

fn binary_operation_condition_execute<T: Rower>(left: &Expr, operator: &Operator, right: &Expr, row: &Row<T>, zoom: f64) -> Option<bool> {
    
    
    match operator {
        Operator::NumericalOperator(_) => None,
        Operator::LogicalOperator(log_op) => {
            match log_op {
                LogicalOperator::And => {
                    let l = left.condition_execute(row,zoom);
                    let r = right.condition_execute(row,zoom);
                    match (l,r) {
                        (Some(lt),Some(rt)) => Some(lt && rt),
                        _ => None
                    }
                },
                LogicalOperator::Or => {
                    let l = left.condition_execute(row,zoom);
                    let r = right.condition_execute(row,zoom);
                    match (l,r) {
                        (Some(lt),Some(rt)) => Some(lt || rt),
                        _ => None
                    }
                },
                _ => {
                
                    let left_value = left.execute(row, zoom);
                    let right_value = right.execute(row, zoom);
                    logical_binary_operation_condition_execute(left_value, log_op, right_value)
                }
            }
        },
        Operator::SpecialOperator(spec_op) => {
            match spec_op {
                SpecialOperator::GeometryOverlaps => Some(true),
                SpecialOperator::HStoreAccess => None,
                SpecialOperator::HStoreContainsKey => {
                    match right.execute(row,zoom) {
                        Value::Text(t) => Some(row.pick(&t)!=Value::Null),
                        _ => None
                    }
                },
                SpecialOperator::HStoreContainsKeyValue => {
                    match right.execute(row,zoom) {
                        Value::Text(t) => {
                            let kv = t.split("=>").collect::<Vec<&str>>();
                            if kv.len()==2 {
                                Some(row.pick(kv[0])==Value::Text(kv[1].to_string()))
                            } else {
                                None
                            } 
                            
                        },
                        _ => None
                    }
                },
                SpecialOperator::HStoreContainsKeyValueReverse => {
                    match left.execute(row,zoom) {
                        Value::Text(t) => {
                            let kv = t.split("=>").collect::<Vec<&str>>();
                            if kv.len()==2 {
                                Some(row.pick(kv[0])==Value::Text(kv[1].to_string()))
                            } else {
                                None
                            } 
                            
                        },
                        _ => None
                    }
                },
                SpecialOperator::RegExpMatch => {
                    let left_val = left.execute(row,zoom);
                    let right_val = right.execute(row,zoom);
                    match (left_val, right_val) {
                        (Value::Text(l),Value::Text(r)) => {
                            match Regex::new(&r) {
                                Ok(m) => Some(m.is_match(&l)),
                                Err(e) => {
                                    println!("don't understand regex {}, {:?}", r, e);
                                    None
                                }
                            }
                        },
                        _ => None,
                    }
                },
                SpecialOperator::StringConcat => None
            }
        }
    }
}

fn unary_operation_condition_execute<T: Rower>(expr: &Expr, operation: &UnaryOperator, row: &Row<T>, zoom: f64) -> Option<bool> {
    
    match operation {
        
        UnaryOperator::Not => {
            let value = expr.condition_execute(row, zoom);
            match value {
                Some(v) => Some(!v),
                None => None
            }
            
        },
        //_ => None
    }
}

fn unary_operation_execute<T: Rower>(expr: &Expr, operator: &UnaryOperator, row: &Row<T>, zoom: f64) -> Value {
    
    match operator {
        UnaryOperator::Not => {
            match expr.execute(row,zoom) {
                Value::Bool(b) => Value::Bool(!b),
                _ => Value::Null
            }
        },
            
        
        //_ => Value::Null
    }
}

fn in_list_execute<T: Rower>(expr: &Expr, expr_list: &Vec<Expr>, negated: bool, row: &Row<T>, zoom:f64) -> Option<bool> {
    let left_val = expr.execute(row,zoom);
    if left_val == Value::Null { return Some(negated); }
    
    for il in expr_list {
        let right_val = il.execute(row, zoom);
        match logical_binary_operation_condition_execute(left_val.clone(), &LogicalOperator::Eq, right_val) {
            Some(true) => { return Some(!negated); },
            Some(false) => {},
            None => {return None; }
            
        }
    }
    return Some(negated);
}

fn between_execute<T: Rower>(expr: &Expr,negated: bool,low: &Expr, high: &Expr, row: &Row<T>, zoom:f64) -> Option<bool> {
    let val = expr.execute(row, zoom);
    if val == Value::Null { return None; }
    
    let low_val = low.execute(row, zoom);
    
    match logical_binary_operation_condition_execute(val.clone(), &LogicalOperator::Ge, low_val) {
        None => {return None; },
        Some(false) => {if !negated { return Some(false); } },
        Some(true) => {if negated { return Some(false); } },
    }
    
    let high_val = high.execute(row, zoom);
    match logical_binary_operation_condition_execute(val, &LogicalOperator::Le, high_val) {
        None => {return None; },
        Some(false) => {if !negated { return Some(false); } },
        Some(true) => {if negated { return Some(false); } },
    }
    Some(true)
}


fn substring_execute<T: Rower>(expr: &Expr, from_expr: &Option<Box<Expr>>, for_expr: &Option<Box<Expr>>, row: &Row<T>, zoom: f64) -> Value {
    
    let val = match expr.execute(row, zoom) {
        Value::Text(val) => val,
        p => {
            println!("substring({:?} ... ) not a string", p);
            return Value::Null;
        }
    };
        
      
    let from_val = match from_expr {
        None => Value::Null,
        Some(f) => f.execute(row, zoom)
    };
    
    let for_val = match for_expr {
        None => Value::Null,
        Some(f) => f.execute(row, zoom)
    };
    
    match (&from_val, &for_val) {
        
        (Value::Null, Value::Integer(t)) => {
            if t+1 > val.len() as i64 || t+1 < 0 {
                println!("substring({:?} for {} out of range...", val, t);
                Value::Null
            } else {
                Value::Text(val[ .. (t+1) as usize].to_string())
            }
        },
        (Value::Integer(f), Value::Integer(t)) => {
            if f+1 < 0 || t+f+1 < 0 || f+1 > val.len() as i64 || t+f+1 > val.len() as i64 {
                println!("substring({:?} from {} for {} out of range...", val, f, t);
                Value::Null
            } else {
                Value::Text(val[(f+1) as usize .. (f+t+1) as usize].to_string())
            }
        },
        (Value::Text(pat), Value::Null) => {
            match Regex::new(&pat) {
                Ok(m) => {
                    match m.find(&val) {
                        Some(c) => Value::Text(c.as_str().to_string()),
                        None => Value::Null,
                    }
                },
                Err(e) => {
                    println!("didn't understand regex {} {:?}", pat, e);
                    Value::Null
                }
            }
        }
        _ => {
            println!("didn't understand substring({:?} from {:?} for {:?})", val, from_val, for_val);
            Value::Null
        }
    }
}
        
        
