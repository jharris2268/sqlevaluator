use crate::{Row,Value,Expr,Rower};

use itertools::join;

#[cfg(feature = "with-serde")]
use serde::{Serialize,Deserialize};


pub trait TablesPick<T: Rower> {
    fn pick<'a>(&'a self, name: &str) -> Option<&'a Vec<Row<T>>>;
}


#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "with-serde", derive(Serialize, Deserialize))]
pub enum Table {
    Columns{columns: Vec<Expr>, table: Box<Table>},
    Selection{selection: Box<Expr>, table: Box<Table>},
    OrderBy{order_by: Vec<(Expr,Direction)>, table: Box<Table>},
    Pick{name: String},
    Union{left: Box<Table>, right: Box<Table>}
}

impl Table {
    
    pub fn execute<T: TablesPick<S>, S: Rower+Clone>(&self, tables: &T, zoom: f64) -> Vec<Row<S>> {
        
        match self {
            Table::Columns{columns, table} => {
                let mut res = Vec::new();
                for r in table.execute(tables, zoom) {
                    res.push(execute_columns(&columns, &r, zoom));
                }
                res
            },
            Table::Selection{selection, table} => {
                let mut res = Vec::new();
                for r in table.execute(tables, zoom) {
                    match selection.condition_execute(&r, zoom) {
                        Some(true) => { res.push(r); },
                        None | Some(false) => {}
                    }
                }
                res
            },
            Table::OrderBy{order_by, table} => {
                let mut res = table.execute(tables, zoom);
                res.sort_by(|l,r| compare_order_by(&order_by, zoom, &l, &r));
                res
            },
            Table::Pick{name} => {
                match tables.pick(&name) {
                    Some(r) => r.to_vec().clone(),
                    None => Vec::new()
                }
            },
            Table::Union{left, right} => {
                let mut res = left.execute(tables, zoom);
                res.extend(right.execute(tables, zoom));
                res
            }
        }
    }
    
    pub fn sql(&self) -> String {
        match self {
            Table::Columns{columns, table} => {
                format!("SELECT {} FROM ({})", join(columns.iter().map(|c| c.sql()), ", "), table.sql())
            },
            Table::Selection{selection, table} => {
                format!("SELECT * FROM ({}) WHERE {}", table.sql(), selection.sql())
            },
            Table::OrderBy{order_by, table} => {
                format!("SELECT * FROM ({}) ORDER BY {}", table.sql(), join(order_by.iter().map(order_clause_sql), ", "))
            },
            Table::Pick{name} => {
                name.clone()
            },
            Table::Union{left,right} => {
                format!("({}) UNION ALL ({})", left.sql(), right.sql())
            }
        }
    }
    
}
            
                
            
            
        
        
    


#[derive(Debug,PartialEq)]
#[cfg_attr(feature = "with-serde", derive(Serialize, Deserialize))]
pub enum Direction {
    Asc,
    Desc
}


fn order_clause_sql(c: &(Expr, Direction)) -> String {
    format!("{}{}", c.0.sql(), if c.1==Direction::Desc { " DESC" } else { "" })
}

fn compare_order_by<T: Rower>(order_by: &Vec<(Expr,Direction)>, zoom: f64, l: &Row<T>, r: &Row<T>) -> std::cmp::Ordering {
    
    for (c,d) in order_by {
        
        let lv = c.execute(l, zoom);
        let rv = c.execute(r, zoom);
        
        let p = match (lv, rv) {
            (Value::Null, _) => std::cmp::Ordering::Less,
            (_,Value::Null) => std::cmp::Ordering::Greater,
            (Value::Text(ls),Value::Text(rs)) => ls.cmp(&rs),
            (Value::Integer(ls),Value::Integer(rs)) => ls.cmp(&rs),
            (Value::Float(ls),Value::Float(rs)) => ls.partial_cmp(&rs).unwrap_or(std::cmp::Ordering::Equal),
            (Value::Integer(ls),Value::Float(rs)) => (ls as f64).partial_cmp(&rs).unwrap_or(std::cmp::Ordering::Equal),
            (Value::Float(ls),Value::Integer(rs)) => ls.partial_cmp(&(rs as f64)).unwrap_or(std::cmp::Ordering::Equal),
            _ => std::cmp::Ordering::Equal
        };
        
        
        match (p, d) {
            (std::cmp::Ordering::Less, Direction::Asc) => { return std::cmp::Ordering::Less; }
            (std::cmp::Ordering::Equal, Direction::Asc) => {}
            (std::cmp::Ordering::Greater, Direction::Asc) => { return std::cmp::Ordering::Greater; }
            (std::cmp::Ordering::Less, Direction::Desc) => { return std::cmp::Ordering::Greater; }
            (std::cmp::Ordering::Equal, Direction::Desc) => {},
            (std::cmp::Ordering::Greater, Direction::Desc) => { return std::cmp::Ordering::Less; }
        }
    }
    return std::cmp::Ordering::Equal;
}
    
pub fn execute_columns<T: Rower>(columns: &Vec<Expr>, row: &Row<T>, zoom: f64) -> Row<T> {
    
    let mut nrow = row.extend();
    
    for (i,c) in columns.iter().enumerate() {
        let v = c.execute(row, zoom);
        let n = match c.field_name() {
            Some(s) => s,
            None => format!("col_{}", i)
        };
        nrow.add(n,v);
    }
    Row::Extended(nrow)
}


