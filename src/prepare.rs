use crate::{Value,Table,Direction,DataType};//PickTable,Select,Direction,DataType,UnionTable};
//,Column,PickTable,PickColumn,AliasColumn,FunctionColumn,CaseColumn,OperationColumn,CaseClause};
use crate::{Expr,Operator,LogicalOperator,NumericalOperator,SpecialOperator,UnaryOperator};

//use itertools::join;
use std::io::{Result,Error,ErrorKind};



pub fn prep_expr(expr: &sqlparser::ast::Expr) -> Result<Expr> {
    
    match expr {
        sqlparser::ast::Expr::Value(sqlparser::ast::Value::Number(n, _)) => {
            match n.parse::<i64>() {
                Ok(n) => Ok(Expr::Value{value: Value::Integer(n)}),
                Err(_) => {
                    match n.parse::<f64>() {
                        Ok(f) => Ok(Expr::Value{value: Value::Float(f)}),
                        Err(_) => Err(Error::new(ErrorKind::Other, format!("not impl {:?}", expr)))
                    }
                }
            }
        },
        sqlparser::ast::Expr::Value(sqlparser::ast::Value::SingleQuotedString(ref s)) => Ok(Expr::Value{value: Value::Text(s.clone())}),
        
        sqlparser::ast::Expr::Value(sqlparser::ast::Value::NationalStringLiteral(ref s)) => Ok(Expr::Value{value: Value::Text(s.clone())}),

        sqlparser::ast::Expr::Value(sqlparser::ast::Value::Boolean(n)) => Ok(Expr::Value{value: Value::Bool(*n)}),

        sqlparser::ast::Expr::Value(sqlparser::ast::Value::Null) => Ok(Expr::Value{value: Value::Null}),
        
        
        sqlparser::ast::Expr::Value(sqlparser::ast::Value::HexStringLiteral(s)) => Ok(Expr::Value{value: Value::Text(s.clone())}),
        sqlparser::ast::Expr::Value(sqlparser::ast::Value::DoubleQuotedString(s)) => Ok(Expr::Value{value: Value::Text(s.clone())}),
        
                
        sqlparser::ast::Expr::Identifier(ref id) => Ok(Expr::Pick{name: prep_ident(id)}),

        sqlparser::ast::Expr::Value(sqlparser::ast::Value::Interval{..}) => Err(Error::new(ErrorKind::Other, format!("not impl {:?}", expr))),
        
        sqlparser::ast::Expr::Case { operand, conditions, results, else_result } => {
            
            if !operand.is_none() {
                return Err(Error::new(ErrorKind::Other, format!("weird case ? {:?} {:?} {:?} {:?})", operand, conditions, results, else_result)));
            }
            let mut when_clauses = Vec::new();
            for (condition,result) in conditions.iter().zip(results.iter()) {
                
                let when = prep_expr(condition)?;
                if !when.is_condition() {
                    return Err(Error::new(ErrorKind::Other, format!("when clause not valid condition {:?}", when)))
                }
                let then = prep_expr(result)?;
                when_clauses.push((when, then));
            }
            
            let else_clause = match else_result {
                None => None,
                Some(expr) => Some(Box::new(prep_expr(expr)?))
            };
            
            Ok(Expr::Case{when_clauses: when_clauses, else_clause: else_clause})
        }
               

        sqlparser::ast::Expr::Cast { ref expr, ref data_type } => {
            let dt = match data_type {
                
                sqlparser::ast::DataType::Boolean => DataType::Bool,
                sqlparser::ast::DataType::SmallInt(_display) => DataType::Integer,
                sqlparser::ast::DataType::Int(_display) => DataType::Integer,
                sqlparser::ast::DataType::BigInt(_display) => DataType::Integer,
                sqlparser::ast::DataType::Float(_) | sqlparser::ast::DataType::Real => DataType::Float,
                sqlparser::ast::DataType::Double => DataType::Float,
                sqlparser::ast::DataType::Text => DataType::Text,
                sqlparser::ast::DataType::Char(_) | sqlparser::ast::DataType::Varchar(_) => DataType::Text,
                sqlparser::ast::DataType::Decimal(..) => DataType::Float,
                //sqlparser::ast::DataType::Timestamp => DataType::Timestamp(TimeUnit::Nanosecond, None)),
                //sqlparser::ast::DataType::Date => DataType::Date32),
                other => { return Err(Error::new(ErrorKind::Other, format!("Unsupported data_type {:?}",other))); },
            };
            
            Ok(Expr::Cast{expr: Box::new(prep_expr(expr)?), data_type: dt})
            
        },  
        sqlparser::ast::Expr::IsNull(ref expr) => Ok(Expr::IsNull{expr: Box::new(prep_expr(expr)?)}),
        sqlparser::ast::Expr::IsNotNull(ref expr) => Ok(Expr::NotIsNull{expr: Box::new(prep_expr(expr)?)}),
        
        
        sqlparser::ast::Expr::BinaryOp {ref left, ref op, ref right} => {
            let operator = match *op {
                sqlparser::ast::BinaryOperator::Gt => Ok(Operator::LogicalOperator(LogicalOperator::Gt)),
                sqlparser::ast::BinaryOperator::GtEq => Ok(Operator::LogicalOperator(LogicalOperator::Ge)),
                sqlparser::ast::BinaryOperator::Lt => Ok(Operator::LogicalOperator(LogicalOperator::Lt)),
                sqlparser::ast::BinaryOperator::LtEq => Ok(Operator::LogicalOperator(LogicalOperator::Le)),
                sqlparser::ast::BinaryOperator::Eq => Ok(Operator::LogicalOperator(LogicalOperator::Eq)),
                sqlparser::ast::BinaryOperator::NotEq => Ok(Operator::LogicalOperator(LogicalOperator::NotEq)),
                sqlparser::ast::BinaryOperator::Plus => Ok(Operator::NumericalOperator(NumericalOperator::Plus)),
                sqlparser::ast::BinaryOperator::Minus => Ok(Operator::NumericalOperator(NumericalOperator::Minus)),
                sqlparser::ast::BinaryOperator::Multiply => Ok(Operator::NumericalOperator(NumericalOperator::Multiply)),
                sqlparser::ast::BinaryOperator::Divide => Ok(Operator::NumericalOperator(NumericalOperator::Divide)),
                sqlparser::ast::BinaryOperator::Modulo => Ok(Operator::NumericalOperator(NumericalOperator::Modulo)),
                sqlparser::ast::BinaryOperator::And => Ok(Operator::LogicalOperator(LogicalOperator::And)),
                sqlparser::ast::BinaryOperator::Or => Ok(Operator::LogicalOperator(LogicalOperator::Or)),
                sqlparser::ast::BinaryOperator::StringConcat => Ok(Operator::SpecialOperator(SpecialOperator::StringConcat)),
                sqlparser::ast::BinaryOperator::HyphenRight => Ok(Operator::SpecialOperator(SpecialOperator::HStoreAccess)),
                sqlparser::ast::BinaryOperator::HyphenRightRight => Ok(Operator::SpecialOperator(SpecialOperator::HStoreAccess)),
                sqlparser::ast::BinaryOperator::Question => Ok(Operator::SpecialOperator(SpecialOperator::HStoreContainsKey)),
                sqlparser::ast::BinaryOperator::AtRight => Ok(Operator::SpecialOperator(SpecialOperator::HStoreContainsKeyValue)),
                sqlparser::ast::BinaryOperator::PGRegexMatch => Ok(Operator::SpecialOperator(SpecialOperator::RegExpMatch)),
                sqlparser::ast::BinaryOperator::AtLeft => Ok(Operator::SpecialOperator(SpecialOperator::HStoreContainsKeyValueReverse)),
                /*sqlparser::ast::BinaryOperator::Like => Ok(Operator::Like),
                sqlparser::ast::BinaryOperator::NotLike => Ok(Operator::NotLike),
                
                sqlparser::ast::BinaryOperator::PGRegexIMatch => Ok(Operator::RegexIMatch),
                sqlparser::ast::BinaryOperator::PGRegexNotMatch => Ok(Operator::RegexNotMatch),
                sqlparser::ast::BinaryOperator::PGRegexNotIMatch => Ok(Operator::RegexNotIMatch),*/
                _ => Err(Error::new(ErrorKind::Other, format!("Unsupported SQL binary operator {:?}",op))),
            }?;
            
            let left = Box::new(prep_expr(left)?);
            let right = Box::new(prep_expr(right)?);
            match operator {
                Operator::LogicalOperator(LogicalOperator::And) | Operator::LogicalOperator(LogicalOperator::Or) => {
                    if !left.is_condition() || !right.is_condition() {
                        return Err(Error::new(ErrorKind::Other, format!("{:?} {:?} {:?}??", left, operator, right)));
                    }
                },
                _ => {}
            }
            
            Ok(Expr::BinaryOperation {left: left, operator: operator, right: right})
        },
        sqlparser::ast::Expr::InList {ref expr, ref list, ref negated} => {
            let expr_list = list
                .iter()
                .map(|e| prep_expr(e))
                .collect::<Result<Vec<_>>>()?;
            
            Ok(Expr::InList {
                expr: Box::new(prep_expr(expr)?),
                expr_list: expr_list,
                negated: *negated
            })
        },
                    
        sqlparser::ast::Expr::Nested(e) => prep_expr(e),
        sqlparser::ast::Expr::Function(function) => {
            let name = if function.name.0.len() > 1 {
                // DF doesn't handle compound identifiers
                // (e.g. "foo.bar") for function names yet
                function.name.to_string()
            } else {
                // if there is a quote style, then don't normalize
                // the name, otherwise normalize to lowercase
                let ident = &function.name.0[0];
                match ident.quote_style {
                    Some(_) => ident.value.clone(),
                    None => ident.value.to_ascii_lowercase(),
                }
            };
            if !function.over.is_none() {
                return Err(Error::new(ErrorKind::Other, format!("can't handle window functions.. {:?}", function)))
            }
            
            
            // first, scalar built-in
            //if let Ok(fun) = functions::BuiltinScalarFunction::from_str(&name) {
            let args = function.args.iter().map(|a| {
                match a {
                    sqlparser::ast::FunctionArg::Named { name: _, arg } => prep_expr(arg),
                    sqlparser::ast::FunctionArg::Unnamed(value) => prep_expr(value),
                }
            }).collect::<Result<Vec<Expr>>>()?;
            if !crate::expr::check_function(&name, args.len()) {
                return Err(Error::new(ErrorKind::Other, format!("can't handle function.. {:?}", function)))
            }
            Ok(Expr::Function { function: name, arguments: args })
                
        },
        
        sqlparser::ast::Expr::UnaryOp { ref op, ref expr } => match op {
            sqlparser::ast::UnaryOperator::Not => Ok(Expr::UnaryOperation{ expr: Box::new(prep_expr(expr)?), operator: UnaryOperator::Not}),
            _ => Err(Error::new(ErrorKind::Other, format!("Unsupported SQL unary operator {:?}", op))),
        },
        
        sqlparser::ast::Expr::Substring { ref expr, ref substring_from, ref substring_for } => {
            let expr = prep_expr(expr)?;
            let substring_from = match substring_from { None => None, Some(e) => Some(Box::new(prep_expr(e)?)) };
            let substring_for = match substring_for { None => None, Some(e) => Some(Box::new(prep_expr(e)?)) };
            
            Ok(Expr::Substring{ expr: Box::new(expr), from_expr: substring_from, for_expr: substring_for})
            
        },
        sqlparser::ast::Expr::Wildcard => Err(Error::new(ErrorKind::Other, format!("Wildcard shouldn't be reached"))),
        
        
        sqlparser::ast::Expr::Between {ref expr, ref negated, ref low, ref high} => 
            Ok(Expr::Between {
                expr: Box::new(prep_expr(expr)?),
                negated: *negated,
                low: Box::new(prep_expr(low)?),
                high: Box::new(prep_expr(high)?),
            }),
        
        
        sqlparser::ast::Expr::Trim { expr, trim_where } => {
            let arg = prep_expr(expr)?;
            
            let (fun, args) = match trim_where {
                Some((sqlparser::ast::TrimWhereField::Leading, expr)) => {
                    ("left_trim_from", vec![arg, prep_expr(expr)?])
                }
                Some((sqlparser::ast::TrimWhereField::Trailing, expr)) => {
                    ("right_trim_from", vec![arg, prep_expr(expr)?])
                }
                Some((sqlparser::ast::TrimWhereField::Both, expr)) => {
                    ("both_trim_from", vec![arg, prep_expr(expr)?])
                }
                None => ("trim", vec![arg]),
            };
            Ok(Expr::Function { function: fun.to_string(), arguments: args })
        },
        
        sqlparser::ast::Expr::Extract { field, expr } => Err(Error::new(ErrorKind::Other, format!("not impl Extract({:?} {:?})", field,expr))),
        sqlparser::ast::Expr::CompoundIdentifier(ids) => Err(Error::new(ErrorKind::Other, format!("CompoundIdentifier {:?} not impl", ids))),
                
        sqlparser::ast::Expr::TryCast {ref expr, ref data_type} => Err(Error::new(ErrorKind::Other, format!("TryCast {:?} {:?} not impl", expr, data_type))),
        sqlparser::ast::Expr::TypedString {ref value, ref data_type} => Err(Error::new(ErrorKind::Other, format!("TypedString {:?} {:?} not impl", value, data_type))),
        
        sqlparser::ast::Expr::QualifiedWildcard(_) => Err(Error::new(ErrorKind::Other, format!("not impl QualifiedWildcard"))),
        
        sqlparser::ast::Expr::InSubquery{..} => Err(Error::new(ErrorKind::Other, format!("not impl InSubquery"))),
        sqlparser::ast::Expr::Subquery{..} => Err(Error::new(ErrorKind::Other, format!("not impl Subquery"))),
        sqlparser::ast::Expr::Collate{..} => Err(Error::new(ErrorKind::Other, format!("not impl Collate"))),
        sqlparser::ast::Expr::Exists{..} => Err(Error::new(ErrorKind::Other, format!("not impl Exists"))),
        sqlparser::ast::Expr::MapAccess{..} => Err(Error::new(ErrorKind::Other, format!("not impl MapAccess"))),
        sqlparser::ast::Expr::ListAgg(_) => Err(Error::new(ErrorKind::Other, format!("not impl ListAgg"))),
        sqlparser::ast::Expr::IsDistinctFrom(..) => Err(Error::new(ErrorKind::Other, format!("not impl IsDistinctFrom"))),
        sqlparser::ast::Expr::IsNotDistinctFrom(..) => Err(Error::new(ErrorKind::Other, format!("not impl IsNotDistinctFrom"))),
        
        
    }
}

fn prep_ident(ident: &sqlparser::ast::Ident) -> String {
    let s = ident.to_string();
    if s.len()>2 && &s[..1] == "\"" && &s[s.len()-1..]=="\"" {
        s[1..s.len()-1].to_string()
    } else {
        s
    }
    
}


fn prep_objectname(objname: &sqlparser::ast::ObjectName) -> String {
    objname.to_string()
    
}


pub fn prep_select(select: &sqlparser::ast::Select) -> Result<Table> {
    if !select.top.is_none() || !select.group_by.is_empty() || !select.cluster_by.is_empty() || !select.distribute_by.is_empty() || !select.sort_by.is_empty() || !select.having.is_none() {
        return Err(Error::new(ErrorKind::Other, format!("not impl {:?}", select)));
    }
    
    if select.from.len() != 1 {
        return Err(Error::new(ErrorKind::Other, format!("select.from.len() != 1 not impl {:?}", select.from)));
    }
    
    if !select.from[0].joins.is_empty() {
        return Err(Error::new(ErrorKind::Other, format!("!select.from[0].joins().is_empty() not impl {:?}", select.from[0].joins)));
    }
    
    let mut table = match &select.from[0].relation {
        sqlparser::ast::TableFactor::Table{name,..} => Table::Pick{name: prep_objectname(&name)},
        sqlparser::ast::TableFactor::Derived{subquery, ..} => prep_query(&subquery)?,
        x => { return Err(Error::new(ErrorKind::Other, format!("not impl {:?}", x))); }
    };
    
    
    match &select.selection {
        None => {},
        Some(expr) => {
            let filter = prep_expr(expr)?;
            if !filter.is_condition() {
                return Err(Error::new(ErrorKind::Other, format!("{:?} not a condition", filter)));
            }
            
            table = Table::Selection{selection: Box::new(filter), table: Box::new(table)};
        }
    };
    
    if select.projection.len()==1 && select.projection[0] == sqlparser::ast::SelectItem::Wildcard {
        // nothing
    } else {
        
        let mut cols = Vec::new();
    
        for c in &select.projection {
            match c {
                sqlparser::ast::SelectItem::UnnamedExpr(expr) => { cols.push(prep_expr(expr)?); },
                sqlparser::ast::SelectItem::ExprWithAlias{expr,alias} => {
                    let cc = prep_expr(expr)?;
                    let aa = prep_ident(alias);
                    //cols.push(Column::Alias(crate::column::AliasColumn{alias: aa, column: Box::new(cc)}));
                    cols.push(Expr::Alias{expr: Box::new(cc), alias: aa});
                },
                _ => return Err(Error::new(ErrorKind::Other, format!("not impl {:?}", c)))
            }
        }
        table = Table::Columns{columns: cols, table: Box::new(table)};
        
    }
    
    Ok(table)
    
}
    

pub fn prep_query(query: &sqlparser::ast::Query) -> Result<Table> {
    
    if !query.with.is_none() || !query.limit.is_none() || !query.offset.is_none() || !query.fetch.is_none() {
        return Err(Error::new(ErrorKind::Other, format!("not impl {:?}", query)))
    }
    
    let mut order_by = Vec::new();
    for o in &query.order_by {
        order_by.push((prep_expr(&o.expr)?, match o.asc { Some(false) => Direction::Desc, _ => Direction::Asc}));
    }
    
    let body = prep_setexpr(&query.body)?;
    if order_by.is_empty() {
        Ok(body)
    } else {
        Ok(Table::OrderBy{table: Box::new(body), order_by: order_by})
    }
}

fn prep_setexpr(setexpr: &sqlparser::ast::SetExpr) -> Result<Table> {
    match &setexpr {
        sqlparser::ast::SetExpr::Select(s) => prep_select(&s),
        sqlparser::ast::SetExpr::Query(q) => prep_query(&q),
        sqlparser::ast::SetExpr::SetOperation {op, left, right, all} => {
            match (op, all) {
                (sqlparser::ast::SetOperator::Union, true) => {
                    let left_table = prep_setexpr(left.as_ref())?;
                    let right_table = prep_setexpr(right.as_ref())?;
                    Ok(Table::Union{left: Box::new(left_table), right: Box::new(right_table)})
                },
                _ => Err(Error::new(ErrorKind::Other, format!("SetExpr not impl {:?} {:?} {:?} {}", op,left,right,all)))
            }
        },
        _ => Err(Error::new(ErrorKind::Other, format!("not impl {:?}", setexpr)))
    }    
}

pub fn prep_statement(ast: &sqlparser::ast::Statement) -> Result<Table> {
    
    match ast {
        sqlparser::ast::Statement::Query(q) => prep_query(q),
        _ => Err(Error::new(ErrorKind::Other, format!("not impl for {:?}", ast)))
    }
}
