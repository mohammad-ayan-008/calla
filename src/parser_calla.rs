use std::{env::set_var, str::Split};

use crate::{
    expr::{Expr, Literal},
    stmt::Stmt,
};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "calla_lang.pest"]
pub struct CallaParser;

pub fn parse_literal(pair: Pair<Rule>) -> Result<Literal, String> {
    let inner = pair.into_inner().next().unwrap();
    let lit = match inner.as_rule() {
        Rule::float => Literal::FLOAT(inner.as_str().parse().unwrap()),
        Rule::integer => Literal::INT(inner.as_str().parse().unwrap()),
        Rule::string => {
            let strs = inner.as_str();
            Literal::String(strs[1..strs.len() - 1].to_owned())
        }
        Rule::boolean => {
            if inner.as_str() == "true" {
                Literal::Bool(true)
            } else {
                Literal::Bool(false)
            }
        }
        Rule::NULL => Literal::Nil,
        _ => return Err(format!("uknown literal {}", inner.as_str())),
    };
    Ok(lit)
}
fn parse_binary(
    mut inner: pest::iterators::Pairs<Rule>,
    allowed_ops: &[Rule],
) -> Result<Expr, String> {
    let mut left = parse_ast(inner.next().ok_or("Expected left-hand side")?)?;
    while let Some(op_pair) = inner.next() {
        if !allowed_ops.contains(&op_pair.as_rule()) {
            return Err(format!("Unexpected operator: {:?}", op_pair.as_str()));
        }
        let op = op_pair.as_str().trim().to_string();
        let right = parse_ast(
            inner
                .next()
                .ok_or(format!("Expected right-hand side after {:?}", op))?,
        )?;
        left = Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        };
    }
    Ok(left)
}

pub fn parse_ast(pair: Pair<Rule>) -> Result<Expr, String> {
    match pair.as_rule() {
      

   Rule::assignment => {
    // we don't trust the shape blindly — inspect children safely
    let mut inner = pair.into_inner();
    let first = inner.next().ok_or("Empty assignment node")?;

    // If there's no second child, this wasn't an identifier = expr pair;
    // it must be a nested expression like `equality`, so just parse it.
    if let Some(second) = inner.next() {
        // two children present -> expect: identifier, exp
        if first.as_rule() == Rule::identifier {
            let name = first.as_str().to_string();
            let value_expr = parse_ast(second)?; // parse rhs expression safely
            return Ok(Expr::Assign { name, exp: Box::new(value_expr) });
        } else {
            // Not an identifier on LHS — it's malformed for assignment, fallback
            return Err(format!("Invalid assignment LHS: {:?}", first.as_str()));
        }
    } else {
        // Single child — treat as normal expression (e.g. equality)
        return parse_ast(first);
    }
}
        Rule::exp => parse_ast(pair.into_inner().next().unwrap()),
        Rule::literal => Ok(Expr::Literal {
            value: parse_literal(pair)?,
        }),
        Rule::primary => parse_ast(pair.into_inner().next().unwrap()),
        Rule::unary => {
            let mut inner = pair.into_inner();
            let first = inner.next().unwrap();
            if ["!", "-"].contains(&first.as_str()) {
                let right = parse_ast(inner.next().unwrap())?;
                Ok(Expr::Unary {
                    op: first.as_str().to_owned(),
                    expr: Box::new(right),
                })
            } else {
                parse_ast(first)
            }
        }

        Rule::fn_call => {
            let mut inner = pair.into_inner(); // fn_call inner: identifier, then possibly exp(s)

            // First item = function name (identifier)
            let name = inner.next().unwrap().as_str().to_string();

            // The rest (if any) = arguments (expressions)
            let mut args = Vec::new();
            for arg_pair in inner {
                // Each arg is an expression node — so recurse into your `build_expr`
                args.push(parse_ast(arg_pair)?);
            }

            Ok(Expr::Func_call { name, expr: args })
        }
        Rule::identifier => {
            let inner = pair.as_str().to_string();
            if !(inner.as_str() == "true" || inner.as_str() == "false") {
                Ok(Expr::Variable { name: inner })
            } else {
                let a = inner == "true";
                Ok(Expr::Literal {
                    value: Literal::Bool(a),
                })
            }
        }
        Rule::equality => parse_binary(pair.into_inner(), &[Rule::equality_op]),
        Rule::comparision => parse_binary(pair.into_inner(), &[Rule::comparision_op]),
        Rule::term => parse_binary(pair.into_inner(), &[Rule::term_op]),
        Rule::factor => parse_binary(pair.into_inner(), &[Rule::factor_op]),
        _ => Err("undef rule".to_owned()),
    }
}

pub fn parse_params<'a>(sp: Split<'a, &'static str>) -> Vec<String> {
    let mut vec = vec![];
    for i in sp {
        println!("->{i}");
    }
    vec
}

pub fn parse_expr_stmts(source: &str) -> Result<Vec<Stmt>, String> {
    // Parse the program
    let parser =
        CallaParser::parse(Rule::program, source).map_err(|e| format!("Parse error: {}", e))?;
    let mut stmts = vec![];

    for pair in parser {
        match pair.as_rule() {
            Rule::program => {
                // Iterate over top-level functions
                for inner in pair.into_inner() {
                    match inner.as_rule() {
                        Rule::func => {
                            let mut func_inner = inner.into_inner();

                            let fn_name = func_inner.next().unwrap().as_str().to_owned();
                            let mut params = Vec::new();
                            let mut ret_type = String::new();
                            let mut func_stmts = Vec::new();

                            // Iterate through function internals
                            for inner_pair in func_inner {
                                match inner_pair.as_rule() {
                                    Rule::params => {
                                        for param_pair in inner_pair.into_inner() {
                                            if param_pair.as_rule() == Rule::param {
                                                let mut p = param_pair.into_inner();
                                                let ty = p.next().unwrap().as_str().to_string();
                                                let name = p.next().unwrap().as_str().to_string();
                                                params.push((ty, name));
                                            }
                                        }
                                    }
                                    Rule::r#type => {
                                        // Return type
                                        ret_type = inner_pair.as_str().to_string();
                                    }
                                    Rule::stmt => {
                                        let inner_stmt = inner_pair.into_inner().next().unwrap();
                                        match inner_stmt.as_rule() {
                                            Rule::printst => {
                                                let mut print_inner = inner_stmt.into_inner();
                                                let fmt = print_inner.next().unwrap().as_str();
                                                let expr = parse_ast(print_inner.next().unwrap())?;
                                                func_stmts.push(Stmt::print {
                                                    format_argss: fmt[1..fmt.len() - 1].to_owned(),
                                                    expr,
                                                });
                                            }
                                            Rule::var_decl => {
                                                let mut decl_inner = inner_stmt.into_inner();
                                                let ty =
                                                    decl_inner.next().unwrap().as_str().to_owned();
                                                let name =
                                                    decl_inner.next().unwrap().as_str().to_owned();
                                                let expr = parse_ast(decl_inner.next().unwrap())?;
                                                func_stmts.push(Stmt::Var {
                                                    identifier: name,
                                                    data_type: ty,
                                                    expr,
                                                });
                                            }
                                            Rule::ret_st => {
                                                let expr = parse_ast(
                                                    inner_stmt.into_inner().next().unwrap(),
                                                )?;
                                                func_stmts.push(Stmt::Return { exp: expr });
                                            }
                                            Rule::expr_stmt => {
                                                let exp = parse_ast(
                                                    inner_stmt.into_inner().next().unwrap(),
                                                )?;
                                                func_stmts.push(Stmt::EXPR_STMT { expr: exp });
                                            }
                                            _ => {
                                                return Err(
                                                    "Invalid statement inside function".to_string()
                                                );
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            stmts.push(Stmt::Func {
                                name: fn_name,
                                ret_type,
                                params,
                                expr: func_stmts,
                            });
                        }
                        _ => return Err("Unexpected top-level rule, expected function".to_string()),
                    }
                }
            }
            _ => return Err("Unexpected top-level rule".to_string()),
        }
    }

    println!("{:#?}", stmts);
    Ok(stmts)
}
