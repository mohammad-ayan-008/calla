use crate::{expr::{Expr, Literal}, stmt::Stmt};
use pest::{
    Parser,
    iterators::Pair,
};
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
        Rule::equality => parse_binary(pair.into_inner(), &[Rule::equality_op]),
        Rule::comparision => parse_binary(pair.into_inner(), &[Rule::comparision_op]),
        Rule::term => parse_binary(pair.into_inner(), &[Rule::term_op]),
        Rule::factor => parse_binary(pair.into_inner(), &[Rule::factor_op]),
        _ => Err("undef rule".to_owned()),
    }
}


pub fn parse_expr_stmts(source: &str) -> Result<Vec<Stmt>, String> {
    let parser = CallaParser::parse(Rule::stmt, source).unwrap();
    let mut stmts = vec![];

    for pair in parser {
        match pair.as_rule() {
            Rule::stmt => {
                for inner in pair.into_inner() {
                    match inner.as_rule() {
                        Rule::printst => {
                            let mut pair1 = inner.into_inner();
                            let formatter = pair1.next().unwrap().as_str();
                            let expr = parse_ast(pair1.next().unwrap())?;
                            stmts.push(Stmt::print { format_argss: formatter[1..formatter.len()-1].to_owned(), expr });
                        }
                        _ => return Err("no such statement".to_string()),
                    }
                }
            }
            _ => return Err("unexpected top-level rule".to_string()),
        }
    }

    println!("{:?}", stmts);
    Ok(stmts)
}

