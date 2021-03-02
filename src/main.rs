use std::io::{self, Write};
use std::collections::HashMap;
use std::fmt;
#[derive(Clone) ]
enum LispExpr {
    /// TODO: Add a `LispExpr` type to capture the type
    /// of each expression
    /// Symbol: `+`, `-`, `(`, `)`, `def`, `if` 
    /// Number: f64
    List(Vec<LispExpr>),

    /// Atoms
    Symbol(String),
    Number(f64),
    Boolean(bool),

    // Function 
    Function(fn(&[LispExpr]) -> Result<LispExpr, LispErr>)
}

impl fmt::Display for LispExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            LispExpr::List(vec) => {
                let vec: Vec<String> = vec.iter().map(|expr| expr.to_string()).collect();
                format!("{}", vec.join(", "))
            },
            LispExpr::Symbol(sym) => sym.clone(),
            LispExpr::Number(num) => num.to_string(),
            LispExpr::Function(_fun) => "Function {}".to_string(),
            LispExpr::Boolean(b) => b.to_string(),
        };
        write!(f, "{}", res)
    }
}
#[derive(Debug)]
enum LispErr {
    /// TODO: Add a `LispErr` type to capture errors and 
    /// propogate them back to the user
    Reason(String)
}

struct LispEnv {
    /// TODO: Add a `LispEnv` type, which is responsible for
    /// storing defined variables, built-in functions, etc.
    /// Associate '+' with a built-in addition function 
    /// The function needs to be able to handle some arbitrary
    /// number of operands 
    builtins: HashMap<String, LispExpr>,
}

fn parse_floats(floats: &[LispExpr]) -> Result<Vec<f64>, LispErr> {
    floats.iter().map(|expr| {
        match expr {
            LispExpr::Number(number) => Ok(*number),
            _ => Err(LispErr::Reason(String::from("Expected a number")))
        }
    }).collect()
}

impl LispEnv {
    /// TODO: Add a `new` method on the `LispEnv` to initialize
    /// an instance of it
    /// signature will be same for all
    fn new() -> Self {
        let mut builtins = HashMap::new();

        builtins.insert(
            "+".to_string(), 
            LispExpr::Function(|args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let sum = parse_floats(args)?
                    .iter()
                    .fold(0.0, |state, arg| state + arg);
                    
                Ok(LispExpr::Number(sum))
            }
        ));

        builtins.insert(
            "-".to_string(), 
            LispExpr::Function(|args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let parsed_floats = parse_floats(args)?;

                if let Some((first, rest)) = parsed_floats.split_first() {
                    let sum_of_rest = rest.iter()
                        .fold(0.0, |state, arg| state + arg);

                    Ok(LispExpr::Number(first - sum_of_rest))
                } else {
                    Err(LispErr::Reason(String::from("Expected an operand")))
                }
            }
        ));

        builtins.insert(
            ">".to_string(), 
            LispExpr::Function(|args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let parsed_floats = parse_floats(args)?;

                let answer = parsed_floats.windows(2)
                    .all(|s| s[0] > s[1]);

                Ok(LispExpr::Boolean(answer))
            }
        ));

        builtins.insert(
            ">=".to_string(), 
            LispExpr::Function(|args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let parsed_floats = parse_floats(args)?;

                let answer = parsed_floats.windows(2)
                    .all(|s| s[0] >= s[1]);

                Ok(LispExpr::Boolean(answer))
            }
        ));

        builtins.insert(
            "<".to_string(), 
            LispExpr::Function(|args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let parsed_floats = parse_floats(args)?;
                
                let answer = parsed_floats.windows(2)
                    .all(|s| s[0] < s[1]);

                Ok(LispExpr::Boolean(answer))
            }
        ));

        builtins.insert(
            "<=".to_string(), 
            LispExpr::Function(|args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let parsed_floats = parse_floats(args)?;

                let answer = parsed_floats.windows(2)
                    .all(|s| s[0] <= s[1]);

                Ok(LispExpr::Boolean(answer))
            }
        ));

        builtins.insert(
            "=".to_string(), 
            LispExpr::Function(|args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let parsed_floats = parse_floats(args)?;

                let answer = parsed_floats.windows(2)
                    .all(|s| s[0] == s[1]);

                Ok(LispExpr::Boolean(answer))
            }
        ));


        LispEnv { builtins }
    }

    /// TODO: Add an `eval` method on the `LispEnv` that takes
    /// a `LispExpr` reference and evaluates it, returning a
    /// `LispErr` if the eval process failed 
    fn eval(&self, expr: &LispExpr) -> Result<LispExpr, LispErr> {
        // match on the LispExpr 
        match expr {
            // Symbol 
            LispExpr::Symbol(sym) => {
                if let Some(func) = self.builtins.get(sym) {
                    Ok(func.clone())
                } else {
                    Err(LispErr::Reason(format!("Unexpected symbol: {}", sym)))
                }
            },
            LispExpr::Boolean(_b) => Ok(expr.clone()),
            // Number
            LispExpr::Number(_n) => Ok(expr.clone()),
            // Function
            LispExpr::Function(_) => Err(LispErr::Reason(String::from("Unexpected function"))),
            // List
            LispExpr::List(list) => {
                // split off the first expression from the list 
                if let Some((first, rest)) = list.split_first() {
                    // eval the first expression 
                    let first_eval = self.eval(first)?;
                    
                    // match on first_eval and make sure it's a function 
                    match first_eval {
                        LispExpr::Function(f) => {
                            // pass the rest of the exprs as operands to the function 
                            // after evaluating them all 
                            let rest_eval = rest
                                .iter()
                                .map(|r| self.eval(r))
                                .collect::<Result<Vec<LispExpr>, LispErr>>();

                            f(&rest_eval?)
                        },
                        _ => Err(LispErr::Reason(String::from("Expected a function"))),
                    }
                } else {
                    Err(LispErr::Reason(String::from("Expected a non-empty list")))
                }
            },
        }
    }

    // define a `parse_eval` method that ties the parsing and evaluating steps together
    fn parse_eval(&self, user_input: String) -> Result<LispExpr, LispErr> {
        // pass the user's input into the tokenizer
        // pass the tokens to the `parse` function 
        // pass the AST that the parser spits out to the `eval` method 
        let tokens = tokenize(user_input);
        let (parsed_tokens, _unparsed) = parse(&tokens)?;
        let result = self.eval(&parsed_tokens)?;

        Ok(result)
    }
}

/// Given a String expression, returns a Vector of 
/// all the tokens in the expression
/// `(+ 3 2)` -> `['(+', '3', '2)']`
/// `(+ 3 2)` -> ` ( + 3 2 ) `
fn tokenize(expr: String) -> Vec<String> {
    // given some arithmetic expression, breaks it up into tokens
    // `split_whitespace`
    // Use the `replace` method to replace each `(` with ` ( `
    expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        //note, this doens't give Strings back, but a special 'split_whitespace' type
        .split_whitespace()
        //take return values of 'split_whitespace' and turn them into Strings
        // we can do this with a `map` call 
        .map(|value| value.to_string())
        .collect()
}

/// Given some tokens, parses them into a `LispExpr` or
/// returns a `LispErr` if parsing failed
/// 'a is a "lifetime" ( a rust feature)
/// lifetimes are all related to references 
/// lifetimes specify how long a reference lives 
fn parse<'a>(tokens: &'a [String]) -> Result<(LispExpr, &'a [String]), LispErr> {
    // split off the first token from the `tokens` slice 
    let (token, rest) = tokens.split_first()
        .ok_or(LispErr::Reason(String::from("Unexpected end of statement")))?;  // checks that the Option is a Some
    
    // match on the first token 
    match &token[..] {
        // "(" indicates the start of a sequence of characters to read 
        "(" => read_seq(rest),
        // ")" indicates a closer before an opener occurred; return an error
        ")" => Err(LispErr::Reason(String::from("Unexpected `)`"))),
        // _ pass it another function called `parse_atom`
        _ => Ok((parse_atom(token), rest)),
    }
}

/// input: `(+ 3 2)`
/// tokenization: `['(', '+', '3', '2', ')']`
/// parse(['(', '+', '3', '2', ')'])
/// read_seq(['+', '3', '2', ')']) -> Vec[
///     LispExpr::Symbol("+")
///     LispExpr::Number(3)  
///     LispExpr::Number(2)
/// ]

/// Reads all the tokens following an "(" and returns the parsed token 
/// and the following un-parsed tokens 
fn read_seq<'a>(tokens: &'a [String]) -> Result<(LispExpr, &'a [String]), LispErr> {
    // collecting all the tokens in between parens 
    let mut rv: Vec<LispExpr> = vec![];
    let mut ts = tokens;

    loop {
        let (next_token, rest) = ts
            .split_first()
            .ok_or(LispErr::Reason(String::from("Missing `)`")))?;

        if next_token == ")" {
            // we read the entire sequence inside a set of parens 
            return Ok((LispExpr::List(rv), rest));
        }

        // send the first token to be parsed, then add it to our 
        // vector of parsed tokens 
        let (parsed, new_ts) = parse(&ts)?;
        rv.push(parsed);
        ts = new_ts;
    }
}

fn parse_atom(token: &str) -> LispExpr {
    match token {
        "true" => LispExpr::Boolean(true),
        "false" => LispExpr::Boolean(false),
        _ => {
            // attempt to parse the token as a f64
            match token.parse::<f64>() {
                // if parsing to an f64 succeeded, return the number
                Ok(float) => LispExpr::Number(float),
                // otherwise, treat it as a symbol
                Err(_) => LispExpr::Symbol(String::from(token))
            }
        }
    }
}

/// Reads input from stdin and returns it as a String
fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

/// Main function that initializes the REPL loop
fn main() {
    // init the LispEnv 
    let environ = LispEnv::new();
    
    loop {
        print!("> ");
        // force the above `print` to actually print to stdout
        io::stdout().flush().unwrap();

        // fetch the user's input
        let user_input = slurp_expr();
        let user_input = user_input.trim();

        if user_input == ":q" {
            break;
        } else if user_input == ":quit" {
            break
        }

        match environ.parse_eval(String::from(user_input)) {
            Ok(result) => println!("{}", result),
            Err(e) => eprintln!("Error: {:?}", e),
        }
    }
}