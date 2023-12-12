use std::io::Write;

use mathvis::evaluation_tree::{args::Args, Evaluatable};
use num::complex::Complex64;

fn main() {
    println!("Welcome to CLI calculator!");
    let mut input = String::new();
    'outer: loop {
        // read input from stdin
        print!("\n\nPlease, input your formula: ");
        std::io::stdout().flush().unwrap();
        input.clear();
        if std::io::stdin().read_line(&mut input).unwrap() == 0 {
            return;
        }
        input.pop(); // pop '\n'

        // lex to tokens
        let tokens = match mathvis::lexer::lex(input.as_str()) {
            Ok((left, tokens)) => {
                if !left.is_empty() {
                    println!("Info: \"{left}\" was ignored");
                }
                tokens
            }
            Err(err) => {
                eprintln!("Failed to evaluate: {err}");
                continue 'outer;
            }
        };

        // parse to tree
        let tree = match mathvis::evaluation_tree::EvaluationTree::from_tokens(tokens.as_slice()) {
            Ok(tree) => tree,
            Err(err) => {
                eprintln!("Failed to parse tokens: {err}");
                continue 'outer;
            }
        };

        // create the arguments
        let mut args = tree.args(); // input args
        args.merge(default_args()); // merge with default args

        if args.functions2().into_iter().next().is_some() {
            // there are functions2, this expression will not be evaluated
            eprintln!("CLI calculator does not support functions2");
            continue 'outer;
        }

        if let Some(name) = args
            .functions_mut()
            .into_iter()
            .find_map(|(name, v)| v.is_none().then_some(name))
        {
            // there are unknown functions
            eprintln!("{name} is an unknown function. CLI calculator does not support your own function definition, please use trig functions only");
            continue 'outer;
        }

        // ask user to assign all the variables
        for (name, val) in args.variables_mut() {
            if val.is_some() {
                continue;
            }
            print!("Please assign {name} := ");
            std::io::stdout().flush().unwrap();
            input.clear();
            if std::io::stdin().read_line(&mut input).unwrap() == 0 {
                return;
            }
            input.pop(); // pop '\n'
            match input.parse() {
                Ok(parsed) => {
                    *val = Some(parsed);
                }
                Err(err) => {
                    eprintln!("Bad assignment format: {err}");
                    continue 'outer;
                }
            }
        }

        // evaluate
        let res = match tree.evaluate(&args) {
            Ok(res) => res,
            Err(err) => {
                eprintln!("Unexpected error while evaluation: {err}");
                continue 'outer;
            }
        };

        println!("The end result is {res}");
    }
}

fn default_args() -> Args {
    let mut args = Args::new();
    // well-known constants
    args.assign_variable("pi", Complex64::from(std::f64::consts::PI));
    args.assign_variable("e", Complex64::from(std::f64::consts::E));

    // basic trig functions
    args.assign_function("sin", Complex64::sin);
    args.assign_function("cos", Complex64::cos);
    args.assign_function("tan", Complex64::tan);
    args.assign_function("tg", Complex64::tan);
    fn cot(z: Complex64) -> Complex64 {
        Complex64::new(1.0, 0.0) / z.tan()
    }
    args.assign_function("cot", cot);
    args.assign_function("ctg", cot);
    args.assign_function("asin", Complex64::asin);
    args.assign_function("acos", Complex64::acos);
    args.assign_function("atan", Complex64::atan);
    args.assign_function("arctg", Complex64::atan);
    fn acot(z: Complex64) -> Complex64 {
        Complex64::new(std::f64::consts::FRAC_PI_2, 0.0) - z.atan()
    }
    args.assign_function("acot", acot);
    args.assign_function("arcctg", acot);
    args
}