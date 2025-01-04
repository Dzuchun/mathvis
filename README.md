The name is `MathVis`. Stands for "Math Vision"

## Adding to project
There's no cargo entry for it yet, but you can (hopefully) still use it by including
```toml
    mathvis = { git = "https://github.com/Dzuchun/mathvis.git" }
```
into your project's `Cargo.toml` file.

# Overview
This is a simple library I coded to recognize basic math expressions involving constants, variables, arithmetic operations and functions. Operating is done exclusively with `num::complex::Complex64` for now, as I have failed with dynamic typing (would appreciate any help with that, you may see some implementations for arbitrary types at my [`first commit`]).

Here's some info on what this library can be used for:

## Lexing
All text input should first be lexed (i.e. turned into tokens) first. This can be done with `mathvis::lexer::lex` function, returning `Vec<Token>`, if successful.

At this point, you may recognize some extended syntax and replace it with your own. Examples:
- `2x` would not be parsed as `2 * x` by default (it would be an error, actually). You may insert `Token::Operator(Operator::Star)` between each `Token::Number` and `Token::Ident` you find to get this sort of behavior.
- Only functions with one or two arguments are supported for now. But three and more - argument function syntax can still recognized my you manually and transformed into two-argument function call(s), if possible.

## Parsing
Slice of tokens can be parsed (i.e. turned into evaluation tree). This is done with `mathvis::evaluation_tree::EvaluationTree::from_tokens` function, returning `EvaluationTree`, if successful.

`EvaluationTree` is my way to represent syntax, in a form that's easy to compute. You can't make any modifications into syntax at this point, *but* you still have not defined any variables and/or functions used, as these are not represented by this tree.

## Evaluation
`EvaluationTree` can be, well, evaluated to a single `Complex64` with `mathvis::evaluation_tree::Evaluatable::evaluate` function. To do that, it needs a suitable `mathvis::evaluation_tree::args::Args` object, used to represent variable values and function definitions. To obtain it (and find out which variables/functions parsed expression actually need), you may use `mathvis::evaluation_tree::Evaluatable::args` function defined for `EvaluationTree`. Resulting `Args` object will have all the required values *registered*, but *assigned*. You are expected to assign all of the values/functions registered by a tree.

You can define your own constants and functions this way, for example:
- To define a constant, you'll want to unconditionally call `Args::assign_variable`.
- To define a custom function, you'll want to use `Args::assign_function`/`Args::assign_function2`. These accept `impl Fn`s, so you may use function pointers as well as closures.

There's no problem in case of unused assignments in `Args` struct, so you are *not forced* to use your custom constants/functions.

Both `EvaluationTree` and `Args` are **not** consumed upon evaluation, meaning you can reuse same tree for iterative computations as well as modify arguments to get a different result.

# Examples
## Simple
The following is a `simple` example, involving all of the steps described above

```rust
# use num::complex::Complex64;
# use crate::mathvis::evaluation_tree::Evaluatable;
let input = "2 * sum_sq(x, y) ^ y + sin(x * y)";

let (_, tokens) = mathvis::lexer::lex(input).expect("Should be able to recognize tokens");

let tree = mathvis::evaluation_tree::EvaluationTree::from_tokens(tokens.as_slice())
    .expect("Should be able to parse into tree");

let mut args = tree.args();
// args are expected to contain variables "x" and "y", as well as "sin" function and "sum_sq" function2
// let define them, then:
args.assign_variable("x", Complex64::new(2.0, -3.0));
args.assign_variable("y", Complex64::new(-std::f64::consts::PI, 1.0));
args.assign_function("sin", Complex64::sin);
args.assign_function2("sum_sq", |x, y| x * x + y * y);

let result = tree.evaluate(&args).expect("Should be able to evaluate");

print!("{}", result);
```
I got an output of
```bash
6460.656547978857-45323.133349725416i
```
which is similar to [what WolframAlpha gets][simple wolfram]

## Cli calculator
The following is a loop part of a `cli_calculator` example. Steps can be clearly seen here.

```rust,ignore
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
```

# On complex arithmetic
Pretty much the only this end user does not control about it - is complex exponentiation syntax. It should be noted, that strictly-speaking, this operation would have several results in case of a non-integer exponent. Currently, I'm using `num`'s exponentiation implementation, but you can obvious define your own (just define a function2 for it).


# Flaws
- **Lots of allocations**: unfortunately, I see no *safe* way to avoid these on every tree node.
- **Slow computation**: right now tree is actually walked around to compute the result. This is less than ideal, as it results in uncountable many pointer chases, and well as decent recursion leveling. To mitigate this, I plan on creating a module to convert `EvaluationTree` into sort-of-compiled instructions array that can be linearly followed during the execution. Any suggestions on this end would be greatly appreciated too.
- **Traits instead of enums**: for some reason, I decided it would be great to give end-used an ability to define their custom node types. But now, when I think about it, it seems that there are not much to be added, especially considering I do not grant an ability for custom syntax (apart from bare token modifications).

I'll try to deal with these some time in the future, but it is like that for now, I guess.


[`first commit`]: https://github.com/Dzuchun/mathvis/commit/6b9cd1ad46366f689b4afd7a4c35069eead69a0f
[simple wolfram]: https://www.wolframalpha.com/input?i=2+*+%28%282+-+3i%29%5E2+%2B++%28-pi%2Bi%29%5E2%29+%5E+%28-pi%2Bi%29+%2B+sin%28%282+-+3i%29+*+%28-pi%2Bi%29%29
