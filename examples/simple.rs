use mathvis::evaluation_tree::Evaluatable;
use num::complex::Complex64;

fn main() {
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
}
