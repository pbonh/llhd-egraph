macro_rules! symbol {
    ($sym:expr) => {
        Symbol::from($sym)
    };
}

macro_rules! span {
    () => {
        DUMMY_SPAN.clone()
    };
}

macro_rules! literal {
    ($val:literal) => {
        Literal::from($val)
    };
}

macro_rules! expr {
    // For literals (integers, strings, etc.)
    ($val:literal) => {
        GenericExpr::Lit(span!(), literal!($val))
    };
    // For variables (assumed to be string literals)
    (var $var:expr) => {
        GenericExpr::Var(span!(), symbol!($var))
    };
    // For function calls with arguments
    ($func:expr, $( $args:tt ),* ) => {
        GenericExpr::Call(
            span!(),
            symbol!($func),
            vec![
                $( expr!($args) ),*
            ],
        )
    };
    // For function calls without arguments
    ($func:expr) => {
        GenericExpr::Call(
            span!(),
            symbol!($func),
            vec![],
        )
    };
}

macro_rules! variant {
    ($name:expr, [$($types:expr),*] $(, cost = $cost:expr)?) => {
        Variant {
            span: span!(),
            name: symbol!($name),
            types: vec![ $( symbol!($types) ),* ],
            cost: None $( .or(Some($cost)) )?,
        }
    };
}

macro_rules! schema {
    (inputs = [$($inputs:expr),*], output = $output:expr) => {
        Schema {
            input: vec![ $( symbol!($inputs) ),* ],
            output: symbol!($output),
        }
    };
}

macro_rules! fact {
    // Equality fact with multiple expressions
    (eq [$( $exprs:tt ),+]) => {
        GenericFact::Eq(
            span!(),
            vec![ $( expr!($exprs) ),+ ],
        )
    };
    // Single expression fact
    ($expr:tt) => {
        GenericFact::Fact(expr!($expr))
    };
}

macro_rules! schedule {
    // Saturate schedule
    (saturate $sched:tt) => {
        GenericSchedule::Saturate(
            span!(),
            Box::new(schedule!($sched)),
        )
    };
    // Repeat schedule
    (repeat $times:expr, $sched:tt) => {
        GenericSchedule::Repeat(
            span!(),
            $times,
            Box::new(schedule!($sched)),
        )
    };
    // Run schedule with ruleset and optional until conditions
    (run $ruleset:expr $(, until = [$($until:tt),*])? ) => {
        GenericSchedule::Run(
            span!(),
            GenericRunConfig {
                ruleset: symbol!($ruleset),
                until: None $( .or(Some(vec![ $( fact!($until) ),* ])) )?,
            },
        )
    };
    // Sequence of schedules
    (sequence [$( $sched:tt ),+]) => {
        GenericSchedule::Sequence(
            span!(),
            vec![ $( schedule!($sched) ),+ ],
        )
    };
}

macro_rules! sort {
    ($symbol:expr, $option:expr) => {
        Sort(DUMMY_SPAN.clone(), $symbol, Some($option))
    };
    ($symbol:expr) => {
        Sort(DUMMY_SPAN.clone(), $symbol, None)
    };
}

macro_rules! function_decl {
    ($name:expr, inputs = [$($inputs:expr),*], output = $output:expr $(, $field_name:ident = $field_value:expr )* ) => {
        GenericFunctionDecl {
            name: symbol!($name),
            schema: Schema {
                input: vec![ $( symbol!($inputs) ),* ],
                output: symbol!($output),
            },
            default: None,
            merge: None,
            merge_action: vec![],
            cost: None,
            unextractable: false,
            ignore_viz: false,
            span: span!(),
            $( $field_name: $field_value ),*
        }
    };
}

macro_rules! cmd {
    // For variants with named fields
    ($variant:ident { $($field_name:ident : $field_value:expr),* $(,)? }) => {
        GenericCommand::$variant {
            $(
                $field_name: cmd_helper!($field_name, $field_value),
            )*
        }
    };
    // For variants with unnamed fields
    ($variant:ident ( $($field_value:expr),* $(,)? )) => {
        GenericCommand::$variant(
            $(
                cmd_helper!(field, $field_value),
            )*
        )
    };
}

macro_rules! cmd_helper {
    // Fields that are Symbols
    (name, $val:expr) => {
        symbol!($val)
    };
    (ruleset, $val:expr) => {
        symbol!($val)
    };
    // Fields that are GenericExpr
    (value, $val:expr) => {
        expr!($val)
    };
    (expr, $val:expr) => {
        expr!($val)
    };
    // Fields that are Variants
    (variants, $val:expr) => {
        $val // Assuming $val is an expression like `vec![ ... ]`
    };
    // Fields that are GenericFunctionDecl
    (function_decl, $val:expr) => {
        $val
    };
    // Fields that are GenericSchedule
    (schedule, $val:expr) => {
        $val
    };
    // Fields that are GenericFact
    (facts, $val:expr) => {
        $val
    };
    // Fields that are Schema
    (schema, $val:expr) => {
        $val
    };
    // Fields that are a Sort
    (sort, $val:expr) => {
        $val
    };
    // For other fields, pass the value as is
    ($field_name:ident, $val:expr) => {
        $val
    };
}
