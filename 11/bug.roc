app "advent-11-1"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Task.{ Task }]
    provides [main] to pf

main : Task {} []

parseWithLambdas = \s ->
    (if s == "*" then (Op \a, b -> a * b)
    else if s == "+" then (Op \a, b -> a + b)
    else (Op \a, _ -> a))
    |> \instr -> when instr is
        Op op -> (\a -> op a a)
        _ -> (\a -> a)

# Works as expected
expect (parseWithLambdas "*" |> \f -> f 3) == 9
expect (parseWithLambdas "+" |> \f -> f 3) == 6

parseWithBuiltinsNoop = \s ->
    (if s == "*" then (Op Num.mul)
    else if s == "+" then (Op Num.add)
    else Noop)
    |> \instr -> when instr is
        Op op -> (\a -> op a a)
        _ -> (\a -> a)

# should fail, but passes
expect (parseWithBuiltinsNoop "*" |> \f -> f 3) == 6
expect (parseWithBuiltinsNoop "+" |> \f -> f 3) == 9


# should pass, but fails
expect (parseWithBuiltinsNoop "*" |> \f -> f 3) == 9
expect (parseWithBuiltinsNoop "+" |> \f -> f 3) == 6

# parseWithBuiltins = \s ->
#     (if s == "*" then (Op Num.mul)
#     else if s == "+" then (Op Num.add)
#     else (Op \a, _ -> a))
#     |> \instr -> when instr is
#         Op op -> (\a -> op a a)
#         _ -> (\a -> a)

# This expectation crashes :
# expect (parseWithBuiltins "*" |> \f -> f 3) == 9
