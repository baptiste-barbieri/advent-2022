app "advent-04-1"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

main : Task {} []
main =
    task =
        path = Path.fromStr "input"
        contents <- await (File.readUtf8 path)
        process contents
        |> Stdout.line

    Task.attempt task \result ->
        Stdout.line (when result is
            Ok _ -> "Successfully did stuff"
            Err err -> when err is
                FileReadErr _ _ -> "Error reading file"
                _ -> "Unknown error"
        )

ClosedRange : {min: Nat, max: Nat}

buildRangePair : Str -> Result {a: ClosedRange, b: ClosedRange} [NotFound]
buildRangePair = \s -> s
    |> Str.split ","
    |> List.keepOks buildRange
    |> \l -> when l is
        [x, y] -> Ok {a: x, b: y}
        _ -> Err NotFound

buildRange : Str -> Result ClosedRange [NotFound]
buildRange = \s -> s
    |> Str.splitFirst "-"
    |> Result.map \r -> {min: toNumberOrZero r.before, max: toNumberOrZero r.after}

fullyContainsEitherWay : {a: ClosedRange, b: ClosedRange} -> Bool
fullyContainsEitherWay = \r -> (r.a.min <= r.b.min && r.b.max <= r.a.max) || (r.b.min <= r.a.min && r.a.max <= r.b.max)

process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.keepOks buildRangePair
    |> List.countIf fullyContainsEitherWay
    |> Num.toStr

toNumberOrZero = \s ->
    when Str.toNat s is
        Ok number -> number
        Err _ -> 0
