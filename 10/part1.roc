app "advent-10-1"
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


makeSigStr : List Instruction -> List I32
makeSigStr = \instructions ->
    List.walk instructions {a: [1], prev: 1} \l, m ->
        when m is
            Noop -> {a: (List.append l.a l.prev), prev: l.prev}
            Add x -> {a: (List.concat l.a [l.prev, l.prev]), prev: l.prev + x}
    |> \r -> r.a


Instruction : [Add I32, Noop]

makeInstructions : Str -> Instruction
makeInstructions = \s ->
    when Str.split s " " |> List.get 1 is
        Ok intStr -> Str.toI32 intStr |> Result.map Add |> Result.withDefault Noop
        Err _ -> Noop

processSignal : List I32 -> I32
processSignal = \l ->
    List.range { start: At 20, end: At (List.len l), step: 40 }
    |> List.keepOks (\i -> List.get l i |> Result.map \s -> Num.mul (Num.toI32 i) s)
    |> List.walk 0 Num.add

process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.map makeInstructions
    |> makeSigStr
    |> processSignal
    |> Num.toStr
