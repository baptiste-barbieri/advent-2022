app "advent-10-2"
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
            Add x -> {a: (List.concat l.a [l.prev, l.prev+x]), prev: l.prev + x}
    |> \r -> r.a


Instruction : [Add I32, Noop]

makeInstructions : Str -> Instruction
makeInstructions = \s ->
    when Str.split s " " |> List.get 1 is
        Ok intStr -> Str.toI32 intStr |> Result.map Add |> Result.withDefault Noop
        Err _ -> Noop

processSignal : List I32 -> List Str
processSignal = \l ->
    List.mapWithIndex l \signal, index -> if Num.abs (signal - ((Num.toI32 index) % 40)) > 1 then "." else "#"

partitionList : List a, Nat -> List (List a)
partitionList = \l, n ->
    if (List.len l) >= n then
        List.split l n |> \r -> List.concat [r.before] (partitionList r.others n)
    else
        [l]

process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.map makeInstructions
    |> makeSigStr
    |> processSignal
    |> partitionList 40
    |> List.map \l -> Str.joinWith l ""
    |> Str.joinWith "\n"
