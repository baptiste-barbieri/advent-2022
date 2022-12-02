app "advent-02-1"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

Playable : [Rock, Paper, Scissors]

letterToPlayable : Str -> Playable
letterToPlayable = \letter ->
    when letter is
        "A" | "X" -> Rock
        "B" | "Y" -> Paper
        "C" | "Z" -> Scissors
        _ -> Rock # should not happen

playablePoints : Playable -> Num*
playablePoints = \playable ->
    when playable is
        Rock -> 1
        Paper -> 2
        Scissors -> 3

playablePretty : Playable -> Str
playablePretty = \playable ->
    when playable is
        Rock -> "rock"
        Paper -> "paper"
        Scissors -> "scissors"

outcome = \me, op ->
    when me is
        Rock -> when op is
            Rock -> 3
            Paper -> 0
            Scissors -> 6
        Paper -> when op is
            Rock -> 6
            Paper -> 3
            Scissors -> 0
        Scissors -> when op is
            Rock -> 0
            Paper -> 6
            Scissors -> 3

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

process = \content ->
    content
    |> Str.split "\n"
    |> List.map \e -> Str.split e " "
        |> List.map letterToPlayable
        |> \l -> when l is
            [op, me] -> (playablePoints me) + (outcome me op)
            _ -> 0
    |> List.walk 0 \a, b -> a + b
    |> Num.toStr

expect process "A Y" != "0"
expect process "A Y" == "8"
expect process "A Y\nB X\n" == "9"