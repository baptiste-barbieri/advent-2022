app "advent-02-2"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

Playable : [Rock, Paper, Scissors]
Outcome : [Draw, Win, Loss]

letterToPlayable : Str -> Playable
letterToPlayable = \letter ->
    when letter is
        "A" -> Rock
        "B" -> Paper
        "C" -> Scissors
        _ -> Rock # should not happen

letterToOutcome : Str -> Outcome
letterToOutcome = \letter ->
    when letter is
        "X" -> Loss
        "Y" -> Draw
        "Z" -> Win
        _ -> Draw # should not happen

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

outcomePoints : Outcome -> Num*
outcomePoints = \outcome ->
    when outcome is
        Win -> 6
        Draw -> 3
        Loss -> 0

played : Playable, Outcome -> Playable
played = \op, outcome ->
    when op is
        Rock -> when outcome is
            Win -> Paper
            Draw -> Rock
            Loss -> Scissors
        Paper -> when outcome is
            Win -> Scissors
            Draw -> Paper
            Loss -> Rock
        Scissors -> when outcome is
            Win -> Rock
            Draw -> Scissors
            Loss -> Paper

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
        |> \l -> when l is
            [op, outcome] -> (outcomePoints (letterToOutcome outcome)) + playablePoints (played (letterToPlayable op) (letterToOutcome outcome))
            _ -> 0
    |> List.walk 0 \a, b -> a + b
    |> Num.toStr

expect process "A Y" == "4"
expect process "A Y\nB X\n" == "5"