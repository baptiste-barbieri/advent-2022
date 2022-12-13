app "advent-12-2"
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

Cell : {height: Nat, step: Nat}
Matrix : {cells: List Cell, width: Nat, start: Nat, highest: Nat}

chara = 97
charz = 122
maxNat = 10000000

parseGrid : Str -> Matrix
parseGrid = \s ->
    width = Str.splitFirst s "\n"
        |> Result.map (\r -> Str.countGraphemes r.before) |> Result.withDefault (Str.countGraphemes s)
    raw = Str.replaceEach s "\n" "" |> Result.withDefault ""
    end = Str.graphemes raw
        |> List.findFirstIndex \c -> c == "E"
        |> Result.withDefault 0
    start = Str.graphemes raw
        |> List.findFirstIndex \c -> c == "S"
        |> Result.withDefault 0
    grid = raw
        |> Str.toScalars
        |> List.map \c ->
            if c == 69 then {height: Num.toNat charz, step: 0}
            else if c == 83 then {height: Num.toNat chara, step: maxNat}
            else {height: Num.toNat c, step: maxNat}
    {cells: grid, width: width, start: start, highest: end}

indexAbove : Nat, Nat -> Result Nat [Top]*
indexAbove = \i, w ->
    if i < w then Err Top
    else Ok (i - w)

indexBelow : Nat, Nat -> Result Nat []*
indexBelow = \i, w ->
    Ok (i + w)

indexLeft : Nat, Nat -> Result Nat [Left]*
indexLeft = \i, w ->
    if (i % w) == 0 then Err Left
    else Ok (i - 1)

indexRight : Nat, Nat -> Result Nat [Right]*
indexRight = \i, w ->
    if (i % w) == ((w-1) % w) then Err Right
    else Ok (i + 1)

getNeighbors : Matrix, Nat -> List Cell
getNeighbors = \m, i ->
    [indexAbove i m.width, indexBelow i m.width, indexLeft i m.width, indexRight i m.width]
        |> List.keepOks \n -> n
        |> List.keepOks \n -> List.get m.cells n

makePath : Matrix -> Matrix
makePath = \m ->
    cells = m.cells
    if (countSteps m) < maxNat then m
    else
        makePath {
            cells: (List.mapWithIndex cells \c, i ->
                (getNeighbors m i)
                |> List.keepIf \cell -> cell.height <= (c.height + 1)
                |> List.map \cell -> cell.step + 1
                |> List.append c.step
                |> List.min
                |> Result.withDefault c.step
                |> \s -> {step: s, height: c.height}),
            width: m.width,
            start: m.start,
            highest: m.highest
        }

countSteps : Matrix -> Nat
countSteps = \m ->
    List.get m.cells m.start
        |> Result.map \cell -> cell.step
        |> Result.withDefault 0


score : Matrix -> Nat
score = \m ->
    m.cells
        |> List.keepIf \cell -> cell.height == chara
        |> List.map \cell -> cell.step
        |> List.min
        |> Result.withDefault 0

process : Str -> Str
process = \content ->
    content
    |> parseGrid
    |> makePath
    |> score
    |> Num.toStr
