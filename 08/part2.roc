app "advent-08-2"
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

listMax: List Nat -> Nat
listMax = \l -> List.walk l 0 \a, b -> if a > b then a else b

#                           vvv- no Num.maxNat
listMin: List Nat -> Nat
listMin = \l -> List.walk l 100 \a, b -> if a < b then a else b

visibleTrees: List Nat, Nat -> Nat
visibleTrees = \l, cell ->
    List.walkUntil l 0 \s, e -> if cell > e then Continue (s + 1) else Break (s + 1)

Matrix : List (List Nat)
Coords : {x: Nat, y: Nat}

visibleFromTop : Nat, Matrix, Coords -> Nat
visibleFromTop = \cell, m, c ->
    List.takeFirst m c.y
    |> List.map (\row -> List.get row c.x
        |> Result.withDefault 0)
    |> List.reverse
    |> visibleTrees cell

visibleFromBottom : Nat, Matrix, Coords -> Nat
visibleFromBottom = \cell, m, c ->
    List.takeLast m ((List.len m) - c.y - 1)
    |> List.map (\row -> List.get row c.x
        |> Result.withDefault 0)
    |> visibleTrees cell

visibleFromLeft : Nat, Matrix, Coords -> Nat
visibleFromLeft = \cell, m, c ->
    List.get m c.y
    |> Result.map (\row -> List.takeFirst row c.x
        |> List.reverse |> visibleTrees cell)
    |> Result.withDefault 0

visibleFromRight : Nat, Matrix, Coords -> Nat
visibleFromRight = \cell, m, c ->
    List.get m c.y
    |> Result.map (\row -> List.takeLast row ((List.len row) - c.x - 1)
        |> visibleTrees cell)
    |> Result.withDefault 0

scenic : Nat, Matrix, Coords -> Nat
scenic = \cell, m, c -> List.product [(visibleFromTop cell m c), (visibleFromBottom cell m c), (visibleFromLeft cell m c), (visibleFromRight cell m c)]

makeScenicMatrix = \m -> List.mapWithIndex m \row, y -> List.mapWithIndex row \cell, x -> scenic cell m {x: x, y: y}


process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.map (\s -> Str.graphemes s |> List.map (\n -> toNumberOrZero n |> \i -> i+1))
    |> makeScenicMatrix
    |> List.joinMap \l -> l
    |> List.max
    |> Result.withDefault 0
    |> Num.toStr

toNumberOrZero = \s ->
    when Str.toNat s is
        Ok number -> number
        Err _ -> 0
