app "advent-08-1"
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

Matrix : List (List Nat)
Coords : {x: Nat, y: Nat}

visibleFromTop : Matrix, Coords -> Nat
visibleFromTop = \m, c ->
    List.takeFirst m c.y
    |> List.map (\row -> List.get row c.x
        |> Result.withDefault 0)
    |> listMax

visibleFromBottom : Matrix, Coords -> Nat
visibleFromBottom = \m, c ->
    List.takeLast m ((List.len m) - c.y - 1)
    |> List.map (\row -> List.get row c.x
        |> Result.withDefault 0)
    |> listMax

visibleFromLeft : Matrix, Coords -> Nat
visibleFromLeft = \m, c ->
    List.get m c.y
    |> Result.map (\row -> List.takeFirst row c.x
        |> listMax)
    |> Result.withDefault 0

visibleFromRight : Matrix, Coords -> Nat
visibleFromRight = \m, c ->
    List.get m c.y
    |> Result.map (\row -> List.takeLast row ((List.len row) - c.x - 1)
        |> listMax)
    |> Result.withDefault 0

visible : Nat, Matrix, Coords -> Bool
visible = \cell, m, c -> cell > listMin [(visibleFromTop m c), (visibleFromBottom m c), (visibleFromLeft m c), (visibleFromRight m c)]

expect visible 2 [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 0, y: 0} == Bool.true
expect visibleFromTop [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 0, y: 0} == 0
expect visibleFromLeft [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 0, y: 0} == 0
expect visibleFromRight [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 2, y: 2} == 0
expect visibleFromRight [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 2, y: 1} == 0
expect visibleFromBottom [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 2, y: 2} == 0
expect visibleFromBottom [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 1, y: 2} == 0
expect visibleFromTop [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 1, y: 1} == 1
expect visibleFromLeft [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 1, y: 1} == 1
expect visibleFromRight [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 1, y: 1} == 1
expect visibleFromBottom [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 1, y: 1} == 1
expect visible 2 [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 1, y: 1} == Bool.true
expect visible 1 [[1, 1, 1], [1, 1, 1], [1, 1, 1]] {x: 1, y: 1} == Bool.false
expect visible 2 [[1, 2, 1], [2, 1, 2], [1, 2, 1]] {x: 1, y: 1} == Bool.false

makeVisibilityMatrix = \m -> List.mapWithIndex m \row, y -> List.mapWithIndex row \cell, x -> visible cell m {x: x, y: y}

expect makeVisibilityMatrix [[1, 1, 1], [1, 1, 1], [1, 1, 1]] == [[Bool.true, Bool.true, Bool.true], [Bool.true, Bool.false, Bool.true], [Bool.true, Bool.true, Bool.true]]
expect makeVisibilityMatrix [[1, 2, 1], [2, 1, 2], [1, 2, 1]] == [[Bool.true, Bool.true, Bool.true], [Bool.true, Bool.false, Bool.true], [Bool.true, Bool.true, Bool.true]]


process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.map (\s -> Str.graphemes s |> List.map (\n -> toNumberOrZero n |> \i -> i+1))
    |> makeVisibilityMatrix
    |> List.joinMap \l -> l
    |> List.countIf \b -> b
    |> Num.toStr

toNumberOrZero = \s ->
    when Str.toNat s is
        Ok number -> number
        Err _ -> 0
