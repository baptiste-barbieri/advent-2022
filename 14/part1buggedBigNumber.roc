app "advent-13-1"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

main : Task {} []
main =
    task =
        path = Path.fromStr "example"
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

fixedPoint = \e, f ->
    r = f e
    if r == e then r
    else fixedPoint r f

Coords : {x: Nat, y: Nat}
Cell : {coords: Coords, content: CellContent}
CellContent : [Rock, Air, Sand]
Cave : List Cell

OrderedPair a : {first: a, second: a}

pairs : List Coords -> List (OrderedPair Coords)
pairs = \l ->
    dbg "Pairing"
    dbg l
    List.map2 l (List.dropFirst l) \x, y -> {first: x, second: y}

least = \a, b -> if a <= b then a else b
most = \a, b -> if a >= b then a else b

coordsInPath : OrderedPair Coords -> Result (List Coords) [PointsNotAligned]
coordsInPath = \orderedPair ->
    from = orderedPair.first
    to = orderedPair.second
    (if from.x == to.x then
        Ok (List.range { start: At (least from.y to.y), end: At (most from.y to.y) }
            |> List.map \y -> {x: from.x, y: y})
    else if from.y == to.y then
        Ok (List.range { start: At (least from.x to.x), end: At (most from.x to.x) }
            |> List.map \x -> {x: x, y: from.y})
    else Err PointsNotAligned)


processCellString = \cellString ->
    cell = Str.split cellString ","
        |> List.keepOks Str.toNat
        |> \r -> when r is
            [a, b] -> [{x: a, y: b}]
            _ -> []
    dbg cell
    cell

createCave : Str -> Cave
createCave = \s ->
    cave = Str.split s "\n"
        |> List.joinMap (\pathString ->
            Str.split pathString " -> "
            |> List.joinMap processCellString
            |> pairs
            |> List.keepOks coordsInPath
            |> List.joinMap \a -> a)
        |> List.map \coords -> {coords: coords, content: Rock}
    dbg cave
    cave

placeSand : Cave, Coords -> Cave
placeSand = \cave, coords ->
    List.append cave {coords: coords, content: Sand}

fallOneSand : Cave -> Cave
fallOneSand = \cave ->
    fixedPoint {x: 500, y:0} \coords -> tickOneSand cave coords
    |> \coords -> placeSand cave coords

tickOneSand : Cave, Coords -> Coords
tickOneSand = \cave, coords ->
    [cellBelow, cellLeftBelow, cellRightBelow]
    |> List.map \cellGetter -> cellGetter cave coords
    |> List.findFirst (\cell -> cell.content == Air)
    |> Result.map \cell -> cell.coords
    |> Result.withDefault coords

cellBelow : Cave, Coords -> Cell
cellBelow = \cave, coords ->
    List.findFirst cave \cell -> cell.coords == {x: coords.x - 1, y: coords.y}
    |> Result.withDefault {coords: coords, content: Air}

cellLeftBelow : Cave, Coords -> Cell
cellLeftBelow = \cave, coords ->
    List.findFirst cave \cell -> cell.coords == {x: coords.x - 1, y: coords.y - 1}
    |> Result.withDefault {coords: coords, content: Air}

cellRightBelow : Cave, Coords -> Cell
cellRightBelow = \cave, coords ->
    List.findFirst cave \cell -> cell.coords == {x: coords.x - 1, y: coords.y + 1}
    |> Result.withDefault {coords: coords, content: Air}

countSands: Cave -> Nat
countSands = \cave ->
    List.countIf cave \cell -> cell.content == Sand

process : Str -> Str
process = \content ->
    content
    |> createCave
    |> \_ -> 0
    |> Num.toStr
