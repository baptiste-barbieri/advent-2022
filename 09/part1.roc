app "advent-09-1"
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

makeMoves : Str -> Str
makeMoves = \s ->
    when (Str.split s " ") is
        [m, q] -> Str.repeat m (toNumberOrZero q)
        _ -> ""

Coords : {x: I32, y: I32}

makeHeadPositions : Str -> List Coords
makeHeadPositions = \s -> Str.graphemes s
    |> List.walk [] \l, m ->
        previousPos = List.last l |> Result.withDefault {x: 0, y: 0}
        when m is
            "U" -> List.append l ({x: previousPos.x, y: previousPos.y + 1})
            "L" -> List.append l ({x: previousPos.x + 1, y: previousPos.y})
            "D" -> List.append l ({x: previousPos.x, y: previousPos.y - 1})
            "R" -> List.append l ({x: previousPos.x - 1, y: previousPos.y})
            _ -> []

moveTail : Coords, Coords -> Coords
moveTail = \previousPos, h ->
    if ((Num.abs (previousPos.x - h.x) + Num.abs (previousPos.y - h.y)) > 2 ) then
        {x: Num.add previousPos.x (divAbsCeil (h.x - previousPos.x) 2), y: Num.add previousPos.y (divAbsCeil (h.y - previousPos.y) 2)}
    else
        {x: Num.add previousPos.x ((h.x - previousPos.x) // 2), y: Num.add previousPos.y ((h.y - previousPos.y) // 2)}

divAbsCeil = \a, b ->
    if (Num.isNegative a) != (Num.isNegative b) then
        (a // b) + (a % b)
    else
        Num.divCeil a b

expect divAbsCeil 1 2 == 1
expect divAbsCeil 2 2 == 1
expect divAbsCeil -1 2 == -1
expect divAbsCeil -2 2 == -1
expect (moveTail {x: 10, y: 5} {x: 10, y: 3}) == {x: 10, y: 4}
expect (moveTail {x: 10, y: 5} {x: 9, y: 5}) == {x: 10, y: 5}
expect (moveTail {x: 9, y: 5} {x: 10, y: 5}) == {x: 9, y: 5}
expect (moveTail {x: 10, y: 5} {x: 9, y: 3}) == {x: 9, y: 4}
expect (moveTail {x: 9, y: 3} {x: 10, y: 5}) == {x: 10, y: 4}

makeTailPositions : List Coords -> List Coords
makeTailPositions = \hs ->
    List.walk hs [{x: 0, y: 0}] \l, h ->
        previousPos = List.last l |> Result.withDefault {x: 0, y: 0}
        List.append l (moveTail previousPos h)


process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.map makeMoves
    |> Str.joinWith ""
    |> makeHeadPositions
    |> makeTailPositions
    |> Set.fromList
    |> Set.len
    |> Num.toStr

toNumberOrZero = \s ->
    when Str.toNat s is
        Ok number -> number
        Err _ -> 0
