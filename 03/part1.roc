app "advent-03-1"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

splitInHalf : List a -> {before: List a, others: List a}
splitInHalf = \l ->
    List.split l ((List.len l) // 2)

expect splitInHalf [1, 2, 3, 4] == {before: [1, 2], others: [3, 4]}

intersectLists : {x: List a, y: List a} -> List a | a has Bool.Eq
intersectLists = \r ->
    Set.intersection (Set.fromList r.x) (Set.fromList r.y)
    |> Set.toList

# Those test crash the compiler for some reason
# expect intersectLists {a: [1, 2], b: []} == []
# expect intersectLists {a: [1, 2], b: [1]} == [1]
# expect intersectLists {a: [1, 2], b: [1, 2]} == [1, 2]
# expect intersectLists {a: [1, 2], b: [1, 2, 3]} == [1, 2]

unfoldSingleStr = \l ->
    when l is
        [a] -> a
        _ -> ""

chara = 97
charz = 122
charA = 65
charZ = 90
charapoints = 1
charApoints = 27

priority : Str -> U32
priority = \c -> c
    |> Str.toScalars
    |> \l -> when l is
        [u] -> if chara <= u && u <= charz then u - chara + charapoints
            else if charA <= u && u <= charZ then u - charA + charApoints
            else 0
        _ -> 0

sumList = \l -> List.walk l 0 \a, b -> a + b

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

process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.map \s ->
        Str.graphemes s
        |> splitInHalf
        |> \r -> {x: r.before, y: r.others}
        |> intersectLists
        |> unfoldSingleStr
        |> priority
    |> sumList
    |> Num.toStr
