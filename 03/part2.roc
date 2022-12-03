app "advent-03-2"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

intersectLists : List a, List a -> List a | a has Bool.Eq
intersectLists = \x, y->
    Set.intersection (Set.fromList x) (Set.fromList y)
    |> Set.toList

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
    |> List.map Str.graphemes
    |> findBadges
    |> List.map priority
    |> sumList
    |> Num.toStr


findBadges : List (List Str) -> List Str
findBadges = \l ->
    when l is
        [a, b, c, ..] -> List.concat (intersectLists a b |> intersectLists c) (findBadges (List.drop l 3))
        _ -> []
