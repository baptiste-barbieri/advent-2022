app "advent-13-2"
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

Packet : [IntPacket Nat, SubPacket (List Packet)]

changeAt : List a, Nat, (a -> a) -> List a
changeAt = \l, i, f ->
    List.get l i
    |> Result.map \e -> List.set l i (f e)
    |> Result.withDefault l

splitOuterComma : Str -> List Str
splitOuterComma = \s ->
    Str.graphemes s
        |> List.walk {level: 0, split: [""]} \acc, e ->
            if e == "," && acc.level == 0 then {level: 0, split: List.append acc.split ""}
            else {
                level: acc.level + (if e == "]" then -1 else if e == "[" then 1 else 0),
                split: changeAt acc.split ((List.len acc.split) - 1) (\c -> Str.concat c e)
            }
        |> \r -> r.split

expect splitOuterComma "[1],[2,3,4]" == ["[1]","[2,3,4]"]

parsePacket : Str -> Packet
parsePacket = \raw ->
    s = Str.trim raw
    asNum = Str.toNat s
    if s == "[]" then SubPacket []
    else if Result.isOk asNum then (Result.withDefault asNum 0 |> IntPacket)
    else s
        |> Str.replaceFirst "[" ""
        |> Result.try \a -> Str.replaceLast a "]" ""
        |> Result.withDefault ""
        |> splitOuterComma
        |> List.map parsePacket
        |> SubPacket

expect parsePacket "[]" == SubPacket []
expect parsePacket "[[]]" == SubPacket [SubPacket []]
expect parsePacket "[1]" == SubPacket [IntPacket 1]
expect parsePacket "[1,2]" == SubPacket [IntPacket 1, IntPacket 2]
expect parsePacket "[1,[2]]" == SubPacket [IntPacket 1, SubPacket [IntPacket 2]]
expect parsePacket "[1,[2],3]" == SubPacket [IntPacket 1, SubPacket [IntPacket 2], IntPacket 3]
expect parsePacket "[[1],[2,3,4]]" == SubPacket [SubPacket [IntPacket 1], SubPacket [IntPacket 2, IntPacket 3, IntPacket 4]]

correctOrder : Packet, Packet -> [LT, EQ, GT]
correctOrder = \left, right ->
    when left is
        IntPacket l ->
            when right is
                IntPacket r -> Num.compare l r
                SubPacket _ -> correctOrder (SubPacket [left]) right
        SubPacket subl ->
            when right is
                IntPacket _ -> correctOrder left (SubPacket [right])
                SubPacket subr -> List.map2 subl subr correctOrder
                    |> List.dropIf \e -> e == EQ
                    |> List.first
                    |> Result.withDefault (Num.compare (List.len subl) (List.len subr))

expect correctOrder (SubPacket []) (SubPacket [IntPacket 3]) == LT
expect correctOrder (SubPacket []) (SubPacket [SubPacket []]) == LT
expect correctOrder (SubPacket [IntPacket 1, SubPacket [IntPacket 2], IntPacket 3]) (SubPacket [SubPacket []]) == GT
expect correctOrder (SubPacket [IntPacket 1, SubPacket [IntPacket 2], IntPacket 3]) (SubPacket [SubPacket []]) == GT
expect correctOrder (SubPacket [SubPacket [IntPacket 1], SubPacket [IntPacket 2, IntPacket 3, IntPacket 4]]) (SubPacket [SubPacket [IntPacket 1], SubPacket [IntPacket 4]]) == LT

divider1 = SubPacket [SubPacket [IntPacket 2]]
divider2 = SubPacket [SubPacket [IntPacket 6]]

expect List.sortWith [IntPacket 2, IntPacket 1] correctOrder == [IntPacket 1, IntPacket 2]
expect List.sortWith [SubPacket [IntPacket 2], SubPacket [IntPacket 1]] correctOrder == [SubPacket [IntPacket 1], SubPacket [IntPacket 2]]

process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.dropIf \s -> (Str.countGraphemes s) == 0
    |> List.map parsePacket
    |> List.concat [divider1, divider2]
    |> List.sortWith correctOrder
    |> List.mapWithIndex \e, i -> if (e == divider1 || e == divider2) then i + 1 else 0
    |> List.dropIf \e -> e == 0
    |> List.walk 1 Num.mul
    |> Num.toStr
