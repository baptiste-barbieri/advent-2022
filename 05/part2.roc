app "advent-05-2"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

Stack : List Str
Stacks : List Stack
Move : {qte: Nat, source: Nat, dest: Nat}

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

preProcess : Str -> {stacks: List Str, moves: List Str}
preProcess = \content ->
    content
    |> Str.splitFirst "\n\n"
    |> Result.map \r -> {stacks: Str.split r.before "\n", moves: Str.split r.after "\n"}
    |> Result.withDefault {stacks: [], moves: []}

parseMove : Str -> Move
parseMove = \s -> s
    |> Str.split " "
    |> List.keepOks Str.toNat
    |> \l -> when l is
        [q, f, t] -> {qte: q, source: f - 1, dest: t - 1}
        _ -> {qte: 0, source: 0, dest: 0}

expect parseMove "move 3 from 4 to 1" == {qte: 3, source: 3, dest: 0}

# Due to bug, fix move by making it a series of one-crate moves
# that moves all creates to a third pile before moving them again to the destination pile
fixMove : Move -> List Move
fixMove = \m ->
    List.range 0 3
    |> List.findFirst \i -> i != m.source && i != m.dest
    |> Result.withDefault 4
    |> \other -> List.concat
        (List.repeat {qte: 1, source: m.source, dest: other} m.qte)
        (List.repeat {qte: 1, source: other, dest: m.dest} m.qte)

# This function assumes single digit stack ids
parseCrates : List Str -> Stacks
parseCrates = \l -> l
    |> List.reverse
    |> List.split 1
    |> \r ->
        List.walk r.others (prepareEmptyLists (List.first r.before |> Result.withDefault "") (List.len r.others)) \s, e ->
            List.mapWithIndex s \stack, i ->
                List.concat stack (crateAt e i)

crateAt : Str, Nat -> List Str
crateAt = \s, i ->
    s
    |> Str.graphemes
    |> List.get (1 + i*4)
    |> Result.map List.single
    |> Result.withDefault []
    |> List.dropIf \c -> c == " "

expect crateAt "[Z] [M] [P]" 0 == ["Z"]
expect crateAt "[Z] [M] [P]" 1 == ["M"]
expect crateAt "[Z] [M] [P]" 2 == ["P"]
expect crateAt "[Z] [M]    " 2 == []
expect crateAt "[Z] [M] [P]" 3 == []

expect parseCrates [] == []
expect parseCrates [" 1   2   3 "] == [[], [], []]
expect parseCrates ["[Z] [M] [P]", " 1   2   3 "] == [["Z"], ["M"], ["P"]]
expect parseCrates ["[N] [C]    ", "[Z] [M] [P]", " 1   2   3 "] == [["Z", "N"], ["M", "C"], ["P"]]
expect parseCrates ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]", " 1   2   3 "] == [["Z", "N"], ["M", "C", "D"], ["P"]]

process : Str -> Str
process = \content ->
    content
    |> preProcess
    |> \r -> {stacks: parseCrates r.stacks, moves: r.moves |> List.map parseMove |> List.joinMap fixMove}
    |> moveCrates
    |> pickLasts
    |> Str.joinWith ""

pickLasts : Stacks -> List Str
pickLasts = \stacks ->
    List.mapTry stacks List.last
    |> Result.withDefault []

expect pickLasts [["C", "A"], ["B"]] == ["A", "B"]

moveCrates : {stacks: Stacks, moves: List Move} -> Stacks
moveCrates = \r -> List.walk r.moves r.stacks doOneMove

expect moveCrates {stacks: [["A", "B"], []], moves: [{qte: 1, source: 0, dest: 1}]} == [["A"], ["B"]]
expect moveCrates {stacks: [["A", "B"], []], moves: [{qte: 1, source: 0, dest: 1}, {qte: 1, source: 0, dest: 1}]} == [[], ["B", "A"]]
expect moveCrates {stacks: [["A", "B"], [], ["C"]], moves: [{qte: 1, source: 0, dest: 1}, {qte: 1, source: 0, dest: 1}]} == [[], ["B", "A"], ["C"]]
expect moveCrates {stacks: [["Z", "N"], ["M", "C", "D"], ["P"]], moves: [{qte: 1, source: 1, dest: 0}]} == [["Z", "N", "D"], ["M", "C"], ["P"]]
# expect moveCrates {stacks: [["Z", "N"], ["M", "C", "D"], ["P"]], moves: [{qte: 1, source: 1, dest: 0}, {qte: 3, source: 0, dest: 2}]} == [[], ["M", "C"], ["P", "D", "N", "Z"]]
# expect moveCrates {stacks: [["Z", "N"], ["M", "C", "D"], ["P"]], moves: [{qte: 1, source: 1, dest: 0}, {qte: 3, source: 0, dest: 2}, {qte: 2, source: 1, dest: 0}, {qte: 1, source: 0, dest: 1}]} == [["C"], ["M"], ["P", "D", "M", "Z"]]

getOrEmpty : List (List a), Nat -> List a
getOrEmpty = \l, i ->
    List.get l i |> Result.withDefault []

concatAt : List (List a), Nat, List a -> List (List a)
concatAt = \l, i, e ->
    getOrEmpty l i
    |> List.concat e
    |> \r -> List.set l i r

# bugged ?
# expect concatAt [["C"]] 0 ["A", "B"] == [["C", "A", "B"]]

doOneMove : Stacks, Move -> Stacks
doOneMove = \stacks, move ->
    stacks
    |> concatAt move.dest (getOrEmpty stacks move.source |> List.takeLast move.qte |> List.reverse)
    |> List.set move.source (getOrEmpty stacks move.source |> List.reverse |> List.drop move.qte |> List.reverse)

expect doOneMove [["A", "B"], []] {qte: 1, source: 0, dest: 1} == [["A"], ["B"]]
expect doOneMove [["A", "B"], []] {qte: 2, source: 0, dest: 1} == [[], ["B", "A"]]
# expect doOneMove [["A", "B"], ["C"]] {qte: 2, source: 0, dest: 1} == [[], ["C", "B", "A"]]
expect doOneMove [["A", "B"], ["C"]] {qte: 2, source: 0, dest: 1} == [[], ["C", "B", "B"]]
expect doOneMove [["A"], ["B"]] {qte: 1, source: 0, dest: 1} == [[], ["B", "A"]]
expect doOneMove [["A"], ["B"]] {qte: 1, source: 1, dest: 0} == [["A", "B"], []]
expect doOneMove [["A"], ["B"], ["C"]] {qte: 1, source: 1, dest: 0} == [["A", "B"], [], ["C"]]
# expect doOneMove [["Z", "N", "D"], ["M", "C"], ["P"]] {qte: 3, source: 0, dest: 2} == [[], ["M", "C"], ["P", "D", "N", "Z"]]

expect (List.takeLast ["Z", "N", "D"] 3 |> List.reverse) == ["D", "N", "Z"]
expect (List.concat ["A"] (List.takeLast ["Z", "N", "D"] 3 |> List.reverse))== ["A", "D", "N", "Z"]
expect List.concat ["Z", "N"] ["D"] == ["Z", "N", "D"]

prepareEmptyLists : Str, Nat -> List (List *)
prepareEmptyLists = \stacks, capacity ->
    capacity
    |> List.withCapacity
    |> List.repeat (
        Str.replaceEach stacks " " ""
        |> Result.withDefault ""
        |> Str.countGraphemes
        )

expect prepareEmptyLists "" 3 == []
expect prepareEmptyLists " 1   3   4 " 3 == [[], [], []]
