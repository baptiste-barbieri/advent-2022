app "advent-11-1"
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

Monkey : {items: List Nat, op: (Nat -> Nat), test: Nat, divisible: Nat, nonDivisible: Nat, inspections: Nat}

parseMonkey : Str -> Monkey
parseMonkey = \monkeyString ->
    Str.split monkeyString "\n" |> \monkeyLines ->
        {
            items: (List.get monkeyLines 1 # "Starting items: 84, 66, 62, 69, 88, 91, 91"
                |> Result.try (\s -> Str.split s ": " |> List.last)
                |> Result.withDefault ""
                |> Str.split ", "
                |> List.keepOks Str.toNat),
            op: (List.get monkeyLines 2 # "Operation: new = old * 11"
                |> Result.try (\s -> Str.split s " = " |> List.last) # "old * 11"
                |> Result.withDefault ""
                |> Str.split " "
                |> List.map \s ->
                    if s == "old" then Old
                    else if s == "*" then (Op \a, b -> a * b) # Bugged when using Num.mul ????
                    else if s == "+" then (Op \a, b -> a + b)
                    else Constant (Str.toNat s |> Result.withDefault 0)
                |> \l -> when l is
                    [Old, Op op, Old] ->
                        (\o -> op o o)
                    [Old, Op op, Constant c] ->
                        (\o -> op o c)
                    _ -> (\o -> o)
            ),
            test: (List.get monkeyLines 3 # "Test: divisible by 2"
                    |> Result.try (\s -> Str.split s "by " |> List.last)
                    |> Result.try Str.toNat
                    |> Result.withDefault 1),
            divisible: (List.get monkeyLines 4 # "If true: throw to monkey 4"
                |> Result.try (\s -> Str.split s "monkey " |> List.last)
                |> Result.try Str.toNat
                |> Result.withDefault 0),
            nonDivisible: (List.get monkeyLines 5 # "If false: throw to monkey 4"
                |> Result.try (\s -> Str.split s "monkey " |> List.last)
                |> Result.try Str.toNat
                |> Result.withDefault 0),
            inspections: 0
        }

removeItem : Monkey -> Monkey
removeItem = \m -> {items: List.dropLast m.items, op: m.op, test: m.test, divisible: m.divisible, nonDivisible: m.nonDivisible, inspections: m.inspections + 1}

addItem : Monkey, Nat -> Monkey
addItem = \m, item -> {items: List.append m.items item, op: m.op, test: m.test, divisible: m.divisible, nonDivisible: m.nonDivisible, inspections: m.inspections}

inspectOne : Monkey, Nat -> {target: Nat, worry: Nat}
inspectOne = \monkey, worry ->
    newWorry = (monkey.op worry) // 3
    {
        target: if ((newWorry % monkey.test) == 0) then monkey.divisible else monkey.nonDivisible,
        worry: newWorry
    }

throwItem : List Monkey, {source: Nat, target: Nat, worry: Nat}* -> Result (List Monkey) [NotFound]
throwItem = \l, t ->
    List.get l t.target
    |> Result.map \targetMonkey -> List.set l t.target (addItem targetMonkey t.worry)
    |> Result.try \monkeys ->
        List.get monkeys t.source
            |> Result.map \sourceMonkey -> List.set monkeys t.source (removeItem sourceMonkey)

round : List Monkey, Nat -> List Monkey
round = \monkeys, i ->
    List.get monkeys i
    |> Result.try (\monkey ->
        List.map monkey.items \item -> inspectOne monkey item
        |> List.walkTry monkeys \ms, throw -> (throwItem ms {source: i, target: throw.target, worry: throw.worry}))
    |> Result.withDefault monkeys

indices : List * -> List Nat
indices = \l -> List.mapWithIndex l \_, i -> i

identity : a -> a
identity = \a -> a

turns : List Monkey, Nat -> List Monkey
turns = \l, nb ->
    indices l
    |> List.repeat nb
    |> List.joinMap identity
    |> List.walk l \monkeys, index -> round monkeys index

score : List Monkey -> Nat
score = \monkeys ->
    List.map monkeys \m -> m.inspections
    |> List.sortDesc
    |> List.takeFirst 2
    |> List.walk 1 Num.mul

process : Str -> Str
process = \content ->
    content
    |> Str.split "\n\n"
    |> List.map parseMonkey
    |> turns 20
    |> score
    |> Num.toStr
