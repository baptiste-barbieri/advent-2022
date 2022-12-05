app "advent-05-1"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf


main : Task {} []
main =
    prepareEmptyLists " 1 2 3 " 3
        |> List.len
        |> Num.toStr
        |> Stdout.line

# Should be prepareEmptyLists : Str, Nat -> List (List *)
prepareEmptyLists : Str, Nat -> List List *
prepareEmptyLists = \stacks, capacity ->
    capacity
    |> List.withCapacity
    |> List.repeat (
        Str.replaceEach stacks " " ""
        |> Result.withDefault ""
        |> Str.countGraphemes
        )

expect 1 == 1
expect prepareEmptyLists "" 3 == []
expect prepareEmptyLists " 1 3 4 " 3 == [[], [], []]
