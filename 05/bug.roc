app "advent-05-1"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ Task }]
    provides [main] to pf


main : Task {} []
main =
    prepareEmptyLists 3 0
        |> List.len
        |> Num.toStr
        |> Stdout.line

# Should be prepareEmptyLists : Nat -> List (List *)
prepareEmptyLists : Nat, Nat -> List List *
prepareEmptyLists = \capacity, quantity ->
    List.withCapacity capacity
    |> List.repeat quantity

expect prepareEmptyLists 0 3 == []
expect prepareEmptyLists 3 0 == [[], [], []]
expect prepareEmptyLists 3 3 == [[], [], []]
