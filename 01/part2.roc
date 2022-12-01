app "advent-01-2"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path]
    provides [main] to pf

main : Task {} []
main =
    task =
        path = Path.fromStr "input1.txt"
        contents <- await (File.readUtf8 path)
        process contents
            |> Num.toStr
            |> Stdout.line

    Task.attempt task \result ->
        Stdout.line (when result is
            Ok _ -> "Successfully did stuff"
            Err err -> when err is
                FileReadErr _ _ -> "Error reading file"
                _ -> "Unknown error"
        )

process = \content ->
    content
    |> Str.split "\n\n"
    |> List.map \e -> Str.split e "\n"
        |> List.map Str.toI32
        |> List.walk 0 addNumberOrZero
    |> List.sortDesc
    |> List.takeFirst 3
    |> List.sum

addNumberOrZero = \number, result ->
    when result is
        Ok other -> number + other
        Err _ -> number
