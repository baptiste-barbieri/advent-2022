app "advent-00"
    packages { pf: "/home/baptistebarbieri/roc/roc_nightly-linux_x86_64-2022-11-28-patch/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Task.{ await, Task }, pf.File, pf.Path, pf.Stderr, pf.Process]
    provides [main] to pf

main : Task {} []
main =
    task =
        path = Path.fromStr "input.txt"
        contents <- await (File.readUtf8 path)
        process contents
        |> Num.toStr
        |> Stdout.line

    Task.attempt task \result ->
        when result is
            Ok _ -> Stdout.line "Successfully did stuff"
            Err err ->
                msg =
                    when err is
                        FileReadErr _ _ -> "Error reading file"
                        _ -> "Uh oh, there was an error!"

                {} <- Stderr.line msg |> Task.await
                Process.exit 1

process = \content ->
    content
    |> Str.split "\n"
    |> List.map Str.toI32
    |> List.walk 0 addNumberOrZero

addNumberOrZero = \number, result ->
    when result is
        Ok other -> number + other
        Err _ -> number
