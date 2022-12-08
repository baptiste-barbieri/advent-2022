app "advent-07-1"
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

Line : [Back, Cd Str, Noop, File Str Nat, Root]

parseLine : Str -> Line
parseLine = \s ->
    if Str.startsWith s "$ cd .." then
        Back
    else if Str.startsWith s "$ cd /" then
        Root
    else if Str.startsWith s "$ cd" then
        Cd (s |> Str.splitLast " " |> Result.map (\r -> r.after) |> Result.withDefault "")
    else if Str.startsWith s "dir" then
        Noop
    else if Str.startsWith s "$ ls" then
        Noop
    else
        File
            (s |> Str.splitFirst " " |> Result.map (\r -> r.after) |> Result.withDefault "")
            (s |> Str.splitFirst " " |> Result.map (\r -> r.before) |> Result.try Str.toNat |> Result.withDefault 0)

addDirToLines = \lines -> List.walk lines (List.len lines |> List.withCapacity |> List.append (Pair (List.single "/") Cd)) \l, s ->
     currDir = List.last l
         |> Result.try (\p -> when p is
             Pair dir _ -> Ok dir
             _ -> Err NotFound)
         |> Result.withDefault ["/"]
     when s is
         Back -> List.append l (Pair (List.dropLast currDir) Back)
         Root -> List.append l (Pair ["/"] Root)
         Cd dir -> List.append l (Pair (List.append currDir dir) Cd)
         File filename size -> List.append l (Pair currDir (File (List.append currDir filename) size))
         _ -> l

groupByDir = \lines -> List.walk lines Dict.empty \dict, ds ->
    when ds is
        Pair dir size -> Dict.update dict dir (\r ->
            when r is
                Present a -> Present (a + size)
                Missing -> Present size)
        _ -> dict

deduplicate = \lines -> List.walk lines (List.len lines |> List.withCapacity) \l, item ->
    if List.contains l item then l else List.append l item

process : Str -> Str
process = \content ->
    content
    |> Str.split "\n"
    |> List.map parseLine
    |> addDirToLines
    |> List.joinMap \line -> when line is
        Pair dirs (File _filename size) -> List.map dirs \dir -> Pair dir size
        _ -> []
    |> deduplicate
    |> groupByDir
    |> Dict.values
    |> List.keepIf \size -> size <= 100000
    |> List.walk 0 Num.add
    |> Num.toStr
