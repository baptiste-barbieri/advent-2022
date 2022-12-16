app "advent-15-1"
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

Sensor : {x: I32, y: I32, bx: I32, by: I32}
SensorVisibility : {x: I32, y: I32, range: Nat}

parseSensors: Str -> List Sensor
parseSensors = \s ->
    Str.split s "\n"
    |> List.keepOks \line ->
        Str.split line "="
        |> List.joinMap \fragment -> Str.split fragment ","
        |> List.joinMap \fragment -> Str.split fragment ":"
        |> List.keepOks Str.toI32
        |> \l -> when l is
            [x, y, bx, by] -> Ok {x: x, y: y, bx: bx, by: by}
            _ -> Err InvalidSensorString

uncheckLine : List Sensor, I32 -> Nat
uncheckLine = \sensors, y ->
    allXCoords = List.joinMap sensors \s -> [s.x, s.bx]
    minX = List.min allXCoords |> Result.withDefault 0
    maxX = List.max allXCoords |> Result.withDefault 0
    beaconXs = List.keepIf sensors (\s -> s.by == y) |> List.map (\s -> s.bx) |> Set.fromList
    sensorVisibilities = List.map sensors \s -> {x: s.x, y: s.y, range: distanceCovered s}
    maxVisibility = List.map sensorVisibilities (\s -> s.range) |> List.max |> Result.withDefault 0
    List.range {start: At (minX - Num.toI32 maxVisibility), end: At (maxX + Num.toI32 maxVisibility)}
    |> List.dropIf \i -> Set.contains beaconXs i
    |> List.countIf \i -> visible sensorVisibilities i y

visibleFrom : SensorVisibility, I32, I32 -> Bool
visibleFrom = \sensor, x, y ->
    sensor.range >= distance sensor.x sensor.y x y

distance : I32, I32, I32, I32 -> Nat
distance = \x, y, a, b ->
    Num.toNat ((Num.abs (x - a)) + (Num.abs (y - b)))

distanceCovered : Sensor -> Nat
distanceCovered = \s ->
    distance s.x s.y s.bx s.by

visible : List SensorVisibility, I32, I32 -> Bool
visible = \sensors, x, y ->
    List.any sensors \sensor -> visibleFrom sensor x y

process : Str -> Str
process = \content ->
    content
    |> parseSensors
    |> uncheckLine 2000000
    |> Num.toStr
