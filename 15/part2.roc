app "advent-15-2"
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

Sensor : {x: I64, y: I64, bx: I64, by: I64}
SensorVisibility : {x: I64, y: I64, range: Nat}

parseSensors: Str -> List Sensor
parseSensors = \s ->
    Str.split s "\n"
    |> List.keepOks \line ->
        Str.split line "="
        |> List.joinMap \fragment -> Str.split fragment ","
        |> List.joinMap \fragment -> Str.split fragment ":"
        |> List.keepOks Str.toI64
        |> \l -> when l is
            [x, y, bx, by] -> Ok {x: x, y: y, bx: bx, by: by}
            _ -> Err InvalidSensorString

Range : {from: I64, to: I64}

cutRangeFromList : List Range, Range -> List Range
cutRangeFromList = \ranges, rangeToRemove ->
    List.joinMap ranges \range -> cutRange range rangeToRemove

expect cutRangeFromList [{from: 1, to: 2}] {from: 3, to: 4} == [{from: 1, to: 2}]

least = \a, b -> if a <= b then a else b
most = \a, b -> if a >= b then a else b

cutRange : Range, Range -> List Range
cutRange = \range, rangeToRemove ->
    [
        {from: range.from, to: least (rangeToRemove.from) range.to},
        {from: most (rangeToRemove.to + 1) range.from, to: range.to}
    ]
    |> List.keepIf \r -> r.from < r.to

expect cutRange {from: 1, to: 2} {from: 3, to: 4} == [{from: 1, to: 2}]

uncheckedXs : List SensorVisibility, I64 -> List Range
uncheckedXs = \sensors, y ->
    List.keepOks sensors (\sensor ->
        range = (Num.toI64 sensor.range) - (Num.abs (sensor.y - y))
        if range > 0 then Ok {from: sensor.x - range, to: sensor.x + range}
        else Err SensorOutOfRange)
    |> List.walk [{from: minX, to: maxX}] cutRangeFromList

distance : I64, I64, I64, I64 -> Nat
distance = \x, y, a, b ->
    Num.toNat ((Num.abs (x - a)) + (Num.abs (y - b)))

distanceCovered : Sensor -> Nat
distanceCovered = \s ->
    distance s.x s.y s.bx s.by

rangeToCoords : Range, I64 -> {x: I64, y: I64}
rangeToCoords = \r, y ->
    {x: r.from, y: y}

findInvisible : List Sensor -> I64
findInvisible = \sensors ->
    sensorVisibilities = List.map sensors \s -> {x: s.x, y: s.y, range: distanceCovered s}
    List.range {start: At minY, end: At maxY}
    |> List.joinMap (\y ->
        uncheckedXs sensorVisibilities y
        |> List.map \r -> rangeToCoords r y)
    |> List.first
    |> Result.map \c -> c.x * tuningFactor + c.y
    |> Result.withDefault 0

tuningFactor = 4000000

minX = 0
maxX = 4000000

minY = minX
maxY = maxX

process : Str -> Str
process = \content ->
    content
    |> parseSensors
    |> findInvisible
    |> Num.toStr
