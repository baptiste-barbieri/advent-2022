interface Bug
    exposes []
    imports []

Packet : [IntPacket Nat, SubPacket (List Packet)]

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

expect List.sortWith [2, 1] Num.compare == [1, 2]
expect List.sortWith [IntPacket 2, IntPacket 1] correctOrder == [IntPacket 1, IntPacket 2]
expect List.sortWith [SubPacket [IntPacket 2], SubPacket [IntPacket 1]] correctOrder == [SubPacket [IntPacket 1], SubPacket [IntPacket 2]]
