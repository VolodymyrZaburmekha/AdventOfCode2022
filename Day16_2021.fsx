open System
open System.IO
open Microsoft.FSharp.Core

type Packet =
    | Literal of version: int64 * value: int64
    | Operator of version: int64 * subPackets: Packet list * operation: (Packet list -> int64)

type LengthType =
    | Bits of int
    | SubPackets of int

let fromHexadecimalToBinary (hex: string) =
    hex
    |> Seq.fold
        (fun str digit ->
            let binary = Convert.ToString(Convert.ToInt32(digit.ToString(), 16), 2)
            str + String.Join("", Seq.replicate (4 - binary.Length) "0") + binary)
        ""

let parseBinary (str: string) = Convert.ToInt64(str, 2)

let getVersion (packet: string) = packet.Substring(0, 3) |> parseBinary, packet.Substring(3)

let getTypeId = getVersion // same logic, take 3 and return the rest

let parseLiteral version (typelessPacket: string) =
    let rec loop accumulator (remains: string) =
        let currentNumber = remains.Substring(1, 4)
        let nextRemains = remains.Substring(5)
        if remains.StartsWith("0") then
            let value = accumulator + currentNumber |> parseBinary
            Literal(version, value), nextRemains
        else
            loop (accumulator + currentNumber) nextRemains
    loop "" typelessPacket

let getOperatorPacketLengthType (packetRemains: string) =
    let bitsLength, packetsF = if packetRemains.Substring(0, 1) = "0" then (15, Bits) else (11, SubPackets)
    let length = Convert.ToInt32(packetRemains.Substring(1, bitsLength), 2)
    packetsF length, packetRemains.Substring(bitsLength + 1)

let rec getValueOfPacket p =
    match p with
    | Literal(_, value) -> value
    | Operator(_, subPackets, operatorFunc) -> operatorFunc subPackets

let rec getOperatorValue typeId (packets: Packet list) =
    let compare f packets =
        packets
        |> Seq.map getValueOfPacket
        |> Seq.pairwise
        |> Seq.head
        |> (fun (first, second) -> if f first second = true then 1L else 0L)
    match typeId with
    | 0L -> packets |> Seq.sumBy getValueOfPacket // sum
    | 1L -> packets |> Seq.fold (fun acc p -> acc * (getValueOfPacket p)) 1L // product
    | 2L -> packets |> Seq.map getValueOfPacket |> Seq.min // min
    | 3L -> packets |> Seq.map getValueOfPacket |> Seq.max // max
    | 5L -> packets |> compare (>) // greater
    | 6L -> packets |> compare (<) // less
    | 7L -> packets |> compare (=) //equals
    | _ -> failwith "incorrect operator"

let parsePackets (packets: string) =
    let rec loop packStr (embeddedPacketsCount: int64) (packages: Packet list) : Packet list * string =
        if String.IsNullOrEmpty(packStr) || packStr |> Seq.forall (fun c -> c = '0') then
            packages, ""
        elif embeddedPacketsCount = 0 then
            packages, packStr
        else
            let version, remains = getVersion packStr
            let typeId, remains = getTypeId remains
            match typeId with
            | 4L -> // literal
                let literal, remains = parseLiteral version remains
                loop remains (embeddedPacketsCount - 1L) (packages @ [ literal ])
            | operatorId -> //operator
                let operatorFunction = getOperatorValue operatorId
                let lengthType, remains = getOperatorPacketLengthType remains
                match lengthType with
                | Bits length ->
                    let content = remains.Substring(0, length)
                    let internalPackets, _ = loop content (-1L) []
                    let operator = Operator(version, internalPackets, operatorFunction)
                    let otherPackages, remains = loop (remains.Substring(length)) (embeddedPacketsCount - 1L) []
                    packages @ [ operator ] @ otherPackages, remains
                | SubPackets subPacketsCount ->
                    let innerPackets, remains = loop remains subPacketsCount []
                    let operator = Operator(version, innerPackets, operatorFunction)
                    loop remains (embeddedPacketsCount - 1L) (packages @ [ operator ])
    let packets, _ = loop packets -1L []
    List.head packets

let rec solvePart1 packet =
    match packet with
    | Literal(version, _) -> version
    | Operator(version, subPackets, _) -> version + (subPackets |> List.sumBy solvePart1)

#time "on"
let part1 = "Data/Day16_2021.txt" |> File.ReadAllText |> fromHexadecimalToBinary |> parsePackets |> solvePart1
let part2 = "Data/Day16_2021.txt" |> File.ReadAllText |> fromHexadecimalToBinary |> parsePackets |> getValueOfPacket
