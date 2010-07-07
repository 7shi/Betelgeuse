module Utils

open System
open System.IO
open System.Text

let any = (None, "")

let readString (br:BinaryReader) (off:int64) =
    let sb = new StringBuilder()
    let pos = br.BaseStream.Position
    br.BaseStream.Position <- off
    let mutable b = br.ReadByte()
    while b <> 0uy do
        ignore <| sb.Append(b |> char)
        b <- br.ReadByte()
    br.BaseStream.Position <- pos
    sb.ToString()
    
let read (sb:StringBuilder) (pos:int64) (name:string) (format:string) (nl:bool) (f:unit->'t) (cond:'t option*string) =
    ignore <| sb.AppendFormat("{0:x8}: {1,-12}: ", pos, name)
    let ret = f()
    ignore <| sb.AppendFormat(format, ret)
    if nl then ignore <| sb.AppendLine()
    let v, err = cond in if v.IsSome && ret <> v.Value then raise <| new Exception(name + " <> " + err)
    ret

let readByte (sb:StringBuilder) (br:BinaryReader) (cond:byte option*string) (name:string) =
    read sb br.BaseStream.Position name "{0:x2}" true br.ReadByte cond

let readBChar (sb:StringBuilder) (br:BinaryReader) (cond:char option*string) (name:string) =
    read sb br.BaseStream.Position name "'{0}'" true (fun _ -> br.ReadByte() |> char) cond |> byte

let readUInt16 (sb:StringBuilder) (br:BinaryReader) (cond:uint16 option*string) (name:string) =
    read sb br.BaseStream.Position name "{0:x4}" true br.ReadUInt16 cond

let readUInt32 (sb:StringBuilder) (br:BinaryReader) (cond:uint32 option*string) (name:string) =
    read sb br.BaseStream.Position name "{0:x8}" true br.ReadUInt32 cond

let readUInt64 (sb:StringBuilder) (br:BinaryReader) (cond:uint64 option*string) (name:string) =
    read sb br.BaseStream.Position name "{0:x16}" true br.ReadUInt64 cond

let readUInt32WithString (sb:StringBuilder) (br:BinaryReader) (name:string) (stroff:uint64) =
    let ret1 = read sb br.BaseStream.Position name "{0:x8}" false br.ReadUInt32 any
    let ret2 = readString br ((stroff + uint64(ret1)) |> int64)
    if not(String.IsNullOrEmpty(ret2)) then
        ignore <| sb.AppendFormat(" => {0}", ret2)
    ignore <| sb.AppendLine()
    ret1, ret2
