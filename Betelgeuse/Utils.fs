module Utils

open System
open System.IO

let any = (None, "")

let readString (br:BinaryReader) (off:int64) =
    let sw = new StringWriter()
    let pos = br.BaseStream.Position
    br.BaseStream.Position <- off
    let mutable b = br.ReadByte()
    while b <> 0uy do
        sw.Write(b |> char)
        b <- br.ReadByte()
    br.BaseStream.Position <- pos
    sw.ToString()
    
let read (tw:TextWriter) (pos:int64) (name:string) (format:string) (nl:bool) (f:unit->'t) (cond:'t option*string) =
    tw.Write("{0:x8}: {1,-12}: ", pos, name)
    let ret = f()
    tw.Write(format, ret)
    if nl then tw.WriteLine()
    let v, err = cond in if v.IsSome && ret <> v.Value then raise <| new Exception(name + " <> " + err)
    ret

let readByte tw (br:BinaryReader) (cond:byte option*string) name =
    read tw br.BaseStream.Position name "{0:x2}" true br.ReadByte cond

let readBChar tw (br:BinaryReader) (cond:char option*string) name =
    read tw br.BaseStream.Position name "'{0}'" true (fun _ -> br.ReadByte() |> char) cond |> byte

let readUInt16 tw (br:BinaryReader) (cond:uint16 option*string) name =
    read tw br.BaseStream.Position name "{0:x4}" true br.ReadUInt16 cond

let readUInt32 tw (br:BinaryReader) (cond:uint32 option*string) name =
    read tw br.BaseStream.Position name "{0:x8}" true br.ReadUInt32 cond

let readUInt64 tw (br:BinaryReader) (cond:uint64 option*string) name =
    read tw br.BaseStream.Position name "{0:x16}" true br.ReadUInt64 cond

let readUInt32WithString tw (br:BinaryReader) (name:string) (stroff:uint64) =
    let ret1 = read tw br.BaseStream.Position name "{0:x8}" false br.ReadUInt32 any
    let ret2 = readString br ((stroff + uint64(ret1)) |> int64)
    if not(String.IsNullOrEmpty(ret2)) then
        tw.Write(" => {0}", ret2)
    tw.WriteLine()
    ret1, ret2
