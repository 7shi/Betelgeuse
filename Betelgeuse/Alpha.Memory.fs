module Alpha.Memory

open System
open System.IO

open ELF

let stackSize  = 1024UL * 1024UL // 1MB
let stackStart = 0x00f00000UL
let stackEnd   = stackStart + stackSize // 01000000
let putcharAddress = 0x10000000UL

type Ptr = { buf:byte[]; ptr:int }

type MMU =
    { stack      : byte[]
      memory     : byte[]
      memoryStart: uint64
      memoryEnd  : uint64
      output     : TextWriter }

    static member Create (elf:ELF64) (data:byte[]) (output:TextWriter) =
        let vm =
            { stack       = Array.zeroCreate<byte> (stackSize |> int)
              memory      = Array.zeroCreate<byte> (elf.Size  |> int)
              memoryStart = elf.Start
              memoryEnd   = elf.End
              output      = output }
        for sh in elf.Headers do
            array.Copy(data, sh.sh_offset |> int,
                       vm.memory, (sh.sh_addr - vm.memoryStart) |> int,
                       sh.sh_size |> int)
        vm
    
    member x.GetPtr (addr:uint64) (size:int) =
        if addr >= x.memoryStart && addr <= x.memoryEnd - uint64(size) then
            { buf = x.memory; ptr = (addr - x.memoryStart) |> int }
        else if addr >= stackStart && addr <= stackEnd - uint64(size) then
            { buf = x.stack; ptr = (addr - stackStart) |> int }
        else
            failwith(sprintf "不正なアドレス: %016x" addr)

    member x.WriteDouble (addr:uint64) (v:float) =
        let mp = x.GetPtr addr 8
        array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 8)

    member x.Write64 (addr:uint64) (v:uint64) =
        if addr = putcharAddress then x.PutChar(v |> byte) else
            let mp = x.GetPtr addr 8
            array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 8)

    member x.Write32 (addr:uint64) (v:uint32) =
        if addr = putcharAddress then x.PutChar(v |> byte) else
            let mp = x.GetPtr addr 4
            array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 4)

    member x.Write16 (addr:uint64) (v:uint16) =
        if addr = putcharAddress then x.PutChar(v |> byte) else
            let mp = x.GetPtr addr 2
            array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 2)

    member x.Write8 (addr:uint64) (v:byte) =
        if addr = putcharAddress then x.PutChar(v |> byte) else
            let mp = x.GetPtr addr 1
            mp.buf.[mp.ptr] <- v

    member x.Read64 (addr:uint64) =
        if addr = putcharAddress then 0UL else
            let mp = x.GetPtr addr 8
            BitConverter.ToUInt64(mp.buf, mp.ptr)

    member x.Read32 (addr:uint64) =
        if addr = putcharAddress then 0u else
            let mp = x.GetPtr addr 4
            BitConverter.ToUInt32(mp.buf, mp.ptr)

    member x.Read16 (addr:uint64) =
        if addr = putcharAddress then 0us else
            let mp = x.GetPtr addr 2
            BitConverter.ToUInt16(mp.buf, mp.ptr)

    member x.Read8 (addr:uint64) =
        if addr = putcharAddress then 0uy else
            let mp = x.GetPtr addr 1
            mp.buf.[mp.ptr]

    member x.PutChar (v:byte) =
        if v = byte('\n') then 
            x.output.WriteLine()
        else
            x.output.Write("{0}", v |> char)
