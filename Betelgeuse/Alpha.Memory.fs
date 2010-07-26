module Alpha.Memory

open System
open System.Collections.Generic
open System.IO
open System.Text

open ELF
open Alpha

let stackSize  = 1024UL * 1024UL // 1MB
let stackStart = 0x00f00000UL
let stackEnd   = stackStart + stackSize // 01000000

type Ptr = { buf:byte[]; ptr:int }

let createVM (elf:ELF64) (data:byte[]) =
    let vm =
        { pc          = 0UL
          reg         = Array.zeroCreate<uint64> 32
          frg         = Array.zeroCreate<float> 32
          stack       = Array.zeroCreate<byte> (stackSize |> int)
          memory      = Array.zeroCreate<byte> (elf.Size  |> int)
          memoryStart = elf.Start
          memoryEnd   = elf.End
          out         = new StringWriter() }
    for sh in elf.Headers do
        if (sh.sh_type &&& 1u) <> 0u then
            array.Copy(data, sh.sh_offset |> int,
                       vm.memory, (sh.sh_addr - vm.memoryStart) |> int,
                       sh.sh_size |> int)
    vm

let getPtr (vm:VM) (addr:uint64) (size:int) =
    if addr >= vm.memoryStart && addr <= vm.memoryEnd - uint64(size) then
        { buf = vm.memory; ptr = (addr - vm.memoryStart) |> int }
    else if addr >= stackStart && addr <= stackEnd - uint64(size) then
        { buf = vm.stack; ptr = (addr - stackStart) |> int }
    else
        raise(vm.Abort(sprintf "不正なアドレス: %016x" addr))

let writeDouble vm addr (v:float) =
    let mp = getPtr vm addr 8
    array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 8)

let write64 vm addr (v:uint64) =
    let mp = getPtr vm addr 8
    array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 8)

let write32 vm addr (v:uint32) =
    let mp = getPtr vm addr 4
    array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 4)

let write16 vm addr (v:uint16) =
    let mp = getPtr vm addr 2
    array.Copy(BitConverter.GetBytes(v), 0, mp.buf, mp.ptr, 2)

let write8 vm addr (v:byte) =
    let mp = getPtr vm addr 1
    mp.buf.[mp.ptr] <- v

let read64 vm addr =
    let mp = getPtr vm addr 8
    BitConverter.ToUInt64(mp.buf, mp.ptr)

let read32 vm addr =
    let mp = getPtr vm addr 4
    BitConverter.ToUInt32(mp.buf, mp.ptr)

let read16 vm addr =
    let mp = getPtr vm addr 2
    BitConverter.ToUInt16(mp.buf, mp.ptr)

let read8 vm addr =
    let mp = getPtr vm addr 1
    mp.buf.[mp.ptr]

let readString vm addr =
    let mp = getPtr vm addr 1
    let rec read p =
        if mp.buf.[mp.ptr + p] = 0uy then p else read(p + 1)
    let len = read 0
    let bytes = Array.zeroCreate<byte> len
    array.Copy(mp.buf, mp.ptr, bytes, 0, len)
    Encoding.UTF8.GetString(bytes, 0, bytes.Length)
