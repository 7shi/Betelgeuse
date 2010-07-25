module Alpha.Syscall

open System
open System.IO

open Alpha
open Alpha.Memory

type Funcs =
    | Exit   = 0
    | FPutC  = 1
    | FGetC  = 2
    | FOpen  = 3
    | FClose = 4
    | FWrite = 5
    | FRead  = 6
    | FSeek  = 7
    | Last   = 8

let funcStart = 0x00ef0000UL
let funcEnd   = funcStart + (Funcs.Last |> uint64) * 4UL

let mutable openWrite = fun (fn:string) -> new FileStream(fn, FileMode.Create)
let mutable openRead  = fun (fn:string) -> new FileStream(fn, FileMode.Open)

let callFunc (vm:VM) =
    let f = int(vm.pc - funcStart) >>> 2
    match f |> enum<Funcs> with
    | Funcs.Exit ->
        vm.pc <- stackStart
    | Funcs.FPutC ->
        let c = vm.reg.[Regs.A0 |> int] |> char
        let f = vm.reg.[Regs.A1 |> int] |> int
        if f = 0 then
            if c = '\n' then 
                vm.out.WriteLine()
            else
                vm.out.Write("{0}", c)
            vm.pc <- vm.reg.[Regs.RA |> int]
        vm.reg.[Regs.V0 |> int] <- 1UL
    | _ ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        raise << vm.Abort <| sprintf "未実装 Syscall: %x" f
