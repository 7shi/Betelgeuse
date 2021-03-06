﻿module Alpha.Exec

open System
open System.IO
open System.Text

open ELF
open Alpha
open Alpha.Disassemble
open Alpha.Memory
open Alpha.Table
open Alpha.Syscall

let execOp vm =
    let code = read32 vm vm.pc
    let op = getOp code
    let reg = vm.reg
    vm.pc <- vm.pc + 4UL
    match formats.[int(op) >>> 16] with
    | Format.Bra ->
        let ra = int(code >>> 21) &&& 31
        let disp = int(code &&& 0x001fffffu)
        let addr = if disp < 0x00100000
                   then vm.pc + uint64(disp * 4)
                   else vm.pc - uint64((0x00200000 - disp) * 4)
        match op with
        | Op.Br
        | Op.Bsr ->
            if ra <> 31 then reg.[ra] <- vm.pc
            vm.pc <- addr
        | Op.Beq  -> if        reg.[ra]   = 0UL then vm.pc <- addr
        | Op.Bne  -> if        reg.[ra]  <> 0UL then vm.pc <- addr
        | Op.Bge  -> if int64 (reg.[ra]) >= 0L  then vm.pc <- addr
        | Op.Bgt  -> if int64 (reg.[ra]) >  0L  then vm.pc <- addr
        | Op.Blt  -> if int64 (reg.[ra]) <  0L  then vm.pc <- addr
        | Op.Ble  -> if int64 (reg.[ra]) <= 0L  then vm.pc <- addr
        | Op.Blbc -> if uint16(reg.[ra])  = 0us then vm.pc <- addr
        | Op.Blbs -> if uint16(reg.[ra]) <> 0us then vm.pc <- addr
        | _ -> ()
    | Format.Mem  ->
        let ra = int(code >>> 21) &&& 31
        let vb = reg.[int(code >>> 16) &&& 31]
        let disp =
            let d = int(code &&& 0xffffu)
            uint64 <| if d >= 0x8000 then d - 0x10000 else d
        match op with
        | Op.Stt   -> writeDouble vm (vb + disp) vm.frg.[ra]
        | Op.Stq   -> write64 vm (vb + disp) reg.[ra]
        | Op.Stq_u -> write64 vm ((vb + disp) &&& ~~~7UL) reg.[ra]
        | Op.Stl   -> write32 vm (vb + disp) (reg.[ra] |> uint32)
        | Op.Stw   -> write16 vm (vb + disp) (reg.[ra] |> uint16)
        | Op.Stb   -> write8  vm (vb + disp) (reg.[ra] |> byte)
        | _ -> ()
        if ra <> 31 then
            match op with
            | Op.Lda   -> reg.[ra] <- vb + disp
            | Op.Ldah  -> reg.[ra] <- vb + (disp <<< 16)
            | Op.Ldq   -> reg.[ra] <- read64 vm (vb + disp)
            | Op.Ldq_u -> reg.[ra] <- read64 vm ((vb + disp) &&& ~~~7UL)
            | Op.Ldl   -> reg.[ra] <- uint64 << int <| read32 vm (vb + disp)
            | Op.Ldwu  -> reg.[ra] <- uint64        <| read16 vm (vb + disp)
            | Op.Ldbu  -> reg.[ra] <- uint64        <| read8  vm (vb + disp)
            | _ -> ()
    | Format.Mbr ->
        let ra = int(code >>> 21) &&& 31
        let vb = reg.[int(code >>> 16) &&& 31]
        match op with
        | Op.Jmp
        | Op.Jsr
        | Op.Ret
        | Op.Jsr_coroutine ->
            if ra <> 31 then reg.[ra] <- vm.pc
            vm.pc <- vb &&& ~~~3UL
        | _ -> ()
    | Format.Opr ->
        let va = reg.[int(code >>> 21) &&& 31]
        let vb = if (code &&& 0x1000u) = 0u
                 then reg.[int(code >>> 16) &&& 31]
                 else uint64((code >>> 13) &&& 0xffu)
        let rc = int(code &&& 31u)
        if rc <> 31 then
            let m = (int(vb) &&& 7) <<< 3
            let va_l = va |> uint32
            let vb_l = vb |> uint32
            match op with
            | Op.Bis    -> reg.[rc] <- va ||| vb
            | Op.Bic    -> reg.[rc] <- va &&& ~~~vb
            | Op.And    -> reg.[rc] <- va &&& vb
            | Op.Xor    -> reg.[rc] <- va ^^^ vb
            | Op.Ornot  -> reg.[rc] <- va ||| ~~~vb
            | Op.Eqv    -> reg.[rc] <- va ^^^ ~~~vb
            | Op.Zap    -> reg.[rc] <- va &&& ~~~mask.[int(vb) &&& 255]
            | Op.Zapnot -> reg.[rc] <- va &&& mask.[int(vb) &&& 255]
            | Op.Addq   -> reg.[rc] <- va + vb
            | Op.Subq   -> reg.[rc] <- va - vb
            | Op.Mulq   -> reg.[rc] <- (uint64)((int64)va * (int64)vb)
            | Op.Umulh  ->
                if va = 0UL || vb = 0UL then
                    reg.[rc] <- 0UL
                else
                    let xh = va >>> 32
                    let xl = va &&& 0xffffffffUL
                    let yh = vb >>> 32
                    let yl = vb &&& 0xffffffffUL
                    let a = xh * yl
                    let ah = a >>> 32
                    let al = a &&& 0xffffffffUL
                    let b = xl * yh
                    let bh = b >>> 32
                    let bl = b &&& 0xffffffffUL
                    reg.[rc] <- ((((xl * yl) >>> 32) + al + bl) >>> 32) + ah + bh + xh * yh
            | Op.S4addq  -> reg.[rc] <- (va <<< 2) + vb
            | Op.S8addq  -> reg.[rc] <- (va <<< 3) + vb
            | Op.S4subq  -> reg.[rc] <- (va <<< 2) - vb
            | Op.S8subq  -> reg.[rc] <- (va <<< 3) - vb
            | Op.Sextb   -> reg.[rc] <- uint64 << sbyte << byte   <| vb
            | Op.Sextw   -> reg.[rc] <- uint64 << int16 << uint16 <| vb
            | Op.Sll     -> reg.[rc] <- va <<< (int)vb
            | Op.Srl     -> reg.[rc] <- va >>> (int)vb
            | Op.Sra     -> reg.[rc] <- (uint64)(((int64)va) >>> (int)vb)
            | Op.Cmpeq   -> reg.[rc] <- if va = vb then 1UL else 0UL
            | Op.Cmple   -> reg.[rc] <- if int64(va) <= int64(vb) then 1UL else 0UL
            | Op.Cmplt   -> reg.[rc] <- if int64(va) <  int64(vb) then 1UL else 0UL
            | Op.Cmpule  -> reg.[rc] <- if va <= vb then 1UL else 0UL
            | Op.Cmpult  -> reg.[rc] <- if va <  vb then 1UL else 0UL
            | Op.Cmoveq  -> if        va   = 0UL then reg.[rc] <- vb
            | Op.Cmovne  -> if        va  <> 0UL then reg.[rc] <- vb
            | Op.Cmovge  -> if int64 (va) >= 0L  then reg.[rc] <- vb
            | Op.Cmovgt  -> if int64 (va) >  0L  then reg.[rc] <- vb
            | Op.Cmovle  -> if int64 (va) <= 0L  then reg.[rc] <- vb
            | Op.Cmovlt  -> if int64 (va) <  0L  then reg.[rc] <- vb
            | Op.Cmovlbc -> if uint16(va)  = 0us then reg.[rc] <- vb
            | Op.Cmovlbs -> if uint16(va) <> 0us then reg.[rc] <- vb

            | Op.Mskbl -> reg.[rc] <- va &&& ~~~(0xffUL <<< m)
            | Op.Mskwl -> reg.[rc] <- va &&& ~~~(0xffffUL <<< m)
            | Op.Mskll -> reg.[rc] <- va &&& ~~~(0xffffffffUL <<< m)
            | Op.Mskql -> reg.[rc] <- va &&& ~~~(UInt64.MaxValue <<< m)
            | Op.Mskwh -> reg.[rc] <- va &&& ~~~(0xffffUL >>> (64 - m))
            | Op.Msklh -> reg.[rc] <- va &&& ~~~(0xffffffffUL >>> (64 - m))
            | Op.Mskqh -> reg.[rc] <- va &&& ~~~(UInt64.MaxValue >>> (64 - m))
            | Op.Insbl -> reg.[rc] <- (va &&& 0xffUL) <<< m
            | Op.Inswl -> reg.[rc] <- (va &&& 0xffffUL) <<< m
            | Op.Insll -> reg.[rc] <- (va &&& 0xffffffffUL) <<< m
            | Op.Insql -> reg.[rc] <- (va <<< m)
            | Op.Inswh -> reg.[rc] <- (va &&& 0xffffUL) >>> (64 - m)
            | Op.Inslh -> reg.[rc] <- (va &&& 0xffffffffUL) >>> (64 - m)
            | Op.Insqh -> reg.[rc] <- (va >>> (64 - m))
            | Op.Extbl -> reg.[rc] <- (va >>> m) &&& 0xffUL
            | Op.Extwl -> reg.[rc] <- (va >>> m) &&& 0xffffUL
            | Op.Extll -> reg.[rc] <- (va >>> m) &&& 0xffffffffUL
            | Op.Extql -> reg.[rc] <- (va >>> m)
            | Op.Extwh -> reg.[rc] <- (va <<< (64 - m)) &&& 0xffffUL
            | Op.Extlh -> reg.[rc] <- (va <<< (64 - m)) &&& 0xffffffffUL
            | Op.Extqh -> reg.[rc] <- (va <<< (64 - m))

            | Op.Addl -> reg.[rc] <- uint64 << int <| va_l + vb_l
            | Op.Subl -> reg.[rc] <- uint64 << int <| va_l - vb_l
            | Op.Mull -> reg.[rc] <- uint64 <| int(va_l) * int(vb_l)
            | Op.S4addl -> reg.[rc] <- uint64 << int <| (va_l <<< 2) + vb_l
            | Op.S8addl -> reg.[rc] <- uint64 << int <| (va_l <<< 3) + vb_l
            | Op.S4subl -> reg.[rc] <- uint64 << int <| (va_l <<< 2) - vb_l
            | Op.S8subl -> reg.[rc] <- uint64 << int <| (va_l <<< 3) - vb_l

            | _ -> raise << vm.Abort <| sprintf "未実装: [opr] %s" (getMnemonic op)
    | Format.Pcd ->
        let pal = int(code &&& 0x03ffffffu)
        match op with
        | Op.Pal1f ->
            match pal with
            | 0x0000f000 -> // __divl
                vm.pv <- uint64(int(vm.t10) / int(vm.t11))
                vm.pc <- vm.t9
            | 0x0000f001 -> // __divlu
                vm.pv <- uint64(uint32(vm.t10) / uint32(vm.t11))
                vm.pc <- vm.t9
            | 0x0000f002 -> // __divq
                vm.pv <- uint64(int64(vm.t10) / int64(vm.t11))
                vm.pc <- vm.t9
            | 0x0000f003 -> // __divqu
                vm.pv <- vm.t10 / vm.t11
                vm.pc <- vm.t9
            | _ -> raise << vm.Abort <| sprintf "未実装: [pcd] %s %08x" (getMnemonic op) pal
        | _ -> raise << vm.Abort <| sprintf "未実装: [pcd] %s %08x" (getMnemonic op) pal
    | _ -> raise << vm.Abort <| sprintf "未実装: %s" (getMnemonic op)

let exec vm (elf:ELF64) (tw:TextWriter) (args:string[]) =
    vm.out <- tw
 
    array.Clear(vm.stack, 0, vm.stack.Length)
    array.Clear(vm.reg, 0, vm.reg.Length)
    array.Clear(vm.frg, 0, vm.frg.Length)
    
    let argb = [for arg in args -> Encoding.UTF8.GetBytes(arg)]
    let mutable pargs = ((stackSize |> int) - List.sum([for arg in argb -> argb.Length])) >>> 3 <<< 3
    let pargv = pargs - args.Length * 8
    for i = 0 to args.Length - 1 do
        array.Copy(BitConverter.GetBytes(stackStart + (pargs |> uint64)), 0, vm.stack, pargv + i * 8, 8)
        array.Copy(argb.[i], 0, vm.stack, pargs, argb.[i].Length)
        pargs <- pargs + argb.[i].Length
    
    let argv = stackStart + (pargv |> uint64)
    vm.ra <- stackEnd
    vm.sp <- argv
    vm.a0 <- args.Length |> uint64
    vm.a1 <- argv

    let text  = elf.Text
    //let start = text.sh_addr
    //let end'  = start + text.sh_size
    let start = vm.memoryStart
    let end'  = vm.memoryEnd
    vm.pc <- elf.e_entry
    vm.pv <- elf.e_entry // pv(procedure value) for gp(global pointer)

    tw.WriteLine("pc={0:x16}: 開始", vm.pc)
    let startTime = DateTime.Now
    
    execStep <- fun vm ->
        if start <= vm.pc && vm.pc < end' then
            execOp vm
        else if funcStart <= vm.pc && vm.pc < funcEnd then
            callFunc vm
        else
            vm.pc <- vm.pc + 4UL
            raise << vm.Abort <| "不正な実行アドレス"
    
    try
        while vm.pc <> stackEnd do execStep vm
    with
    | :? VMException -> reraise()
    | e -> raise << vm.Abort <| e.Message

    tw.WriteLine("---")
    tw.WriteLine("完了しました: {0:0.00}s", (DateTime.Now - startTime).TotalSeconds)
