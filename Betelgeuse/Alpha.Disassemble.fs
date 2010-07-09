module Alpha.Disassemble

open System
open System.IO

open Alpha
open Alpha.Table

let getMnemonic (op:Op) = op.ToString().ToLower().Replace("__", "/")

let getOp(code:uint32) =
    let op = int(code >>> 26)
    match op with
    | 0x00 -> Op.Call_pal
    | 0x01 -> Op.Opc01
    | 0x02 -> Op.Opc02
    | 0x03 -> Op.Opc03
    | 0x04 -> Op.Opc04
    | 0x05 -> Op.Opc05
    | 0x06 -> Op.Opc06
    | 0x07 -> Op.Opc07
    | 0x08 -> Op.Lda
    | 0x09 -> Op.Ldah
    | 0x0a -> Op.Ldbu
    | 0x0b -> Op.Ldq_u
    | 0x0c -> Op.Ldwu
    | 0x0d -> Op.Stw
    | 0x0e -> Op.Stb
    | 0x0f -> Op.Stq_u
    | 0x10
    | 0x11
    | 0x12
    | 0x13 -> subops.[op].[int(code >>> 5) &&& 0x7f]
    | 0x14
    | 0x15
    | 0x16
    | 0x17 -> subops.[op].[int(code >>> 5) &&& 0x7ff]
    | 0x18 ->
        match int(code &&& 0xffffu) with
        | 0x0000 -> Op.Trapb
        | 0x0400 -> Op.Excb
        | 0x4000 -> Op.Mb
        | 0x4400 -> Op.Wmb
        | 0x8000 -> Op.Fetch
        | 0xa000 -> Op.Fetch_m
        | 0xc000 -> Op.Rpcc
        | 0xe000 -> Op.Rc
        | 0xf000 -> Op.Rs
        | 0xe800 -> Op.Ecb
        | 0xf800 -> Op.Wh64
        | 0xfc00 -> Op.Wh64en
        | _      -> Op.___
    | 0x19 -> Op.Pal19
    | 0x1a -> subops.[op].[int(code >>> 14) &&& 3]
    | 0x1b -> Op.Pal1b
    | 0x1c -> subops.[op].[int(code >>> 5) &&& 0x7f]
    | 0x1d -> Op.Pal1d
    | 0x1e -> Op.Pal1e
    | 0x1f -> Op.Pal1f
    | 0x20 -> Op.Ldf
    | 0x21 -> Op.Ldg
    | 0x22 -> Op.Lds
    | 0x23 -> Op.Ldt
    | 0x24 -> Op.Stf
    | 0x25 -> Op.Stg
    | 0x26 -> Op.Sts
    | 0x27 -> Op.Stt
    | 0x28 -> Op.Ldl
    | 0x29 -> Op.Ldq
    | 0x2a -> Op.Ldl_l
    | 0x2b -> Op.Ldq_l
    | 0x2c -> Op.Stl
    | 0x2d -> Op.Stq
    | 0x2e -> Op.Stl_c
    | 0x2f -> Op.Stq_c
    | 0x30 -> Op.Br
    | 0x31 -> Op.Fbeq
    | 0x32 -> Op.Fblt
    | 0x33 -> Op.Fble
    | 0x34 -> Op.Bsr
    | 0x35 -> Op.Fbne
    | 0x36 -> Op.Fbge
    | 0x37 -> Op.Fbgt
    | 0x38 -> Op.Blbc
    | 0x39 -> Op.Beq
    | 0x3a -> Op.Blt
    | 0x3b -> Op.Ble
    | 0x3c -> Op.Blbs
    | 0x3d -> Op.Bne
    | 0x3e -> Op.Bge
    | 0x3f -> Op.Bgt
    | _    -> Op.___

let disassemble (tw:TextWriter) (addr:uint64) (code:uint32) =
    let op = getOp(code)
    let opc = int(code >>> 26)
    let mne = getMnemonic(op);
    tw.Write("{0:x8} => {1:x2}", code, opc)
    match formats.[opc] with
    | Format.Pcd ->
        let pal = code &&& 0x03ffffffu
        tw.Write("      {0:x8}     => {1,-7} {0:x8}", pal, mne)
    | Format.Bra ->
        let ra = int(code >>> 21) &&& 31
        let disp = int(code &&& 0x001fffffu)
        let sdisp = if disp < 0x00100000
                    then String.Format("{0:x8}", addr + uint64(disp * 4 + 4))
                    else String.Format("{0:x8}", addr - uint64((0x00200000 - disp) * 4 + 4))
        tw.Write("      r{0:00} {1:x8} => {2,-7} {3},{4}",
            ra, disp, mne, regname.[ra], sdisp)
        if ra = 31 && op = Op.Br then tw.Write(" => br {0}", sdisp)
    | Format.Mem ->
        let ra = int(code >>> 21) &&& 31
        let rb = int(code >>> 16) &&& 31
        let disp = int(code &&& 0xffffu)
        let args = if disp < 0x8000
                   then String.Format("{0:x}({1})", disp, regname.[rb])
                   else String.Format("-{0:x}({1})", 0x10000 - disp, regname.[rb])
        tw.Write("      r{0:00} r{1:00} {2:x4}", ra, rb, disp)
        tw.Write(" => {0,-7} {1},", mne, regname.[ra])
        tw.Write(args)
        if rb = 31 && op = Op.Lda then
            tw.Write(" => mov {0:x},{1}", disp, regname.[ra])
        else if rb = 31 && op = Op.Ldah then
            tw.Write(" => mov {0:x}0000,{1}", disp, regname.[ra])
        else if ra = 31 then
            if disp = 0 && op = Op.Ldq_u then
                tw.Write(" => unop")
            else
                let pse =
                    match op with
                    | Op.Ldl -> "prefetch"
                    | Op.Ldq -> "prefetch_en"
                    | Op.Lds -> "prefetch_m"
                    | Op.Ldt -> "prefetch_men"
                    | _      -> ""
                if pse <> "" then
                    tw.Write("{0} {1}", pse, args)
    | Format.Mfc ->
        let ra = int(code >>> 21) &&& 31
        let rb = int(code >>> 16) &&& 31
        tw.Write(".{0:x4} r{1:00} r{2:00}      => {3,-7} {4},{5}",
            code &&& 0xffffu, ra, rb, mne, regname.[ra], regname.[rb])
    | Format.Mbr ->
        let ra = int(code >>> 21) &&& 31
        let rb = int(code >>> 16) &&& 31
        let disp = int(code &&& 0x3fffu)
        tw.Write(".{0:x}   ", (code >>> 14) &&& 3u)
        tw.Write(" r{0:00} r{1:00} {2:x4}", ra, rb, disp)
        tw.Write(" => {0,-7} {1},({2}),{3:x4}",
            mne, regname.[ra], regname.[rb], disp)
    | Format.Opr ->
        let ra = int(code >>> 21) &&& 31
        let rc = int(code &&& 31u)
        tw.Write(".{0:x2}  ", (code >>> 5) &&& 0x7fu)
        let rb, arg2 =
            if (code &&& 0x1000u) = 0u then
                let rb = int(code >>> 16) &&& 31
                tw.Write(" r{0:00} r{1:00} r{2:00} ", ra, rb, rc)
                rb, regname.[rb]
            else
                let arg2 = String.Format("{0:x2}", (code >>> 13) &&& 0xffu)
                tw.Write(" r{0:00}  {1} r{2:00} ", ra, arg2, rc)
                -1, arg2
        tw.Write(" => {0,-7} {1},{2},{3}",
            mne, regname.[ra], arg2, regname.[rc])
        if ra = 31 then
            let pse =
                match op with
                | Op.Bis ->
                    if rb = 31 && rc = 31 then
                        tw.Write(" => nop")
                        ""
                    else if rb = 31 then
                        tw.Write(" => clr {0}", regname.[rc])
                        ""
                    else
                        "mov"
                | Op.Addl    -> "sextl"
                | Op.Ornot   -> "not"
                | Op.Subl    -> "negl"
                | Op.Subl__v -> "negl/v"
                | Op.Subq    -> "negq"
                | Op.Subq__v -> "negq/v"
                | _ -> ""
            if pse <> "" then
                tw.Write(" => {0} {1},{2}", pse, arg2, regname.[rc])
    | Format.F_P ->
        let fa = int(code >>> 21) &&& 31
        let fb = int(code >>> 16) &&& 31
        let fc = int(code &&& 31u)
        tw.Write(".{0:x3} ", (code >>> 5) &&& 0x7ffu)
        tw.Write(" f{0:00} f{1:00} f{2:00}  => {3,-7} f{0:00},f{1:00},f{2:00}",
            fa, fb, fc, mne)
        let pst, pse =
            let fpcr() =
                if not(fa = fb && fb = fc) then 0, "" else
                    match op with
                    | Op.Mf_fpcr | Op.Mt_fpcr -> 1, mne
                    | _ -> 0, ""
            let cpys() =
                if fa <> fb then fpcr() else
                    match op with
                    | Op.Cpys  -> 2, "fmov"
                    | Op.Cpysn -> 2, "fneg"
                    | _ -> fpcr()
            if fa <> 31 then cpys() else
                match op with
                | Op.Cpys ->
                    if fb = 31 && fc = 31 then
                        0, "fnop"
                    else if fb = 31 then
                        1, "fclr"
                    else
                        2, "fabs"
                | Op.Subf      -> 2, "negf"
                | Op.Subf__s   -> 2, "negf/s"
                | Op.Subg      -> 2, "negg"
                | Op.Subg__s   -> 2, "negg/s"
                | Op.Subs      -> 2, "negs"
                | Op.Subs__su  -> 2, "negs/su"
                | Op.Subs__sui -> 2, "negs/sui"
                | Op.Subt      -> 2, "negt"
                | Op.Subt__su  -> 2, "negt/su"
                | Op.Subt__sui -> 2, "negt/sui"
                | _ -> cpys()
        if pse <> "" then
            match pst with
            | 0 -> tw.Write(" => {0}", pse)
            | 1 -> tw.Write(" => {0} f{1:00}", pse, fc)
            | 2 -> tw.Write(" => {0} f{1:00},f{2:00}", pse, fb, fc)
            | _ -> ()
    | _ ->
        tw.Write("                   => {0}", mne)
    op
