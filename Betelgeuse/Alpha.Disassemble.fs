module Alpha.Disassemble

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
