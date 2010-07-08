module Alpha.Table

open Alpha

let formats =
    [| (* 00-03 *) Format.Pcd; Format.___; Format.___; Format.___
       (* 04-07 *) Format.___; Format.___; Format.___; Format.___
       (* 08-0b *) Format.Mem; Format.Mem; Format.Mem; Format.Mem
       (* 0c-0f *) Format.Mem; Format.Mem; Format.Mem; Format.Mem
       (* 10-13 *) Format.Opr; Format.Opr; Format.Opr; Format.Opr
       (* 14-17 *) Format.F_P; Format.F_P; Format.F_P; Format.F_P
       (* 18-1b *) Format.Mfc; Format.___; Format.Mbr; Format.___
       (* 1c-1f *) Format.Opr; Format.___; Format.___; Format.___
       (* 20-23 *) Format.Mem; Format.Mem; Format.Mem; Format.Mem
       (* 24-27 *) Format.Mem; Format.Mem; Format.Mem; Format.Mem
       (* 28-2b *) Format.Mem; Format.Mem; Format.Mem; Format.Mem
       (* 2c-2f *) Format.Mem; Format.Mem; Format.Mem; Format.Mem
       (* 30-33 *) Format.Bra; Format.Bra; Format.Bra; Format.Bra
       (* 34-37 *) Format.Bra; Format.Bra; Format.Bra; Format.Bra
       (* 38-3b *) Format.Bra; Format.Bra; Format.Bra; Format.Bra
       (* 3c-3f *) Format.Bra; Format.Bra; Format.Bra; Format.Bra |]

let regname = [| for i in 0..31 -> (enum<Regs> i).ToString().ToLower() |]

let mask =
    let bittest i j = if (i &&& (1 <<< j)) =0 then 0UL else 0xffUL <<< (8 * j)
    [| for i in 0..255 -> Seq.sum(seq { for j in 0..7 -> bittest i j }) |]

let subops =
    let subops = Array.zeroCreate<Op[]> 0x40
    
    subops.[0x10] <- Array.zeroCreate<Op> 0x80
    subops.[0x11] <- Array.zeroCreate<Op> 0x80
    subops.[0x12] <- Array.zeroCreate<Op> 0x80
    subops.[0x13] <- Array.zeroCreate<Op> 0x80
    subops.[0x14] <- Array.zeroCreate<Op> 0x800
    subops.[0x15] <- Array.zeroCreate<Op> 0x800
    subops.[0x16] <- Array.zeroCreate<Op> 0x800
    subops.[0x17] <- Array.zeroCreate<Op> 0x800
    //subops.[0x18] <- Array.zeroCreate<Op> 0x10000
    subops.[0x1a] <- Array.zeroCreate<Op> 0x4
    subops.[0x1c] <- Array.zeroCreate<Op> 0x80

    subops.[0x10].[0x00] <- Op.Addl
    subops.[0x10].[0x02] <- Op.S4addl
    subops.[0x10].[0x09] <- Op.Subl
    subops.[0x10].[0x0b] <- Op.S4subl
    subops.[0x10].[0x0f] <- Op.Cmpbge
    subops.[0x10].[0x12] <- Op.S8addl
    subops.[0x10].[0x1b] <- Op.S8subl
    subops.[0x10].[0x1d] <- Op.Cmpult
    subops.[0x10].[0x20] <- Op.Addq
    subops.[0x10].[0x22] <- Op.S4addq
    subops.[0x10].[0x29] <- Op.Subq
    subops.[0x10].[0x2b] <- Op.S4subq
    subops.[0x10].[0x2d] <- Op.Cmpeq
    subops.[0x10].[0x32] <- Op.S8addq
    subops.[0x10].[0x3b] <- Op.S8subq
    subops.[0x10].[0x3d] <- Op.Cmpule
    subops.[0x10].[0x40] <- Op.Addl__v
    subops.[0x10].[0x49] <- Op.Subl__v
    subops.[0x10].[0x4d] <- Op.Cmplt
    subops.[0x10].[0x60] <- Op.Addq__v
    subops.[0x10].[0x69] <- Op.Subq__v
    subops.[0x10].[0x6d] <- Op.Cmple

    subops.[0x11].[0x00] <- Op.And
    subops.[0x11].[0x08] <- Op.Bic
    subops.[0x11].[0x14] <- Op.Cmovlbs
    subops.[0x11].[0x16] <- Op.Cmovlbc
    subops.[0x11].[0x20] <- Op.Bis
    subops.[0x11].[0x24] <- Op.Cmoveq
    subops.[0x11].[0x26] <- Op.Cmovne
    subops.[0x11].[0x28] <- Op.Ornot
    subops.[0x11].[0x40] <- Op.Xor
    subops.[0x11].[0x44] <- Op.Cmovlt
    subops.[0x11].[0x46] <- Op.Cmovge
    subops.[0x11].[0x48] <- Op.Eqv
    subops.[0x11].[0x61] <- Op.Amask
    subops.[0x11].[0x64] <- Op.Cmovle
    subops.[0x11].[0x66] <- Op.Cmovgt
    subops.[0x11].[0x6c] <- Op.Implver

    subops.[0x12].[0x02] <- Op.Mskbl
    subops.[0x12].[0x06] <- Op.Extbl
    subops.[0x12].[0x0b] <- Op.Insbl
    subops.[0x12].[0x12] <- Op.Mskwl
    subops.[0x12].[0x16] <- Op.Extwl
    subops.[0x12].[0x1b] <- Op.Inswl
    subops.[0x12].[0x22] <- Op.Mskll
    subops.[0x12].[0x26] <- Op.Extll
    subops.[0x12].[0x2b] <- Op.Insll
    subops.[0x12].[0x30] <- Op.Zap
    subops.[0x12].[0x31] <- Op.Zapnot
    subops.[0x12].[0x32] <- Op.Mskql
    subops.[0x12].[0x34] <- Op.Srl
    subops.[0x12].[0x36] <- Op.Extql
    subops.[0x12].[0x39] <- Op.Sll
    subops.[0x12].[0x3b] <- Op.Insql
    subops.[0x12].[0x3c] <- Op.Sra
    subops.[0x12].[0x52] <- Op.Mskwh
    subops.[0x12].[0x57] <- Op.Inswh
    subops.[0x12].[0x5a] <- Op.Extwh
    subops.[0x12].[0x62] <- Op.Msklh
    subops.[0x12].[0x67] <- Op.Inslh
    subops.[0x12].[0x6a] <- Op.Extlh
    subops.[0x12].[0x72] <- Op.Mskqh
    subops.[0x12].[0x77] <- Op.Insqh
    subops.[0x12].[0x7a] <- Op.Extqh

    subops.[0x13].[0x00] <- Op.Mull
    subops.[0x13].[0x20] <- Op.Mulq
    subops.[0x13].[0x30] <- Op.Umulh
    subops.[0x13].[0x40] <- Op.Mull__v
    subops.[0x13].[0x60] <- Op.Mulq__v

    subops.[0x14].[0x004] <- Op.Itofs
    subops.[0x14].[0x00a] <- Op.Sqrtf__c
    subops.[0x14].[0x00b] <- Op.Sqrts__c
    subops.[0x14].[0x014] <- Op.Itoff
    subops.[0x14].[0x024] <- Op.Itoft
    subops.[0x14].[0x02a] <- Op.Sqrtg__c
    subops.[0x14].[0x02b] <- Op.Sqrtt__c
    subops.[0x14].[0x04b] <- Op.Sqrts__m
    subops.[0x14].[0x06b] <- Op.Sqrtt__m
    subops.[0x14].[0x08a] <- Op.Sqrtf
    subops.[0x14].[0x08b] <- Op.Sqrts
    subops.[0x14].[0x0aa] <- Op.Sqrtg
    subops.[0x14].[0x0ab] <- Op.Sqrtt
    subops.[0x14].[0x0cb] <- Op.Sqrts__d
    subops.[0x14].[0x0eb] <- Op.Sqrtt__d
    subops.[0x14].[0x10a] <- Op.Sqrtf__uc
    subops.[0x14].[0x10b] <- Op.Sqrts__uc
    subops.[0x14].[0x12a] <- Op.Sqrtg__uc
    subops.[0x14].[0x12b] <- Op.Sqrtt__uc
    subops.[0x14].[0x14b] <- Op.Sqrts__um
    subops.[0x14].[0x16b] <- Op.Sqrtt__um
    subops.[0x14].[0x18a] <- Op.Sqrtf__u
    subops.[0x14].[0x18b] <- Op.Sqrts__u
    subops.[0x14].[0x1aa] <- Op.Sqrtg__u
    subops.[0x14].[0x1ab] <- Op.Sqrtt__u
    subops.[0x14].[0x1cb] <- Op.Sqrts__ud
    subops.[0x14].[0x1eb] <- Op.Sqrtt__ud
    subops.[0x14].[0x40a] <- Op.Sqrtf__sc
    subops.[0x14].[0x42a] <- Op.Sqrtg__sc
    subops.[0x14].[0x48a] <- Op.Sqrtf__s
    subops.[0x14].[0x4aa] <- Op.Sqrtg__s
    subops.[0x14].[0x50a] <- Op.Sqrtf__suc
    subops.[0x14].[0x50b] <- Op.Sqrts__suc
    subops.[0x14].[0x52a] <- Op.Sqrtg__suc
    subops.[0x14].[0x52b] <- Op.Sqrtt__suc
    subops.[0x14].[0x54b] <- Op.Sqrts__sum
    subops.[0x14].[0x56b] <- Op.Sqrtt__sum
    subops.[0x14].[0x58a] <- Op.Sqrtf__su
    subops.[0x14].[0x58b] <- Op.Sqrts__su
    subops.[0x14].[0x5aa] <- Op.Sqrtg__su
    subops.[0x14].[0x5ab] <- Op.Sqrtt__su
    subops.[0x14].[0x5cb] <- Op.Sqrts__sud
    subops.[0x14].[0x5eb] <- Op.Sqrtt__sud
    subops.[0x14].[0x70b] <- Op.Sqrts__suic
    subops.[0x14].[0x72b] <- Op.Sqrtt__suic
    subops.[0x14].[0x74b] <- Op.Sqrts__suim
    subops.[0x14].[0x76b] <- Op.Sqrtt__suim
    subops.[0x14].[0x78b] <- Op.Sqrts__sui
    subops.[0x14].[0x7ab] <- Op.Sqrtt__sui
    subops.[0x14].[0x7cb] <- Op.Sqrts__suid
    subops.[0x14].[0x7eb] <- Op.Sqrtt__suid

    subops.[0x15].[0x000] <- Op.Addf__c
    subops.[0x15].[0x001] <- Op.Subf__c
    subops.[0x15].[0x002] <- Op.Mulf__c
    subops.[0x15].[0x003] <- Op.Divf__c
    subops.[0x15].[0x01e] <- Op.Cvtdg__c
    subops.[0x15].[0x020] <- Op.Addg__c
    subops.[0x15].[0x021] <- Op.Subg__c
    subops.[0x15].[0x022] <- Op.Mulg__c
    subops.[0x15].[0x023] <- Op.Divg__c
    subops.[0x15].[0x02c] <- Op.Cvtgf__c
    subops.[0x15].[0x02d] <- Op.Cvtgd__c
    subops.[0x15].[0x02f] <- Op.Cvtgq__c
    subops.[0x15].[0x03c] <- Op.Cvtqf__c
    subops.[0x15].[0x03e] <- Op.Cvtqg__c
    subops.[0x15].[0x080] <- Op.Addf
    subops.[0x15].[0x081] <- Op.Subf
    subops.[0x15].[0x082] <- Op.Mulf
    subops.[0x15].[0x083] <- Op.Divf
    subops.[0x15].[0x09e] <- Op.Cvtdg
    subops.[0x15].[0x0a0] <- Op.Addg
    subops.[0x15].[0x0a1] <- Op.Subg
    subops.[0x15].[0x0a2] <- Op.Mulg
    subops.[0x15].[0x0a3] <- Op.Divg
    subops.[0x15].[0x0a5] <- Op.Cmpgeq
    subops.[0x15].[0x0a6] <- Op.Cmpglt
    subops.[0x15].[0x0a7] <- Op.Cmpgle
    subops.[0x15].[0x0ac] <- Op.Cvtgf
    subops.[0x15].[0x0ad] <- Op.Cvtgd
    subops.[0x15].[0x0af] <- Op.Cvtgq
    subops.[0x15].[0x0bc] <- Op.Cvtqf
    subops.[0x15].[0x0be] <- Op.Cvtqg
    subops.[0x15].[0x100] <- Op.Addf__uc
    subops.[0x15].[0x101] <- Op.Subf__uc
    subops.[0x15].[0x102] <- Op.Mulf__uc
    subops.[0x15].[0x103] <- Op.Divf__uc
    subops.[0x15].[0x11e] <- Op.Cvtdg__uc
    subops.[0x15].[0x120] <- Op.Addg__uc
    subops.[0x15].[0x121] <- Op.Subg__uc
    subops.[0x15].[0x122] <- Op.Mulg__uc
    subops.[0x15].[0x123] <- Op.Divg__uc
    subops.[0x15].[0x12c] <- Op.Cvtgf__uc
    subops.[0x15].[0x12d] <- Op.Cvtgd__uc
    subops.[0x15].[0x12f] <- Op.Cvtgq__vc
    subops.[0x15].[0x180] <- Op.Addf__u
    subops.[0x15].[0x181] <- Op.Subf__u
    subops.[0x15].[0x182] <- Op.Mulf__u
    subops.[0x15].[0x183] <- Op.Divf__u
    subops.[0x15].[0x19e] <- Op.Cvtdg__u
    subops.[0x15].[0x1a0] <- Op.Addg__u
    subops.[0x15].[0x1a1] <- Op.Subg__u
    subops.[0x15].[0x1a2] <- Op.Mulg__u
    subops.[0x15].[0x1a3] <- Op.Divg__u
    subops.[0x15].[0x1ac] <- Op.Cvtgf__u
    subops.[0x15].[0x1ad] <- Op.Cvtgd__u
    subops.[0x15].[0x1af] <- Op.Cvtgq__v
    subops.[0x15].[0x400] <- Op.Addf__sc
    subops.[0x15].[0x401] <- Op.Subf__sc
    subops.[0x15].[0x402] <- Op.Mulf__sc
    subops.[0x15].[0x403] <- Op.Divf__sc
    subops.[0x15].[0x41e] <- Op.Cvtdg__sc
    subops.[0x15].[0x420] <- Op.Addg__sc
    subops.[0x15].[0x421] <- Op.Subg__sc
    subops.[0x15].[0x422] <- Op.Mulg__sc
    subops.[0x15].[0x423] <- Op.Divg__sc
    subops.[0x15].[0x42c] <- Op.Cvtgf__sc
    subops.[0x15].[0x42d] <- Op.Cvtgd__sc
    subops.[0x15].[0x42f] <- Op.Cvtgq__sc
    subops.[0x15].[0x480] <- Op.Addf__s
    subops.[0x15].[0x481] <- Op.Subf__s
    subops.[0x15].[0x482] <- Op.Mulf__s
    subops.[0x15].[0x483] <- Op.Divf__s
    subops.[0x15].[0x49e] <- Op.Cvtdg__s
    subops.[0x15].[0x4a0] <- Op.Addg__s
    subops.[0x15].[0x4a1] <- Op.Subg__s
    subops.[0x15].[0x4a2] <- Op.Mulg__s
    subops.[0x15].[0x4a3] <- Op.Divg__s
    subops.[0x15].[0x4a5] <- Op.Cmpgeq__s
    subops.[0x15].[0x4a6] <- Op.Cmpglt__s
    subops.[0x15].[0x4a7] <- Op.Cmpgle__s
    subops.[0x15].[0x4ac] <- Op.Cvtgf__s
    subops.[0x15].[0x4ad] <- Op.Cvtgd__s
    subops.[0x15].[0x4af] <- Op.Cvtgq__s
    subops.[0x15].[0x500] <- Op.Addf__suc
    subops.[0x15].[0x501] <- Op.Subf__suc
    subops.[0x15].[0x502] <- Op.Mulf__suc
    subops.[0x15].[0x503] <- Op.Divf__suc
    subops.[0x15].[0x51e] <- Op.Cvtdg__suc
    subops.[0x15].[0x520] <- Op.Addg__suc
    subops.[0x15].[0x521] <- Op.Subg__suc
    subops.[0x15].[0x522] <- Op.Mulg__suc
    subops.[0x15].[0x523] <- Op.Divg__suc
    subops.[0x15].[0x52c] <- Op.Cvtgf__suc
    subops.[0x15].[0x52d] <- Op.Cvtgd__suc
    subops.[0x15].[0x52f] <- Op.Cvtgq__svc
    subops.[0x15].[0x580] <- Op.Addf__su
    subops.[0x15].[0x581] <- Op.Subf__su
    subops.[0x15].[0x582] <- Op.Mulf__su
    subops.[0x15].[0x583] <- Op.Divf__su
    subops.[0x15].[0x59e] <- Op.Cvtdg__su
    subops.[0x15].[0x5a0] <- Op.Addg__su
    subops.[0x15].[0x5a1] <- Op.Subg__su
    subops.[0x15].[0x5a2] <- Op.Mulg__su
    subops.[0x15].[0x5a3] <- Op.Divg__su
    subops.[0x15].[0x5ac] <- Op.Cvtgf__su
    subops.[0x15].[0x5ad] <- Op.Cvtgd__su
    subops.[0x15].[0x5af] <- Op.Cvtgq__sv

    subops.[0x16].[0x000] <- Op.Adds__c
    subops.[0x16].[0x001] <- Op.Subs__c
    subops.[0x16].[0x002] <- Op.Muls__c
    subops.[0x16].[0x003] <- Op.Divs__c
    subops.[0x16].[0x020] <- Op.Addt__c
    subops.[0x16].[0x021] <- Op.Subt__c
    subops.[0x16].[0x022] <- Op.Mult__c
    subops.[0x16].[0x023] <- Op.Divt__c
    subops.[0x16].[0x02c] <- Op.Cvtts__c
    subops.[0x16].[0x02f] <- Op.Cvttq__c
    subops.[0x16].[0x03c] <- Op.Cvtqs__c
    subops.[0x16].[0x03e] <- Op.Cvtqt__c
    subops.[0x16].[0x040] <- Op.Adds__m
    subops.[0x16].[0x041] <- Op.Subs__m
    subops.[0x16].[0x042] <- Op.Muls__m
    subops.[0x16].[0x043] <- Op.Divs__m
    subops.[0x16].[0x060] <- Op.Addt__m
    subops.[0x16].[0x061] <- Op.Subt__m
    subops.[0x16].[0x062] <- Op.Mult__m
    subops.[0x16].[0x063] <- Op.Divt__m
    subops.[0x16].[0x06c] <- Op.Cvtts__m
    subops.[0x16].[0x06f] <- Op.Cvttq__m
    subops.[0x16].[0x07c] <- Op.Cvtqs__m
    subops.[0x16].[0x07e] <- Op.Cvtqt__m
    subops.[0x16].[0x080] <- Op.Adds
    subops.[0x16].[0x081] <- Op.Subs
    subops.[0x16].[0x082] <- Op.Muls
    subops.[0x16].[0x083] <- Op.Divs
    subops.[0x16].[0x0a0] <- Op.Addt
    subops.[0x16].[0x0a1] <- Op.Subt
    subops.[0x16].[0x0a2] <- Op.Mult
    subops.[0x16].[0x0a3] <- Op.Divt
    subops.[0x16].[0x0a4] <- Op.Cmptun
    subops.[0x16].[0x0a5] <- Op.Cmpteq
    subops.[0x16].[0x0a6] <- Op.Cmptlt
    subops.[0x16].[0x0a7] <- Op.Cmptle
    subops.[0x16].[0x0ac] <- Op.Cvtts
    subops.[0x16].[0x0af] <- Op.Cvttq
    subops.[0x16].[0x0bc] <- Op.Cvtqs
    subops.[0x16].[0x0be] <- Op.Cvtqt
    subops.[0x16].[0x0c0] <- Op.Adds__d
    subops.[0x16].[0x0c1] <- Op.Subs__d
    subops.[0x16].[0x0c2] <- Op.Muls__d
    subops.[0x16].[0x0c3] <- Op.Divs__d
    subops.[0x16].[0x0e0] <- Op.Addt__d
    subops.[0x16].[0x0e1] <- Op.Subt__d
    subops.[0x16].[0x0e2] <- Op.Mult__d
    subops.[0x16].[0x0e3] <- Op.Divt__d
    subops.[0x16].[0x0ec] <- Op.Cvtts__d
    subops.[0x16].[0x0ef] <- Op.Cvttq__d
    subops.[0x16].[0x0fc] <- Op.Cvtqs__d
    subops.[0x16].[0x0fe] <- Op.Cvtqt__d
    subops.[0x16].[0x100] <- Op.Adds__uc
    subops.[0x16].[0x101] <- Op.Subs__uc
    subops.[0x16].[0x102] <- Op.Muls__uc
    subops.[0x16].[0x103] <- Op.Divs__uc
    subops.[0x16].[0x120] <- Op.Addt__uc
    subops.[0x16].[0x121] <- Op.Subt__uc
    subops.[0x16].[0x122] <- Op.Mult__uc
    subops.[0x16].[0x123] <- Op.Divt__uc
    subops.[0x16].[0x12c] <- Op.Cvtts__uc
    subops.[0x16].[0x12f] <- Op.Cvttq__vc
    subops.[0x16].[0x140] <- Op.Adds__um
    subops.[0x16].[0x141] <- Op.Subs__um
    subops.[0x16].[0x142] <- Op.Muls__um
    subops.[0x16].[0x143] <- Op.Divs__um
    subops.[0x16].[0x160] <- Op.Addt__um
    subops.[0x16].[0x161] <- Op.Subt__um
    subops.[0x16].[0x162] <- Op.Mult__um
    subops.[0x16].[0x163] <- Op.Divt__um
    subops.[0x16].[0x16c] <- Op.Cvtts__um
    subops.[0x16].[0x16f] <- Op.Cvttq__vm
    subops.[0x16].[0x180] <- Op.Adds__u
    subops.[0x16].[0x181] <- Op.Subs__u
    subops.[0x16].[0x182] <- Op.Muls__u
    subops.[0x16].[0x183] <- Op.Divs__u
    subops.[0x16].[0x1a0] <- Op.Addt__u
    subops.[0x16].[0x1a1] <- Op.Subt__u
    subops.[0x16].[0x1a2] <- Op.Mult__u
    subops.[0x16].[0x1a3] <- Op.Divt__u
    subops.[0x16].[0x1ac] <- Op.Cvtts__u
    subops.[0x16].[0x1af] <- Op.Cvttq__v
    subops.[0x16].[0x1c0] <- Op.Adds__ud
    subops.[0x16].[0x1c1] <- Op.Subs__ud
    subops.[0x16].[0x1c2] <- Op.Muls__ud
    subops.[0x16].[0x1c3] <- Op.Divs__ud
    subops.[0x16].[0x1e0] <- Op.Addt__ud
    subops.[0x16].[0x1e1] <- Op.Subt__ud
    subops.[0x16].[0x1e2] <- Op.Mult__ud
    subops.[0x16].[0x1e3] <- Op.Divt__ud
    subops.[0x16].[0x1ec] <- Op.Cvtts__ud
    subops.[0x16].[0x1ef] <- Op.Cvttq__vd
    subops.[0x16].[0x2ac] <- Op.Cvtst
    subops.[0x16].[0x500] <- Op.Adds__suc
    subops.[0x16].[0x501] <- Op.Subs__suc
    subops.[0x16].[0x502] <- Op.Muls__suc
    subops.[0x16].[0x503] <- Op.Divs__suc
    subops.[0x16].[0x520] <- Op.Addt__suc
    subops.[0x16].[0x521] <- Op.Subt__suc
    subops.[0x16].[0x522] <- Op.Mult__suc
    subops.[0x16].[0x523] <- Op.Divt__suc
    subops.[0x16].[0x52c] <- Op.Cvtts__suc
    subops.[0x16].[0x52f] <- Op.Cvttq__svc
    subops.[0x16].[0x540] <- Op.Adds__sum
    subops.[0x16].[0x541] <- Op.Subs__sum
    subops.[0x16].[0x542] <- Op.Muls__sum
    subops.[0x16].[0x543] <- Op.Divs__sum
    subops.[0x16].[0x560] <- Op.Addt__sum
    subops.[0x16].[0x561] <- Op.Subt__sum
    subops.[0x16].[0x562] <- Op.Mult__sum
    subops.[0x16].[0x563] <- Op.Divt__sum
    subops.[0x16].[0x56c] <- Op.Cvtts__sum
    subops.[0x16].[0x56f] <- Op.Cvttq__svm
    subops.[0x16].[0x580] <- Op.Adds__su
    subops.[0x16].[0x581] <- Op.Subs__su
    subops.[0x16].[0x582] <- Op.Muls__su
    subops.[0x16].[0x583] <- Op.Divs__su
    subops.[0x16].[0x5a0] <- Op.Addt__su
    subops.[0x16].[0x5a1] <- Op.Subt__su
    subops.[0x16].[0x5a2] <- Op.Mult__su
    subops.[0x16].[0x5a3] <- Op.Divt__su
    subops.[0x16].[0x5a4] <- Op.Cmptun__su
    subops.[0x16].[0x5a5] <- Op.Cmpteq__su
    subops.[0x16].[0x5a6] <- Op.Cmptlt__su
    subops.[0x16].[0x5a7] <- Op.Cmptle__su
    subops.[0x16].[0x5ac] <- Op.Cvtts__su
    subops.[0x16].[0x5af] <- Op.Cvttq__sv
    subops.[0x16].[0x5c0] <- Op.Adds__sud
    subops.[0x16].[0x5c1] <- Op.Subs__sud
    subops.[0x16].[0x5c2] <- Op.Muls__sud
    subops.[0x16].[0x5c3] <- Op.Divs__sud
    subops.[0x16].[0x5e0] <- Op.Addt__sud
    subops.[0x16].[0x5e1] <- Op.Subt__sud
    subops.[0x16].[0x5e2] <- Op.Mult__sud
    subops.[0x16].[0x5e3] <- Op.Divt__sud
    subops.[0x16].[0x5ec] <- Op.Cvtts__sud
    subops.[0x16].[0x5ef] <- Op.Cvttq__svd
    subops.[0x16].[0x6ac] <- Op.Cvtst__s
    subops.[0x16].[0x700] <- Op.Adds__suic
    subops.[0x16].[0x701] <- Op.Subs__suic
    subops.[0x16].[0x702] <- Op.Muls__suic
    subops.[0x16].[0x703] <- Op.Divs__suic
    subops.[0x16].[0x720] <- Op.Addt__suic
    subops.[0x16].[0x721] <- Op.Subt__suic
    subops.[0x16].[0x722] <- Op.Mult__suic
    subops.[0x16].[0x723] <- Op.Divt__suic
    subops.[0x16].[0x72c] <- Op.Cvtts__suic
    subops.[0x16].[0x72f] <- Op.Cvttq__svic
    subops.[0x16].[0x73c] <- Op.Cvtqs__suic
    subops.[0x16].[0x73e] <- Op.Cvtqt__suic
    subops.[0x16].[0x740] <- Op.Adds__suim
    subops.[0x16].[0x741] <- Op.Subs__suim
    subops.[0x16].[0x742] <- Op.Muls__suim
    subops.[0x16].[0x743] <- Op.Divs__suim
    subops.[0x16].[0x760] <- Op.Addt__suim
    subops.[0x16].[0x761] <- Op.Subt__suim
    subops.[0x16].[0x762] <- Op.Mult__suim
    subops.[0x16].[0x763] <- Op.Divt__suim
    subops.[0x16].[0x76c] <- Op.Cvtts__suim
    subops.[0x16].[0x76f] <- Op.Cvttq__svim
    subops.[0x16].[0x77c] <- Op.Cvtqs__suim
    subops.[0x16].[0x77e] <- Op.Cvtqt__suim
    subops.[0x16].[0x780] <- Op.Adds__sui
    subops.[0x16].[0x781] <- Op.Subs__sui
    subops.[0x16].[0x782] <- Op.Muls__sui
    subops.[0x16].[0x783] <- Op.Divs__sui
    subops.[0x16].[0x7a0] <- Op.Addt__sui
    subops.[0x16].[0x7a1] <- Op.Subt__sui
    subops.[0x16].[0x7a2] <- Op.Mult__sui
    subops.[0x16].[0x7a3] <- Op.Divt__sui
    subops.[0x16].[0x7ac] <- Op.Cvtts__sui
    subops.[0x16].[0x7af] <- Op.Cvttq__svi
    subops.[0x16].[0x7bc] <- Op.Cvtqs__sui
    subops.[0x16].[0x7be] <- Op.Cvtqt__sui
    subops.[0x16].[0x7c0] <- Op.Adds__suid
    subops.[0x16].[0x7c1] <- Op.Subs__suid
    subops.[0x16].[0x7c2] <- Op.Muls__suid
    subops.[0x16].[0x7c3] <- Op.Divs__suid
    subops.[0x16].[0x7e0] <- Op.Addt__suid
    subops.[0x16].[0x7e1] <- Op.Subt__suid
    subops.[0x16].[0x7e2] <- Op.Mult__suid
    subops.[0x16].[0x7e3] <- Op.Divt__suid
    subops.[0x16].[0x7ec] <- Op.Cvtts__suid
    subops.[0x16].[0x7ef] <- Op.Cvttq__svid
    subops.[0x16].[0x7fc] <- Op.Cvtqs__suid
    subops.[0x16].[0x7fe] <- Op.Cvtqt__suid

    subops.[0x17].[0x010] <- Op.Cvtlq
    subops.[0x17].[0x020] <- Op.Cpys
    subops.[0x17].[0x021] <- Op.Cpysn
    subops.[0x17].[0x022] <- Op.Cpyse
    subops.[0x17].[0x024] <- Op.Mt_fpcr
    subops.[0x17].[0x025] <- Op.Mf_fpcr
    subops.[0x17].[0x02a] <- Op.Fcmoveq
    subops.[0x17].[0x02b] <- Op.Fcmovne
    subops.[0x17].[0x02c] <- Op.Fcmovlt
    subops.[0x17].[0x02d] <- Op.Fcmovge
    subops.[0x17].[0x02e] <- Op.Fcmovle
    subops.[0x17].[0x02f] <- Op.Fcmovgt
    subops.[0x17].[0x030] <- Op.Cvtql
    subops.[0x17].[0x130] <- Op.Cvtql__v
    subops.[0x17].[0x530] <- Op.Cvtql__sv

    //subops.[0x18].[0x0000] <- Op.Trapb
    //subops.[0x18].[0x0400] <- Op.Excb
    //subops.[0x18].[0x4000] <- Op.Mb
    //subops.[0x18].[0x4400] <- Op.Wmb
    //subops.[0x18].[0x8000] <- Op.Fetch
    //subops.[0x18].[0xa000] <- Op.Fetch_m
    //subops.[0x18].[0xc000] <- Op.Rpcc
    //subops.[0x18].[0xe000] <- Op.Rc
    //subops.[0x18].[0xe800] <- Op.Ecb
    //subops.[0x18].[0xf000] <- Op.Rs
    //subops.[0x18].[0xf800] <- Op.Wh64
    //subops.[0x18].[0xfc00] <- Op.Wh64en

    subops.[0x1a].[0x0] <- Op.Jmp
    subops.[0x1a].[0x1] <- Op.Jsr
    subops.[0x1a].[0x2] <- Op.Ret
    subops.[0x1a].[0x3] <- Op.Jsr_coroutine

    subops.[0x1c].[0x00] <- Op.Sextb
    subops.[0x1c].[0x01] <- Op.Sextw
    subops.[0x1c].[0x30] <- Op.Ctpop
    subops.[0x1c].[0x31] <- Op.Perr
    subops.[0x1c].[0x32] <- Op.Ctlz
    subops.[0x1c].[0x33] <- Op.Cttz
    subops.[0x1c].[0x34] <- Op.Unpkbw
    subops.[0x1c].[0x35] <- Op.Unpkbl
    subops.[0x1c].[0x36] <- Op.Pkwb
    subops.[0x1c].[0x37] <- Op.Pklb
    subops.[0x1c].[0x38] <- Op.Minsb8
    subops.[0x1c].[0x39] <- Op.Minsw4
    subops.[0x1c].[0x3a] <- Op.Minub8
    subops.[0x1c].[0x3b] <- Op.Minuw4
    subops.[0x1c].[0x3c] <- Op.Maxub8
    subops.[0x1c].[0x3d] <- Op.Maxuw4
    subops.[0x1c].[0x3e] <- Op.Maxsb8
    subops.[0x1c].[0x3f] <- Op.Maxsw4
    subops.[0x1c].[0x70] <- Op.Ftoit
    subops.[0x1c].[0x78] <- Op.Ftois
    
    subops
