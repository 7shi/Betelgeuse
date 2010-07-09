module Alpha.Table

open System

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
    let mktbl op n =
        let h = op <<< 16
        [| for i in 0..n ->
            if Enum.GetName(typeof<Op>, h + i) = null then Op.___ else enum<Op>(h + i) |]
    [| for i in 0..0x3f ->
        match i with
        | 0x10 -> mktbl i 0x80
        | 0x11 -> mktbl i 0x80
        | 0x12 -> mktbl i 0x80
        | 0x13 -> mktbl i 0x80
        | 0x14 -> mktbl i 0x800
        | 0x15 -> mktbl i 0x800
        | 0x16 -> mktbl i 0x800
        | 0x17 -> mktbl i 0x800
        //| 0x18 -> mktbl i 0x10000
        | 0x1a -> mktbl i 0x4
        | 0x1c -> mktbl i 0x80
        | _ -> null |]
