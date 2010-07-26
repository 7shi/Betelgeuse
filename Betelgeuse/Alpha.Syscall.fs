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

let mutable openWrite = new Func<string, Stream>(fun fn -> new FileStream(fn, FileMode.Create) :> Stream)
let mutable openRead  = new Func<string, Stream>(fun fn -> new FileStream(fn, FileMode.Open  ) :> Stream)
let mutable closeFile = new Action<string, Stream, int>(fun _ s _ -> s.Dispose())

let slots = Array.zeroCreate<Stream> 32
let fname = Array.zeroCreate<string> slots.Length
let attrs = Array.zeroCreate<int> slots.Length
let aText = 1
let aWrite = 2

let openSlot() =
    let rec search f =
        if f >= slots.Length then 0
        else if box slots.[f] = null then f
        else search(f + 1)
    search 1

let getSlot (f:int) =
    if f < 0 || f > slots.Length || box slots.[f] = null then
        slots.[0] // always null
    else
        slots.[f]

let callFunc (vm:VM) =
    let func = int(vm.pc - funcStart) >>> 2
    match func |> enum<Funcs> with
    | Funcs.Exit ->
        vm.pc <- stackEnd
    | Funcs.FPutC ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        let c = vm.reg.[Regs.A0 |> int] |> char
        let f = vm.reg.[Regs.A1 |> int] |> int
        vm.reg.[Regs.V0 |> int] <-
            if f = 0 then
                if c = '\n' then
                    vm.out.WriteLine()
                else
                    vm.out.Write("{0}", c)
                1UL
            else
                let fs = getSlot f
                if box fs = null then uint64(-1)
                else
                    try
                        if c = '\n' && (attrs.[f] &&& aText) <> 0 then
                            fs.WriteByte ('\r' |> byte)
                        fs.WriteByte (c |> byte)
                        1UL
                    with _ -> uint64(-1)
    | Funcs.FGetC ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        let f = vm.reg.[Regs.A0 |> int] |> int
        let fs = getSlot f
        vm.reg.[Regs.V0 |> int] <-
            if box fs = null then uint64(-1)
            else
                try
                    let ch = fs.ReadByte()
                    if ch = int('\r') && (attrs.[f] &&& aText) <> 0 then
                        fs.ReadByte() |> uint64
                    else
                        ch |> uint64
                with _ -> uint64(-1)
    | Funcs.FOpen ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        let fn = readString vm vm.reg.[Regs.A0 |> int]
        let md = readString vm vm.reg.[Regs.A1 |> int]
        let f = openSlot()
        vm.reg.[Regs.V0 |> int] <-
            if f = 0 then 0UL
            else
                try
                    let fs, a =
                        match md with
                        | "r"  -> openRead.Invoke(fn), aText
                        | "rb" -> openRead.Invoke(fn), 0
                        | "w"  -> openWrite.Invoke(fn), aWrite ||| aText
                        | "wb" -> openWrite.Invoke(fn), aWrite
                        | _    -> slots.[0], 0
                    if box fs = null then 0UL
                    else
                        slots.[f] <- fs
                        fname.[f] <- fn
                        attrs.[f] <- a
                        f |> uint64
                with _ -> 0UL
    | Funcs.FClose ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        let f = vm.reg.[Regs.A0 |> int] |> int
        let fs = getSlot f
        vm.reg.[Regs.V0 |> int] <-
            if box fs = null then
                uint64(-1)
            else
                closeFile.Invoke(fname.[f], fs, attrs.[f])
                slots.[f] <- Unchecked.defaultof<Stream>
                fname.[f] <- Unchecked.defaultof<string>
                attrs.[f] <- 0
                0UL
    | Funcs.FWrite ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        let p = vm.reg.[Regs.A0 |> int]
        let s = vm.reg.[Regs.A1 |> int] |> int
        let n = vm.reg.[Regs.A2 |> int] |> int
        let f = vm.reg.[Regs.A3 |> int] |> int
        let fs = getSlot f
        vm.reg.[Regs.V0 |> int] <-
            if box fs = null then 0UL
            else
                let mp = getPtr vm p (s * n)
                let rec write i =
                    let ok =
                        try
                            fs.Write(mp.buf, mp.ptr + i * s, s)
                            true
                        with _ ->
                            false
                    if not ok then i
                    else if i >= n - 1 then i + 1
                    else write (i + 1)
                write 0 |> uint64
    | Funcs.FRead ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        let p = vm.reg.[Regs.A0 |> int]
        let s = vm.reg.[Regs.A1 |> int] |> int
        let n = vm.reg.[Regs.A2 |> int] |> int
        let f = vm.reg.[Regs.A3 |> int] |> int
        let fs = getSlot f
        vm.reg.[Regs.V0 |> int] <-
            if box fs = null then 0UL
            else
                let mp = getPtr vm p (s * n)
                let rec read i =
                    let ok =
                        try
                            fs.Read(mp.buf, mp.ptr + i * s, s) = s
                        with _ ->
                            false
                    if not ok then i
                    else if i >= n - 1 then i + 1
                    else read (i + 1)
                read 0 |> uint64
    | Funcs.FSeek ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        let f = vm.reg.[Regs.A0 |> int] |> int
        let o = vm.reg.[Regs.A1 |> int] |> int64
        let s = vm.reg.[Regs.A2 |> int] |> int
        let fs = getSlot f
        vm.reg.[Regs.V0 |> int] <-
            if box fs = null then 1UL
            else
                try
                    match s with
                    | 0 ->
                        fs.Position <- o
                        0UL
                    | 1 ->
                        fs.Position <- fs.Position + o
                        0UL
                    | 2 ->
                        fs.Position <- fs.Length + o
                        0UL
                    | _ ->
                        1UL
                with _ -> 1UL
    | _ ->
        vm.pc <- vm.reg.[Regs.RA |> int]
        raise << vm.Abort <| sprintf "[Syscall] 未実装: %x" func
