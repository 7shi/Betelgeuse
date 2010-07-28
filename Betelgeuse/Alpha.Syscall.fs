module Alpha.Syscall

open System
open System.IO
open System.Text

open Alpha
open Alpha.Memory

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

let exit (vm:VM) =
    vm.pc <- stackEnd

let _fputc (vm:VM) (ch:byte) (f:int) =
    if f = 0 then
        if ch = byte('\n') then
            vm.out.WriteLine()
        else
            vm.out.Write("{0}", ch |> char)
        1UL
    else
        let fs = getSlot f
        if box fs = null then uint64(-1)
        else
            try
                if ch = byte('\n') && (attrs.[f] &&& aText) <> 0 then
                    fs.WriteByte ('\r' |> byte)
                fs.WriteByte ch
                1UL
            with _ -> uint64(-1)

let fputc (vm:VM) =
    vm.v0 <- _fputc vm (vm.a0 |> byte) (vm.a1 |> int)

let fgetc (vm:VM) =
    vm.v0 <-
        let f = vm.a0 |> int
        let fs = getSlot f
        if box fs = null then uint64(-1)
        else
            try
                let ch = fs.ReadByte()
                if ch = int('\r') && (attrs.[f] &&& aText) <> 0 then
                    fs.ReadByte() |> uint64
                else
                    ch |> uint64
            with _ -> uint64(-1)

let fopen (vm:VM) =
    vm.v0 <-
        let fn = readString vm vm.a0
        let md = readString vm vm.a1
        let f = openSlot()
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

let fclose (vm:VM) =
    vm.v0 <-
        let f = vm.a0 |> int
        let fs = getSlot f
        if box fs = null then
            uint64(-1)
        else
            closeFile.Invoke(fname.[f], fs, attrs.[f])
            slots.[f] <- Unchecked.defaultof<Stream>
            fname.[f] <- Unchecked.defaultof<string>
            attrs.[f] <- 0
            0UL

let fwrite (vm:VM) =
    vm.v0 <-
        let p = vm.a0
        let s = vm.a1 |> int
        let n = vm.a2 |> int
        let f = vm.a3 |> int
        let fs = getSlot f
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

let fread (vm:VM) =
    vm.v0 <-
        let p = vm.a0
        let s = vm.a1 |> int
        let n = vm.a2 |> int
        let f = vm.a3 |> int
        let fs = getSlot f
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

let fseek (vm:VM) =
    vm.v0 <-
        let f = vm.a0 |> int
        let o = vm.a1 |> int64
        let s = vm.a2 |> int
        let fs = getSlot f
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

let _fsnputc (vm:VM) (ch:byte) (f:int) (psb:uint64[]) (plen:uint64[]) =
    if psb = null then
        _fputc vm ch f |> ignore
        1
    else
        let len = plen.[0]
        if len = 0UL then 0
        else
            plen.[0] <- len - 1UL
            let sb = psb.[0]
            psb.[0] <- sb + 1UL
            if len = 1UL then
                write8 vm sb 0uy
                0
            else
                write8 vm sb ch
                1

let _fsnprintstr (vm:VM) (buf:byte[]) (ptr:int) (f:int) (psb:uint64[]) (plen:uint64[]) =
    if f = 0 && psb = null then
        let rec count p =
            if buf.[ptr + p] = 0uy then p else count (p + 1)
        let len = count 0
        vm.out.Write(Encoding.UTF8.GetString(buf, ptr, len))
        len
    else
        let rec write p =
            let ch = buf.[ptr + p]
            if ch = 0uy || (_fsnputc vm ch f psb plen) = 0 then p
            else write (p + 1)
        write 0

let _fsnprintlong (vm:VM) (v:int64) (w:int) (zero:bool) (f:int) (psb:uint64[]) (plen:uint64[]) =
    let str =
        if w = 0 then
            v.ToString()
        else if zero then
            String.Format("{0:d" + w.ToString() + "}", v)
        else
            String.Format("{0," + w.ToString() + "}", v)
    let buf = Encoding.UTF8.GetBytes(str + "\u0000")
    _fsnprintstr vm buf 0 f psb plen

let _fsnprinthex (vm:VM) (v:uint64) (w:int) (zero:bool) (f:int) (psb:uint64[]) (plen:uint64[]) =
    let str =
        if w = 0 then
            v.ToString("x")
        else if zero then
            String.Format("{0:x" + w.ToString() + "}", v)
        else
            String.Format("{0," + w.ToString() + ":x}", v)
    let buf = Encoding.UTF8.GetBytes(str + "\u0000")
    _fsnprintstr vm buf 0 f psb plen

let _parseint (buf:byte[]) (ptr:int) =
    let rec parse b p =
        let ch = buf.[ptr + p]
        if ch < byte('0') || ch > byte('9') then b, p
        else parse (b * 10 + int(ch - byte('0'))) (p + 1)
    parse 0 0

let _vfsnprintf (vm:VM) (f:int) (psb:uint64[]) (plen:uint64[]) (format:uint64) (args:uint64[]) =
    let get_arg n =
        if n < args.Length then args.[n] else read64 vm (vm.sp + uint64((n - args.Length) * 8))
    let mp = getPtr vm format 1
    let rec write len p arg =
        let ch = mp.buf.[mp.ptr + p]
        let da, dp, dl =
            match ch |> char with
            | '\u0000' ->
                if psb <> null then _fsnputc vm ch f psb plen |> ignore
                0, 0, 0
            | '%' ->
                let ch2 = mp.buf.[mp.ptr + p + 1]
                match ch2 |> char with
                | 'd' ->
                    1, 2, _fsnprintlong vm (int64 (get_arg arg)) 0 false f psb plen
                | 'x' ->
                    1, 2, _fsnprinthex vm (get_arg arg) 0 false f psb plen
                | 'p' ->
                    1, 2, (_fsnprintstr vm [| byte('0'); byte('x'); 0uy |] 0 f psb plen) +
                          (_fsnprinthex vm (get_arg arg) 16 true f psb plen)
                | 'c' ->
                    1, 2, _fsnputc vm (byte (get_arg arg)) f psb plen
                | 's' ->
                    let mp = getPtr vm (get_arg arg) 1
                    1, 2, _fsnprintstr vm mp.buf mp.ptr f psb plen
                | '%' ->
                    _fsnputc vm ch f psb plen |> ignore
                    0, 2, 1
                | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                    let zero = ch2 = byte('0')
                    let w, wlen = _parseint mp.buf (mp.ptr + p + 1)
                    let ch3 = mp.buf.[mp.ptr + p + 1 + wlen]
                    match ch3 |> char with
                    | 'd' ->
                        1, 2 + wlen, _fsnprintlong vm (int64 (get_arg arg)) w zero f psb plen
                    | 'x' ->
                        1, 2 + wlen, _fsnprinthex vm (get_arg arg) w zero f psb plen
                    | _ ->
                        _fsnputc vm ch f psb plen |> ignore
                        0, 1, 1
                | _ ->
                    _fsnputc vm ch f psb plen |> ignore
                    0, 1, 1
            | _ ->
                _fsnputc vm ch f psb plen |> ignore
                0, 1, 1
        if dl = 0 then len else write (len + dl) (p + dp) (arg + da)
    write 0 0 0 |> uint64

let printf (vm:VM) =
    vm.v0 <- _vfsnprintf vm 0 null null vm.a0 [| vm.a1; vm.a2; vm.a3; vm.a4; vm.a5 |]

let fprintf (vm:VM) =
    vm.v0 <- _vfsnprintf vm (int vm.a0) null null vm.a1 [| vm.a2; vm.a3; vm.a4; vm.a5 |]

let snprintf (vm:VM) =
    let psb = [| vm.a0 |]
    let plen = [| vm.a1 |]
    vm.v0 <- _vfsnprintf vm 0 psb plen vm.a2 [| vm.a3; vm.a4; vm.a5 |]

let strcmp (vm:VM) =
    vm.v0 <- 
        let rec cmp a b =
            let va = read8 vm a
            let vb = read8 vm b
            if va = 0uy && vb = 0uy then 0
            else if va < vb then -1
            else if va > vb then 1
            else cmp (a + 1UL) (b + 1UL)
        cmp vm.a0 vm.a1 |> uint64

let funcs =
    [| exit
       fputc
       fgetc
       fopen
       fclose
       fwrite
       fread
       fseek
       printf
       fprintf
       snprintf
       strcmp |]

let funcStart = 0x00ef0000UL
let funcEnd = funcStart + uint64(funcs.Length * 4)

let callFunc (vm:VM) =
    let f = int(vm.pc - funcStart) >>> 2
    vm.pc <- vm.ra
    if f < 0 || f >= funcs.Length then
        raise << vm.Abort <| sprintf "[Syscall] 未実装: %x" f
    else
        funcs.[f] vm
