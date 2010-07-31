module Alpha.Syscall

open System
open System.IO
open System.Text

open Alpha
open Alpha.Memory

let mutable openWrite = new Func<string, Stream>(fun fn -> new FileStream(fn, FileMode.Create) :> Stream)
let mutable openRead  = new Func<string, Stream>(fun fn -> new FileStream(fn, FileMode.Open  ) :> Stream)
let mutable closeFile = new Action<string, Stream, int>(fun _ s _ -> s.Dispose())
let mutable execStep  = fun (vm:VM) -> ()

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

let _fgetc_stream (fs:Stream) attr =
    try
        let ch = fs.ReadByte()
        if ch = int('\r') && (attr &&& aText) <> 0 then
            fs.ReadByte() |> int
        else
            ch |> int
    with _ -> -1

let _fgetc f =
    let fs = getSlot f
    if box fs = null then -1 else _fgetc_stream fs attrs.[f]

let fgetc (vm:VM) = vm.v0 <- uint64 <| _fgetc (int vm.a0)

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

let _fsnputc vm (ch:byte) (f:int) (sb:byte[]) (sp:int[]) =
    if sp = null then
        _fputc vm ch f |> ignore
        1
    else
        let len = sp.[1]
        if len = 0 then 0
        else
            let p = sp.[0]
            sp.[0] <- p + 1
            sp.[1] <- len - 1
            if len = 1 then
                sb.[p] <- 0uy
                0
            else
                sb.[p] <- ch
                1

let _fsnprintstr vm (buf:byte[]) (ptr:int) f sb sp =
    if f = 0 && sb = null then
        let rec count p =
            if buf.[ptr + p] = 0uy then p else count (p + 1)
        let len = count 0
        vm.out.Write(Encoding.UTF8.GetString(buf, ptr, len).Replace("\n", "\r\n"))
        len
    else
        let rec write p =
            let ch = buf.[ptr + p]
            if ch = 0uy || (_fsnputc vm ch f sb sp) = 0 then p
            else write (p + 1)
        write 0

let _fsnprintlong vm (v:int64) (w:int) (zero:bool) f sb sp =
    let str =
        if w = 0 then
            v.ToString()
        else if zero then
            String.Format("{0:d" + w.ToString() + "}", v)
        else
            String.Format("{0," + w.ToString() + "}", v)
    let buf = Encoding.UTF8.GetBytes(str + "\u0000")
    _fsnprintstr vm buf 0 f sb sp

let _fsnprinthex vm (v:uint64) (w:int) (zero:bool) f sb sp =
    let str =
        if w = 0 then
            v.ToString("x")
        else if zero then
            String.Format("{0:x" + w.ToString() + "}", v)
        else
            String.Format("{0," + w.ToString() + ":x}", v)
    let buf = Encoding.UTF8.GetBytes(str + "\u0000")
    _fsnprintstr vm buf 0 f sb sp

let _parseint (buf:byte[]) (ptr:int) =
    let rec parse b p =
        let ch = buf.[ptr + p]
        if ch < byte('0') || ch > byte('9') then b, p
        else parse (b * 10 + int(ch - byte('0'))) (p + 1)
    parse 0 0

let _vfsnprintf vm f sb sp (format:uint64) (args:uint64[]) =
    let get_arg n =
        if n < args.Length then args.[n] else read64 vm (vm.sp + uint64((n - args.Length) * 8))
    let mp = getPtr vm format 1
    let rec write len p arg =
        let ch = mp.buf.[mp.ptr + p]
        let da, dp, dl =
            match ch |> char with
            | '\u0000' ->
                if sb <> null then _fsnputc vm ch f sb sp |> ignore
                0, 0, 0
            | '%' ->
                let ch2 = mp.buf.[mp.ptr + p + 1]
                match ch2 |> char with
                | 'd' ->
                    1, 2, _fsnprintlong vm (int64 (get_arg arg)) 0 false f sb sp
                | 'x' ->
                    1, 2, _fsnprinthex vm (get_arg arg) 0 false f sb sp
                | 'p' ->
                    1, 2, (_fsnprintstr vm [| byte('0'); byte('x'); 0uy |] 0 f sb sp) +
                          (_fsnprinthex vm (get_arg arg) 16 true f sb sp)
                | 'c' ->
                    1, 2, _fsnputc vm (byte (get_arg arg)) f sb sp
                | 's' ->
                    let mp = getPtr vm (get_arg arg) 1
                    1, 2, _fsnprintstr vm mp.buf mp.ptr f sb sp
                | '%' ->
                    _fsnputc vm ch f sb sp |> ignore
                    0, 2, 1
                | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                    let zero = ch2 = byte('0')
                    let w, wlen = _parseint mp.buf (mp.ptr + p + 1)
                    let ch3 = mp.buf.[mp.ptr + p + 1 + wlen]
                    match ch3 |> char with
                    | 'd' ->
                        1, 2 + wlen, _fsnprintlong vm (int64 (get_arg arg)) w zero f sb sp
                    | 'x' ->
                        1, 2 + wlen, _fsnprinthex vm (get_arg arg) w zero f sb sp
                    | _ ->
                        _fsnputc vm ch f sb sp |> ignore
                        0, 1, 1
                | _ ->
                    _fsnputc vm ch f sb sp |> ignore
                    0, 1, 1
            | _ ->
                _fsnputc vm ch f sb sp |> ignore
                0, 1, 1
        if dl = 0 then len else write (len + dl) (p + dp) (arg + da)
    write 0 0 0 |> uint64

let printf (vm:VM) =
    vm.v0 <- _vfsnprintf vm 0 null null vm.a0 [| vm.a1; vm.a2; vm.a3; vm.a4; vm.a5 |]

let fprintf (vm:VM) =
    vm.v0 <- _vfsnprintf vm (int vm.a0) null null vm.a1 [| vm.a2; vm.a3; vm.a4; vm.a5 |]

let snprintf (vm:VM) =
    let mp = getPtr vm vm.a0 1
    let sp = [| mp.ptr; int vm.a1 |]
    vm.v0 <- _vfsnprintf vm 0 mp.buf sp vm.a2 [| vm.a3; vm.a4; vm.a5 |]

let _strcmp (vm:VM) (a:uint64) (b:uint64) = (readString vm a).CompareTo(readString vm b)
let strcmp (vm:VM) = vm.v0 <- uint64 <| _strcmp vm vm.a0 vm.a1

let _stricmp (vm:VM) (a:uint64) (b:uint64) = (readString vm a).ToLower().CompareTo((readString vm b).ToLower())
let stricmp (vm:VM) = vm.v0 <- uint64 <| _stricmp vm vm.a0 vm.a1

let _strncpy (vm:VM) (mp1:Ptr) (mp2:Ptr) len =
    let rec ncpy i len =
        if len > 0 then
            let v = mp2.buf.[mp2.ptr + i]
            mp1.buf.[mp1.ptr + i] <- v
            if v <> 0uy then ncpy (i + 1) (len - 1)
    ncpy 0 len
    vm.a0

let strncpy (vm:VM) =
    vm.v0 <- _strncpy vm (getPtr vm vm.a0 1) (getPtr vm vm.a1 1) (int vm.a2)

let strncat (vm:VM) =
    let mp1 = getPtr vm vm.a0 1
    let mp2 = getPtr vm vm.a1 1
    let rec ncat i len =
        if len > 0 then
            if mp1.buf.[mp1.ptr + i] = 0uy then
                 _strncpy vm { buf = mp1.buf; ptr = mp1.ptr + i } mp2 len |> ignore
            else
                ncat (i + 1) (len - 1)
    ncat 0 (int vm.a2)
    vm.v0 <- vm.a0

let strlen (vm:VM) =
    let mp = getPtr vm vm.a0 1
    let rec len i =
        if mp.buf.[mp.ptr + i] = 0uy then i else len (i + 1)
    vm.v0 <- uint64 <| len 0

let memcpy (vm:VM) =
    let mp1 = getPtr vm vm.a0 1
    let mp2 = getPtr vm vm.a1 1
    array.Copy(mp2.buf, mp2.ptr, mp1.buf, mp1.ptr, int vm.a2);
    vm.v0 <- vm.a0

let memset (vm:VM) =
    let mp = getPtr vm vm.a0 1
    let b = vm.a1 |> byte
    let len = (vm.a2 |> int) - 1
    for i = 0 to len do mp.buf.[mp.ptr + i] <- b
    vm.v0 <- vm.a0

let pseudoProc (vm:VM) (proc:unit->uint64) =
    let pc = vm.pc
    vm.sp <- vm.sp - 16UL
    write64 vm vm.sp vm.ra
    write64 vm (vm.sp + 8UL) vm.fp
    vm.fp <- vm.sp
    vm.v0 <- proc()
    vm.sp <- vm.fp
    vm.ra <- read64 vm vm.sp
    vm.fp <- read64 vm (vm.sp + 8UL)
    vm.sp <- vm.sp + 16UL
    vm.pc <- pc

let pseudoCall (vm:VM) (ra:uint64) (proc:uint64) (a0:uint64) =
    vm.a0 <- a0
    vm.ra <- ra
    vm.pv <- proc
    vm.pc <- proc
    let fp = vm.fp
    while vm.pc <> ra || vm.fp <> fp do execStep vm

let pseudoCall_2 (vm:VM) (ra:uint64) (proc:uint64) (a0:uint64) (a1:uint64) =
    vm.a1 <- a1
    pseudoCall vm ra proc a0

let rec _lfind (vm:VM) (key:uint64) (base':uint64) num width proc =
    if num < 1 then 0UL
    else if (proc vm key base') = 0 then base'
    else _lfind vm key (base' + width) (num - 1) width proc

let lfind (vm:VM) =
    let key = vm.a0
    let base' = vm.a1
    let num = read32 vm vm.a2 |> int
    let width = vm.a3
    let compare = vm.a4
    if compare = 0x00ef002cUL then
        vm.v0 <- _lfind vm key base' num width _strcmp
    else if compare = 0x00ef004cUL then
        vm.v0 <- _lfind vm key base' num width _stricmp
    else
        pseudoProc vm (fun() ->
            _lfind vm key base' num width (fun vm a b ->
                pseudoCall_2 vm (0x00ef0048UL + 4UL) compare a b
                int << int64 <| vm.v0))

let rec _bsearch (vm:VM) (key:uint64) (base':uint64) num width proc =
    if num <= 4 then _lfind vm key base' num width proc else
        let center = (num - 1) / 2
        let pcenter = base' + uint64(center) * width
        let cmp = proc vm key pcenter
        if cmp = 0 then
            pcenter
        else if cmp < 0 then
            _bsearch vm key base' center width proc
        else 
            _bsearch vm key (pcenter + width) (num - center - 1) width proc

let bsearch (vm:VM) =
    let key = vm.a0
    let base' = vm.a1
    let num = vm.a2 |> int
    let width = vm.a3
    let compare = vm.a4
    if compare = 0x00ef002cUL then
        vm.v0 <- _bsearch vm key base' num width _strcmp
    else if compare = 0x00ef004cUL then
        vm.v0 <- _bsearch vm key base' num width _stricmp
    else
        pseudoProc vm (fun() ->
            _bsearch vm key base' num width (fun vm a b ->
                pseudoCall_2 vm (0x00ef0048UL + 4UL) compare a b
                int << int64 <| vm.v0))

let strtoul (vm:VM) =
    let p = vm.a0
    let ep = vm.a1
    let b = vm.a2
    let mp = getPtr vm p 1
    let rec loop1 v i b =
        let ch = mp.buf.[mp.ptr + i] |> char
        let n =
            if Char.IsDigit(ch) then int(ch) - int('0')
            else if 'A' <= ch && ch <= 'Z' then int(ch) - int('A') + 10
            else if 'a' <= ch && ch <= 'z' then int(ch) - int('a') + 10
            else -1
        if n = -1 || n >= int(b) then
            write64 vm ep (p + uint64(i))
            v
        else
            loop1 (v * b + uint64(n)) (i + 1) b
    let rec loop2 i =
        let ch = mp.buf.[mp.ptr + i]
        if ch = 0uy then 0UL
        else if ch > 32uy then
            if b = 0UL then
                if ch = byte('0') then
                    let ch2 = mp.buf.[mp.ptr + i + 1] |> char
                    if ch2 = 'x' || ch2 = 'X' then loop1 0UL (i + 2) 16UL
                    else loop1 0UL (i + 1) 8UL
                else loop1 0UL i 10UL
            else
                loop1 0UL i b
        else loop2 (i + 1)
    vm.v0 <- loop2 0

let _isdigit ch = '0' <= ch && ch <= '9'
let _isupper ch = 'A' <= ch && ch <= 'Z'
let _islower ch = 'a' <= ch && ch <= 'z'
let _isalpha ch = _isupper ch || _islower ch
let _isalnum ch = _isalpha ch || _isdigit ch

let _btoul b = if b then 1UL else 0UL

let isdigit (vm:VM) = vm.v0 <- _btoul << _isdigit << char <| vm.a0
let isupper (vm:VM) = vm.v0 <- _btoul << _isupper << char <| vm.a0
let islower (vm:VM) = vm.v0 <- _btoul << _islower << char <| vm.a0
let isalpha (vm:VM) = vm.v0 <- _btoul << _isalpha << char <| vm.a0
let isalnum (vm:VM) = vm.v0 <- _btoul << _isalnum << char <| vm.a0

let fgets (vm:VM) =
    vm.v0 <-
        let buf = vm.a0
        let len = vm.a1 |> int
        let f = vm.a2 |> int
        let fs = getSlot f
        if box fs = null then 0UL else
            let attr = attrs.[f]
            let mp = getPtr vm buf 1
            let rec read i len =
                if len = 1 then
                    mp.buf.[mp.ptr + i] <- 0uy
                    true
                else if len < 1 then
                    false
                else
                    let ch = _fgetc_stream fs attr
                    if ch < 0 && i = 0 then
                        false
                    else if ch < 1 then
                        mp.buf.[mp.ptr + i] <- 0uy
                        true
                    else
                        mp.buf.[mp.ptr + i] <- byte <| ch
                        if ch = int('\n') then
                            mp.buf.[mp.ptr + i + 1] <- 0uy
                            true
                        else
                            read (i + 1) (len - 1)
            if read 0 len then buf else 0UL

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
       strcmp
       strncpy
       strncat
       strlen
       memcpy
       memset
       lfind
       bsearch
       stricmp
       strtoul
       isdigit
       isupper
       islower
       isalpha
       isalnum
       fgets |]

let funcStart = 0x00ef0000UL
let funcEnd = funcStart + uint64(funcs.Length * 4)

let callFunc (vm:VM) =
    let f = int(vm.pc - funcStart) >>> 2
    vm.pc <- vm.ra
    if f < 0 || f >= funcs.Length then
        raise << vm.Abort <| sprintf "[Syscall] 未実装: %x" f
    else
        funcs.[f] vm
