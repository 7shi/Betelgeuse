namespace Alpha

open System
open System.IO

type VMException(s) = inherit Exception(s)

type VM =
    { mutable pc : uint64
      reg        : uint64[]
      frg        : float[]
      stack      : byte[]
      memory     : byte[]
      memoryStart: uint64
      memoryEnd  : uint64
      mutable out: TextWriter }

    member x.Abort(msg:string) = new VMException(sprintf "pc=%016x: %s" (x.pc - 4UL) msg)
    
    member x.v0   with get() = x.reg.[ 0] and set(v) = x.reg.[ 0] <- v
    member x.t0   with get() = x.reg.[ 1] and set(v) = x.reg.[ 1] <- v
    member x.t1   with get() = x.reg.[ 2] and set(v) = x.reg.[ 2] <- v
    member x.t2   with get() = x.reg.[ 3] and set(v) = x.reg.[ 3] <- v
    member x.t3   with get() = x.reg.[ 4] and set(v) = x.reg.[ 4] <- v
    member x.t4   with get() = x.reg.[ 5] and set(v) = x.reg.[ 5] <- v
    member x.t5   with get() = x.reg.[ 6] and set(v) = x.reg.[ 6] <- v
    member x.t6   with get() = x.reg.[ 7] and set(v) = x.reg.[ 7] <- v
    member x.t7   with get() = x.reg.[ 8] and set(v) = x.reg.[ 8] <- v
    member x.s0   with get() = x.reg.[ 9] and set(v) = x.reg.[ 9] <- v
    member x.s1   with get() = x.reg.[10] and set(v) = x.reg.[10] <- v
    member x.s2   with get() = x.reg.[11] and set(v) = x.reg.[11] <- v
    member x.s3   with get() = x.reg.[12] and set(v) = x.reg.[12] <- v
    member x.s4   with get() = x.reg.[13] and set(v) = x.reg.[13] <- v
    member x.s5   with get() = x.reg.[14] and set(v) = x.reg.[14] <- v
    member x.fp   with get() = x.reg.[15] and set(v) = x.reg.[15] <- v
    member x.a0   with get() = x.reg.[16] and set(v) = x.reg.[16] <- v
    member x.a1   with get() = x.reg.[17] and set(v) = x.reg.[17] <- v
    member x.a2   with get() = x.reg.[18] and set(v) = x.reg.[18] <- v
    member x.a3   with get() = x.reg.[19] and set(v) = x.reg.[19] <- v
    member x.a4   with get() = x.reg.[20] and set(v) = x.reg.[20] <- v
    member x.a5   with get() = x.reg.[21] and set(v) = x.reg.[21] <- v
    member x.t8   with get() = x.reg.[22] and set(v) = x.reg.[22] <- v
    member x.t9   with get() = x.reg.[23] and set(v) = x.reg.[23] <- v
    member x.t10  with get() = x.reg.[24] and set(v) = x.reg.[24] <- v
    member x.t11  with get() = x.reg.[25] and set(v) = x.reg.[25] <- v
    member x.ra   with get() = x.reg.[26] and set(v) = x.reg.[26] <- v
    member x.t12  with get() = x.reg.[27] and set(v) = x.reg.[27] <- v
    member x.at   with get() = x.reg.[28] and set(v) = x.reg.[28] <- v
    member x.gp   with get() = x.reg.[29] and set(v) = x.reg.[29] <- v
    member x.sp   with get() = x.reg.[30] and set(v) = x.reg.[30] <- v
    member x.zero with get() = x.reg.[31] and set(v) = x.reg.[31] <- v
