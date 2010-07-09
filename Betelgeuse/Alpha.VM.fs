namespace Alpha

open System
open System.IO

type VM =
    { mutable pc : uint64
      reg        : uint64[]
      frg        : float[]
      stack      : byte[]
      memory     : byte[]
      memoryStart: uint64
      memoryEnd  : uint64
      mutable out: TextWriter }

    member x.Abort(msg:string) = new Exception(sprintf "pc=%016x: %s" (x.pc - 4UL) msg)
