module ELF

open System
open System.Collections.Generic
open System.IO

open Utils

let EI_MAG0    = 0
let EI_MAG1    = 1
let EI_MAG2    = 2
let EI_MAG3    = 3
let EI_CLASS   = 4
let EI_DATA    = 5
let EI_VERSION = 6
let EI_PAD     = 7
let EI_NIDENT  = 16

let ELFCLASSNONE = 0uy
let ELFCLASS32   = 1uy
let ELFCLASS64   = 2uy

let ELFDATANONE = 0uy
let ELFDATA2LSB = 1uy
let ELFDATA2MSB = 2uy

let EM_ALPHA_EXP = 0x9026

type Phdr64 =
    { p_type  : uint32
      p_flags : uint32
      p_offset: uint64
      p_vaddr : uint64
      p_paddr : uint64
      p_filesz: uint64
      p_memsz : uint64
      p_align : uint64 }
   
    static member Read tw br =
        { p_type   = readUInt32 tw br any "p_type"
          p_flags  = readUInt32 tw br any "p_flags"
          p_offset = readUInt64 tw br any "p_offset"
          p_vaddr  = readUInt64 tw br any "p_vaddr"
          p_paddr  = readUInt64 tw br any "p_paddr"
          p_filesz = readUInt64 tw br any "p_filesz"
          p_memsz  = readUInt64 tw br any "p_memsz"
          p_align  = readUInt64 tw br any "p_align" }

type Shdr64 =
    { sh_name     : uint32
      sh_type     : uint32
      sh_flags    : uint64
      sh_addr     : uint64
      sh_offset   : uint64
      sh_size     : uint64
      sh_link     : uint32
      sh_info     : uint32
      sh_addralign: uint64
      sh_entsize  : uint64
      Name        : string }
    
    static member Zero =
        { sh_name = 0u; sh_type = 0u; sh_flags = 0UL; sh_addr = 0UL
          sh_offset = 0UL; sh_size = 0UL; sh_link = 0u; sh_info = 0u
          sh_addralign = 0UL; sh_entsize = 0UL; Name = "" }
    
    member x.IsZero = x.sh_name = 0u
    
    static member Read tw br (stroff:uint64) =
        let n1, n2 = readUInt32WithString tw br "sh_name" stroff
        { sh_name      = n1
          sh_type      = readUInt32 tw br any "sh_type"
          sh_flags     = readUInt64 tw br any "sh_flags"
          sh_addr      = readUInt64 tw br any "sh_addr"
          sh_offset    = readUInt64 tw br any "sh_offset"
          sh_size      = readUInt64 tw br any "sh_size"
          sh_link      = readUInt32 tw br any "sh_link"
          sh_info      = readUInt32 tw br any "sh_info"
          sh_addralign = readUInt64 tw br any "sh_addralign"
          sh_entsize   = readUInt64 tw br any "sh_entsize"
          Name         = n2 }

type ELF64 =
    { e_ident    : byte[]
      e_type     : uint16
      e_machine  : uint16
      e_version  : uint32
      e_entry    : uint64
      e_phoff    : uint64
      e_shoff    : uint64
      e_flags    : uint32
      e_ehsize   : uint16
      e_phentsize: uint16
      e_phnum    : uint16
      e_shentsize: uint16
      e_shnum    : uint16
      e_shstrndx : uint16
      shdrs      : List<Shdr64>
      Text       : Shdr64
      Start      : uint64
      End        : uint64 }
    
    member x.Headers = x.shdrs.ToArray()
    member x.Size = x.End - x.Start
    
    static member Read tw br =
        let e_ident = Array.zeroCreate<byte> EI_NIDENT
        e_ident.[EI_MAG0   ] <- readByte  tw br (Some(0x7fuy), "0x7f") "e_ident.[EI_MAG0   ]"
        e_ident.[EI_MAG1   ] <- readBChar tw br (Some('E'), "'E'") "e_ident.[EI_MAG1   ]"
        e_ident.[EI_MAG2   ] <- readBChar tw br (Some('L'), "'L'") "e_ident.[EI_MAG2   ]"
        e_ident.[EI_MAG3   ] <- readBChar tw br (Some('F'), "'F'") "e_ident.[EI_MAG3   ]"
        e_ident.[EI_CLASS  ] <- readByte  tw br (Some(ELFCLASS64), "ELFCLASS64") "e_ident.[EI_CLASS  ]"
        e_ident.[EI_DATA   ] <- readByte  tw br (Some(ELFDATA2LSB), "ELFDATA2LSB") "e_ident.[EI_DATA   ]"
        e_ident.[EI_VERSION] <- readByte  tw br any "e_ident.[EI_VERSION]"
        tw.Write("{0:x8}: e_ident.[EI_PAD    ]:", br.BaseStream.Position)
        for i = EI_PAD to EI_NIDENT - 1 do
            tw.Write(" ")
            e_ident.[i] <- br.ReadByte()
            tw.Write("{0:x2}", e_ident.[i])
        tw.WriteLine()
        let e_type      = readUInt16 tw br any "e_type"
        let e_machine   = readUInt16 tw br any "e_machine"
        let e_version   = readUInt32 tw br any "e_version"
        let e_entry     = readUInt64 tw br any "e_entry"
        let e_phoff     = readUInt64 tw br any "e_phoff"
        let e_shoff     = readUInt64 tw br any "e_shoff"
        let e_flags     = readUInt32 tw br any "e_flags"
        let e_ehsize    = readUInt16 tw br any "e_ehsize"
        let e_phentsize = readUInt16 tw br any "e_phentsize"
        let e_phnum     = readUInt16 tw br any "e_phnum"
        let e_shentsize = readUInt16 tw br any "e_shentsize"
        let e_shnum     = readUInt16 tw br any "e_shnum"
        let e_shstrndx  = read tw br.BaseStream.Position "e_shstrndx" "{0:x4}" false br.ReadUInt16 any
        let stroff =
            if e_shstrndx = 0us then 0UL else
                br.BaseStream.Position <- e_shoff + uint64(e_shstrndx * e_shentsize) + 24UL |> int64
                let stroff = br.ReadUInt64()
                tw.Write(" => {0:x16}", stroff)
                stroff
        tw.WriteLine()
        if e_phoff <> 0UL then
            br.BaseStream.Position <- e_phoff |> int64
            for i = 1 to e_phnum |> int do
                tw.WriteLine()
                Phdr64.Read tw br |> ignore
        let mutable Start = 0UL
        let mutable End = 0UL
        let shdrs = new List<Shdr64>()
        let mutable Text = Shdr64.Zero
        if e_shoff <> 0UL then
            br.BaseStream.Position <- e_shoff |> int64
            for i = 1 to e_shnum |> int do
                tw.WriteLine()
                let sh = Shdr64.Read tw br stroff
                if sh.Name = ".text" then Text <- sh
                if not sh.IsZero then
                    shdrs.Add(sh)
                    let start = sh.sh_addr
                    let end' = start + sh.sh_size
                    if Start = 0UL || Start > start then Start <- start
                    if End = 0UL || End < end' then End <- end'
        { e_ident     = e_ident
          e_type      = e_type
          e_machine   = e_machine
          e_version   = e_version
          e_entry     = e_entry
          e_phoff     = e_phoff
          e_shoff     = e_shoff
          e_flags     = e_flags
          e_ehsize    = e_ehsize
          e_phentsize = e_phentsize
          e_phnum     = e_phnum
          e_shentsize = e_shentsize
          e_shnum     = e_shnum
          e_shstrndx  = e_shstrndx
          shdrs       = shdrs
          Text        = Text
          Start       = Start
          End         = End }
