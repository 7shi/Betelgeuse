open System
open System.IO
open System.Text
open Microsoft.Win32

open ELF

Console.WriteLine("*** Betelgeuse - Alpha Orionis ***")
Console.WriteLine()

let ofd = new OpenFileDialog()
let result = ofd.ShowDialog()
if result.HasValue && result.Value then
    use fs = ofd.OpenFile()
    use br = new BinaryReader(fs)
    let sb = new StringBuilder()
    try
        ignore <| ELF64.Read sb br
    with ex ->
        ignore <| sb.Append(ex.Message)
    Console.WriteLine(sb.ToString())
    Console.ReadLine() |> ignore
