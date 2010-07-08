﻿open System
open System.Drawing
open System.IO
open System.Text
open System.Windows.Forms

open ELF

let f = new Form()
let mono = new Font(FontFamily.GenericMonospace, f.Font.Size)
let t = new TextBox(Multiline = true, ScrollBars = ScrollBars.Both,
                    Dock = DockStyle.Fill, WordWrap = false, Font = mono)
f.Controls.Add t

let open'(fn:string) =
    use fs = new FileStream(fn, FileMode.Open)
    use br = new BinaryReader(fs)
    let sb = new StringBuilder()
    ignore <| sb.AppendLine("*** Betelgeuse - Alpha Orionis ***")
    ignore <| sb.AppendLine()
    try
        ignore <| ELF64.Read sb br
    with ex ->
        ignore <| sb.AppendLine(ex.Message)
    t.Text <- sb.ToString()

let m = new MainMenu()
let mi = new MenuItem("開く")
mi.Click.Add <| fun _ ->
    use ofd = new OpenFileDialog()
    if ofd.ShowDialog() = DialogResult.OK then open' ofd.FileName
ignore <| m.MenuItems.Add mi
f.Menu <- m

[<STAThread>]
do Application.Run f
