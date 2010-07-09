open System
open System.Drawing
open System.IO
open System.Text
open System.Windows.Forms

open ELF
open Alpha.Disassemble

let f = new Form(Text = "Betelgeuse")
let mono = new Font(FontFamily.GenericMonospace, f.Font.Size)
let t = new TextBox(Multiline = true, ScrollBars = ScrollBars.Both,
                    Dock = DockStyle.Fill, WordWrap = false, Font = mono,
                    Text = "*** Betelgeuse - Alpha Orionis ***\r\n")
f.Controls.Add t

let open'(fn:string) =
    use fs = new FileStream(fn, FileMode.Open)
    use br = new BinaryReader(fs)
    let sw = new StringWriter()
    try
        let elf = ELF64.Read sw br
        
        sw.WriteLine()
        let text = elf.Text
        let mutable addr = text.sh_addr
        let end' = addr + text.sh_size
        let mutable off = text.sh_offset;
        fs.Position <- off |> int64
        while addr < end' do
            sw.Write("{0:x8}: ", off)
            if off <> addr then sw.Write("[{0:x8}] ", addr)
            let code = br.ReadUInt32()
            ignore <| disassemble sw addr code
            sw.WriteLine()
            off  <- off  + 4UL
            addr <- addr + 4UL
    with ex ->
        sw.WriteLine(ex.Message)
    t.Text <- sw.ToString()

let m = new MainMenu()
let mi = new MenuItem("開く")
mi.Click.Add <| fun _ ->
    use ofd = new OpenFileDialog()
    if ofd.ShowDialog() = DialogResult.OK then open' ofd.FileName
ignore <| m.MenuItems.Add mi
f.Menu <- m

[<STAThread>] Application.Run f
