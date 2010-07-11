open System
open System.Drawing
open System.IO
open System.Text
open System.Windows.Forms

open ELF
open Alpha
open Alpha.Disassemble
open Alpha.Exec
open Alpha.Memory

let f = new Form(Text = "Betelgeuse - Alpha Orionis",
                 StartPosition = FormStartPosition.WindowsDefaultBounds)

let mono = new Font(FontFamily.GenericMonospace, f.Font.Size)
let createTextBox() =
    new TextBox(Multiline = true, ScrollBars = ScrollBars.Both,
                Dock = DockStyle.Fill, WordWrap = false, Font = mono)

let sc1 = new SplitContainer(Dock = DockStyle.Fill)
let sc2 = new SplitContainer(Dock = DockStyle.Fill, Orientation = Orientation.Horizontal)
sc1.Panel2.Controls.Add sc2
f.Controls.Add sc1

let t1 = createTextBox()
let t2 = createTextBox()
let t3 = createTextBox()
sc1.Panel1.Controls.Add t1
sc2.Panel1.Controls.Add t2
sc2.Panel2.Controls.Add t3

let open'(fn:string) =
    use fs = new FileStream(fn, FileMode.Open)
    let sw1 = new StringWriter()
    let sw2 = new StringWriter()
    let sw3 = new StringWriter()
    let mutable sw = sw1
    try
        let data = Array.zeroCreate<byte> (fs.Length |> int)
        fs.Read(data, 0, data.Length) |> ignore
        
        let br = new BinaryReader(new MemoryStream(data))
        let elf = ELF64.Read sw br
        
        sw <- sw2
        let vm = createVM elf data
        let text = elf.Text
        let mutable addr = text.sh_addr
        let end' = addr + text.sh_size
        let mutable off = text.sh_offset;
        fs.Position <- off |> int64
        while addr < end' do
            sw.Write("{0:x8}: ", off)
            if off <> addr then sw.Write("[{0:x8}] ", addr)
            let code = read32 vm addr
            disassemble sw addr code |> ignore
            sw.WriteLine()
            off  <- off  + 4UL
            addr <- addr + 4UL
        
        sw <- sw3
        exec vm elf sw
    with ex ->
        sw.WriteLine(ex.Message)
    t1.Text <- sw1.ToString()
    t2.Text <- sw2.ToString()
    t3.Text <- sw3.ToString()

let m = new MainMenu()
let mi = new MenuItem("開く")
mi.Click.Add <| fun _ ->
    use ofd = new OpenFileDialog()
    if ofd.ShowDialog() = DialogResult.OK then open' ofd.FileName
m.MenuItems.Add mi |> ignore
f.Menu <- m

[<STAThread>] Application.Run f
