using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Windows;
using System.Windows.Browser;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;

namespace Betelgeuse
{
    public partial class MainPage : UserControl
    {
        public MainPage()
        {
            InitializeComponent();

            Loaded += (sender, e) =>
            {
                HtmlPage.Plugin.Focus();
                textBox1.Focus();
            };

            new WheelObserver(textBox1);
            new WheelObserver(textBox2);
            new WheelObserver(textBox3);
            new WheelObserver(textBox4);

            Alpha.Syscall.openRead = OpenRead;
            Alpha.Syscall.openWrite = OpenWrite;
            Alpha.Syscall.closeFile = CloseFile;
        }

        Dictionary<string, byte[]> files = new Dictionary<string, byte[]>();

        public Stream OpenRead(string fn)
        {
            if (files.ContainsKey(fn))
                return new MemoryStream(files[fn]);
            var uri = new Uri("Test/" + fn, UriKind.Relative);
            return Application.GetResourceStream(uri).Stream;
        }

        public Stream OpenWrite(string fn)
        {
            return new MemoryStream();
        }

        public void CloseFile(string fn, Stream s, int attr)
        {
            if ((attr & Alpha.Syscall.aWrite) != 0)
            {
                var ms = s as MemoryStream;
                if (ms != null) files[fn] = ms.ToArray();
            }
            s.Dispose();
        }

        public void Write(string format, params object[] args)
        {
            textBox1.Text += string.Format(format, args);
        }

        private byte[] data;

        private void ReadElf(string[] args, Stream s, bool silent = false)
        {
            var sw1 = new StringWriter();
            var sw2 = new StringWriter();
            var sw3 = new StringWriter();
            var sw = sw1;
            var msg = "読み込みに失敗しました。";
            try
            {
                data = new byte[s.Length];
                s.Read(data, 0, data.Length);
                btnSave.IsEnabled = true;

                var ms = new MemoryStream(data);
                var br = new BinaryReader(ms);
                var elf = ELF.ELF64.Read(sw, br);

                sw = sw2;
                msg = "逆アセンブルに失敗しました。";
                if (elf.e_machine != ELF.EM_ALPHA_EXP)
                    throw new Exception("Alpha以外はサポートされていません。");
                var vm = Alpha.Memory.createVM(elf, data);
                if (!silent)
                {
                    if (comboBox1.SelectedIndex == 0)
                    {
                        using (var _7d = OpenRead("7d"))
                            ReadElf(new[] { "7d", args[0] }, _7d, true);
                        using (var asm = OpenRead(args[0] + ".asm"))
                        using (var sr = new StreamReader(asm))
                            textBox2.Text = sr.ReadToEnd();
                        sw2 = null;
                    }
                    else
                    {
                        var text = elf.Text;
                        var addr = text.sh_addr;
                        var end = addr + text.sh_size;
                        var off = text.sh_offset;
                        for (; addr < end; addr += 4, off += 4)
                        {
                            sw.Write("{0:x8}: ", off);
                            if (off != addr) sw.Write("[{0:x9}] ", addr);
                            var code = Alpha.Memory.read32(vm, addr);
                            Alpha.Disassemble.disassemble(sw, addr, code);
                            sw.WriteLine();
                        }
                    }
                }

                sw = sw3;
                msg = "実行に失敗しました。";
                Alpha.Exec.exec(vm, elf, sw, args);
            }
            catch (Exception ex)
            {
                sw.WriteLine(ex.Message);
                sw.WriteLine(msg);
            }
            if (!silent)
            {
                textBox1.Text = sw1.ToString();
                if (sw2 != null) textBox2.Text = sw2.ToString();
                textBox4.Text = sw3.ToString();
            }
        }

        private void btnOpen_Click(object sender, RoutedEventArgs e)
        {
            var ofd = new OpenFileDialog();
            if (ofd.ShowDialog() != true) return;

            textBox1.Text = textBox2.Text = textBox3.Text = textBox4.Text = "";
            try
            {
                var fi = ofd.File;
                if (fi.Length > 200 * 1024)
                    throw new Exception("ファイルが大き過ぎます。上限は200KBです。");
                using (var fs = ofd.File.OpenRead())
                    ReadElf(new[] { fi.Name }, fs);
            }
            catch (Exception ex)
            {
                textBox1.Text = ex.Message + Environment.NewLine +
                    "読み込みに失敗しました。" + Environment.NewLine;
            }
        }

        private void btnSave_Click(object sender, RoutedEventArgs e)
        {
            var sfd = new SaveFileDialog();
            if (sfd.ShowDialog() != true) return;

            using (var fs = sfd.OpenFile())
                fs.Write(data, 0, data.Length);
        }

        private void ReadTest(int t)
        {
            var tb = textBox1;
            textBox1.Text = textBox2.Text = textBox3.Text = textBox4.Text = "";
            try
            {
                var tt = t.ToString();
                using (var s = OpenRead(tt))
                    ReadElf(new[] { t.ToString() }, s);
                tb = textBox3;
                using (var s = OpenRead(tt + ".c"))
                using (var sr = new StreamReader(s))
                    textBox3.Text = sr.ReadToEnd();
            }
            catch (Exception ex)
            {
                tb.Text = ex.Message + Environment.NewLine +
                    "読み込みに失敗しました。" + Environment.NewLine;
            }
        }

        private void btnTest_Click(object sender, RoutedEventArgs e)
        {
            var button = sender as Button;
            if (button == null) return;

            var name = button.Name;
            if (!name.StartsWith("btnTest")) return;

            ReadTest(int.Parse(name.Substring(7)));
        }
    }
}
