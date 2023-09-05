<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class TokenizerCell
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(TokenizerCell))
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.SplitContainer3 = New System.Windows.Forms.SplitContainer()
        Me.RTB_DOC_OUT = New System.Windows.Forms.RichTextBox()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.RTB_Info = New System.Windows.Forms.RichTextBox()
        Me.SplitContainer2 = New System.Windows.Forms.SplitContainer()
        Me.ToolStripInputDocControl = New System.Windows.Forms.ToolStrip()
        Me.ToolStripDropDownButton4 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.OpenFileToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.OpenFolderToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.SaveOutputToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripDropDownButton3 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.ButtonTokenizeDocument = New System.Windows.Forms.ToolStripButton()
        Me.ButtonTrainCorpus = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.ToolStripButtonToChars = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButtonToWords = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButtonToSentences = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripDropDownButton1 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.ButtonNewModel = New System.Windows.Forms.ToolStripMenuItem()
        Me.BERTToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.UncasedToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CasedToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.BytePairEncodingToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TokenEncoderToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.HybridToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ButtonImportModel = New System.Windows.Forms.ToolStripButton()
        Me.ButtonExportModel = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripDropDownButton2 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.ButtonImportVocabulary = New System.Windows.Forms.ToolStripButton()
        Me.ButtonExportVocabulary = New System.Windows.Forms.ToolStripButton()
        Me.toolStripSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.HelpToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.SplitContainer4 = New System.Windows.Forms.SplitContainer()
        Me.RTB_TEXT_IN = New System.Windows.Forms.RichTextBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.CorpusFileListBox = New System.Windows.Forms.ListBox()
        Me.CorpusControlToolStrip = New System.Windows.Forms.ToolStrip()
        Me.MultiLingualToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer1.Panel1.SuspendLayout()
        Me.SplitContainer1.Panel2.SuspendLayout()
        Me.SplitContainer1.SuspendLayout()
        CType(Me.SplitContainer3, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer3.Panel1.SuspendLayout()
        Me.SplitContainer3.Panel2.SuspendLayout()
        Me.SplitContainer3.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.SplitContainer2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer2.Panel1.SuspendLayout()
        Me.SplitContainer2.Panel2.SuspendLayout()
        Me.SplitContainer2.SuspendLayout()
        Me.ToolStripInputDocControl.SuspendLayout()
        CType(Me.SplitContainer4, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer4.Panel1.SuspendLayout()
        Me.SplitContainer4.Panel2.SuspendLayout()
        Me.SplitContainer4.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'SplitContainer1
        '
        Me.SplitContainer1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer1.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer1.Margin = New System.Windows.Forms.Padding(4)
        Me.SplitContainer1.Name = "SplitContainer1"
        Me.SplitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal
        '
        'SplitContainer1.Panel1
        '
        Me.SplitContainer1.Panel1.Controls.Add(Me.SplitContainer3)
        '
        'SplitContainer1.Panel2
        '
        Me.SplitContainer1.Panel2.Controls.Add(Me.SplitContainer2)
        Me.SplitContainer1.Size = New System.Drawing.Size(878, 467)
        Me.SplitContainer1.SplitterDistance = 221
        Me.SplitContainer1.SplitterWidth = 5
        Me.SplitContainer1.TabIndex = 0
        '
        'SplitContainer3
        '
        Me.SplitContainer3.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer3.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer3.Margin = New System.Windows.Forms.Padding(4)
        Me.SplitContainer3.Name = "SplitContainer3"
        '
        'SplitContainer3.Panel1
        '
        Me.SplitContainer3.Panel1.Controls.Add(Me.RTB_DOC_OUT)
        '
        'SplitContainer3.Panel2
        '
        Me.SplitContainer3.Panel2.Controls.Add(Me.GroupBox2)
        Me.SplitContainer3.Size = New System.Drawing.Size(878, 221)
        Me.SplitContainer3.SplitterDistance = 587
        Me.SplitContainer3.TabIndex = 0
        '
        'RTB_DOC_OUT
        '
        Me.RTB_DOC_OUT.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RTB_DOC_OUT.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RTB_DOC_OUT.Font = New System.Drawing.Font("Arial Monospace", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.RTB_DOC_OUT.Location = New System.Drawing.Point(0, 0)
        Me.RTB_DOC_OUT.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.RTB_DOC_OUT.Name = "RTB_DOC_OUT"
        Me.RTB_DOC_OUT.Size = New System.Drawing.Size(587, 221)
        Me.RTB_DOC_OUT.TabIndex = 7
        Me.RTB_DOC_OUT.Text = ""
        '
        'GroupBox2
        '
        Me.GroupBox2.BackColor = System.Drawing.Color.Gray
        Me.GroupBox2.Controls.Add(Me.RTB_Info)
        Me.GroupBox2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GroupBox2.Font = New System.Drawing.Font("Arial Nova Cond", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.GroupBox2.ForeColor = System.Drawing.Color.White
        Me.GroupBox2.Location = New System.Drawing.Point(0, 0)
        Me.GroupBox2.Margin = New System.Windows.Forms.Padding(4)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Padding = New System.Windows.Forms.Padding(4)
        Me.GroupBox2.Size = New System.Drawing.Size(287, 221)
        Me.GroupBox2.TabIndex = 1
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Info"
        '
        'RTB_Info
        '
        Me.RTB_Info.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.RTB_Info.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RTB_Info.Font = New System.Drawing.Font("Arial Monospace", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.RTB_Info.Location = New System.Drawing.Point(4, 20)
        Me.RTB_Info.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.RTB_Info.Name = "RTB_Info"
        Me.RTB_Info.Size = New System.Drawing.Size(279, 197)
        Me.RTB_Info.TabIndex = 6
        Me.RTB_Info.Text = ""
        '
        'SplitContainer2
        '
        Me.SplitContainer2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer2.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer2.Margin = New System.Windows.Forms.Padding(4)
        Me.SplitContainer2.Name = "SplitContainer2"
        Me.SplitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal
        '
        'SplitContainer2.Panel1
        '
        Me.SplitContainer2.Panel1.BackColor = System.Drawing.Color.Gray
        Me.SplitContainer2.Panel1.Controls.Add(Me.ToolStripInputDocControl)
        '
        'SplitContainer2.Panel2
        '
        Me.SplitContainer2.Panel2.Controls.Add(Me.SplitContainer4)
        Me.SplitContainer2.Size = New System.Drawing.Size(878, 241)
        Me.SplitContainer2.SplitterDistance = 32
        Me.SplitContainer2.SplitterWidth = 5
        Me.SplitContainer2.TabIndex = 0
        '
        'ToolStripInputDocControl
        '
        Me.ToolStripInputDocControl.BackColor = System.Drawing.Color.Gray
        Me.ToolStripInputDocControl.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripDropDownButton4, Me.ToolStripDropDownButton3, Me.toolStripSeparator, Me.HelpToolStripButton})
        Me.ToolStripInputDocControl.Location = New System.Drawing.Point(0, 0)
        Me.ToolStripInputDocControl.Name = "ToolStripInputDocControl"
        Me.ToolStripInputDocControl.Padding = New System.Windows.Forms.Padding(0, 0, 3, 0)
        Me.ToolStripInputDocControl.Size = New System.Drawing.Size(878, 26)
        Me.ToolStripInputDocControl.TabIndex = 4
        Me.ToolStripInputDocControl.Text = "ToolStrip1"
        '
        'ToolStripDropDownButton4
        '
        Me.ToolStripDropDownButton4.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.OpenFileToolStripButton, Me.OpenFolderToolStripButton, Me.SaveOutputToolStripMenuItem})
        Me.ToolStripDropDownButton4.Image = CType(resources.GetObject("ToolStripDropDownButton4.Image"), System.Drawing.Image)
        Me.ToolStripDropDownButton4.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripDropDownButton4.Name = "ToolStripDropDownButton4"
        Me.ToolStripDropDownButton4.Size = New System.Drawing.Size(66, 23)
        Me.ToolStripDropDownButton4.Text = "File"
        '
        'OpenFileToolStripButton
        '
        Me.OpenFileToolStripButton.Image = CType(resources.GetObject("OpenFileToolStripButton.Image"), System.Drawing.Image)
        Me.OpenFileToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.OpenFileToolStripButton.Name = "OpenFileToolStripButton"
        Me.OpenFileToolStripButton.Size = New System.Drawing.Size(101, 23)
        Me.OpenFileToolStripButton.Text = "&Open File"
        '
        'OpenFolderToolStripButton
        '
        Me.OpenFolderToolStripButton.Image = CType(resources.GetObject("OpenFolderToolStripButton.Image"), System.Drawing.Image)
        Me.OpenFolderToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.OpenFolderToolStripButton.Name = "OpenFolderToolStripButton"
        Me.OpenFolderToolStripButton.Size = New System.Drawing.Size(120, 23)
        Me.OpenFolderToolStripButton.Text = "&Open Folder"
        '
        'SaveOutputToolStripMenuItem
        '
        Me.SaveOutputToolStripMenuItem.Name = "SaveOutputToolStripMenuItem"
        Me.SaveOutputToolStripMenuItem.Size = New System.Drawing.Size(180, 24)
        Me.SaveOutputToolStripMenuItem.Text = "Save Output"
        '
        'ToolStripDropDownButton3
        '
        Me.ToolStripDropDownButton3.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ButtonTokenizeDocument, Me.ButtonTrainCorpus, Me.ToolStripButton1, Me.ToolStripDropDownButton1, Me.ToolStripDropDownButton2})
        Me.ToolStripDropDownButton3.Image = CType(resources.GetObject("ToolStripDropDownButton3.Image"), System.Drawing.Image)
        Me.ToolStripDropDownButton3.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripDropDownButton3.Name = "ToolStripDropDownButton3"
        Me.ToolStripDropDownButton3.Size = New System.Drawing.Size(111, 23)
        Me.ToolStripDropDownButton3.Text = "Tokenizer"
        '
        'ButtonTokenizeDocument
        '
        Me.ButtonTokenizeDocument.Image = CType(resources.GetObject("ButtonTokenizeDocument.Image"), System.Drawing.Image)
        Me.ButtonTokenizeDocument.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonTokenizeDocument.Name = "ButtonTokenizeDocument"
        Me.ButtonTokenizeDocument.Size = New System.Drawing.Size(177, 23)
        Me.ButtonTokenizeDocument.Text = "Tokenize Document"
        '
        'ButtonTrainCorpus
        '
        Me.ButtonTrainCorpus.Image = CType(resources.GetObject("ButtonTrainCorpus.Image"), System.Drawing.Image)
        Me.ButtonTrainCorpus.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonTrainCorpus.Name = "ButtonTrainCorpus"
        Me.ButtonTrainCorpus.Size = New System.Drawing.Size(126, 23)
        Me.ButtonTrainCorpus.Text = "Train Corpus"
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButtonToChars, Me.ToolStripButtonToWords, Me.ToolStripButtonToSentences})
        Me.ToolStripButton1.Image = CType(resources.GetObject("ToolStripButton1.Image"), System.Drawing.Image)
        Me.ToolStripButton1.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripButton1.Name = "ToolStripButton1"
        Me.ToolStripButton1.Size = New System.Drawing.Size(105, 23)
        Me.ToolStripButton1.Text = "Tokenize"
        '
        'ToolStripButtonToChars
        '
        Me.ToolStripButtonToChars.Image = CType(resources.GetObject("ToolStripButtonToChars.Image"), System.Drawing.Image)
        Me.ToolStripButtonToChars.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripButtonToChars.Name = "ToolStripButtonToChars"
        Me.ToolStripButtonToChars.Size = New System.Drawing.Size(96, 23)
        Me.ToolStripButtonToChars.Text = "To Chars"
        '
        'ToolStripButtonToWords
        '
        Me.ToolStripButtonToWords.Image = CType(resources.GetObject("ToolStripButtonToWords.Image"), System.Drawing.Image)
        Me.ToolStripButtonToWords.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripButtonToWords.Name = "ToolStripButtonToWords"
        Me.ToolStripButtonToWords.Size = New System.Drawing.Size(99, 23)
        Me.ToolStripButtonToWords.Text = "To Words"
        '
        'ToolStripButtonToSentences
        '
        Me.ToolStripButtonToSentences.Image = CType(resources.GetObject("ToolStripButtonToSentences.Image"), System.Drawing.Image)
        Me.ToolStripButtonToSentences.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripButtonToSentences.Name = "ToolStripButtonToSentences"
        Me.ToolStripButtonToSentences.Size = New System.Drawing.Size(131, 23)
        Me.ToolStripButtonToSentences.Text = "To Sentences"
        '
        'ToolStripDropDownButton1
        '
        Me.ToolStripDropDownButton1.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ButtonNewModel, Me.ButtonImportModel, Me.ButtonExportModel})
        Me.ToolStripDropDownButton1.Image = CType(resources.GetObject("ToolStripDropDownButton1.Image"), System.Drawing.Image)
        Me.ToolStripDropDownButton1.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripDropDownButton1.Name = "ToolStripDropDownButton1"
        Me.ToolStripDropDownButton1.Size = New System.Drawing.Size(83, 23)
        Me.ToolStripDropDownButton1.Text = "Model"
        '
        'ButtonNewModel
        '
        Me.ButtonNewModel.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BERTToolStripMenuItem, Me.BytePairEncodingToolStripMenuItem, Me.TokenEncoderToolStripMenuItem, Me.HybridToolStripMenuItem})
        Me.ButtonNewModel.Image = CType(resources.GetObject("ButtonNewModel.Image"), System.Drawing.Image)
        Me.ButtonNewModel.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonNewModel.Name = "ButtonNewModel"
        Me.ButtonNewModel.Size = New System.Drawing.Size(189, 24)
        Me.ButtonNewModel.Text = "&New Model"
        '
        'BERTToolStripMenuItem
        '
        Me.BERTToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.UncasedToolStripMenuItem, Me.CasedToolStripMenuItem, Me.MultiLingualToolStripMenuItem})
        Me.BERTToolStripMenuItem.Name = "BERTToolStripMenuItem"
        Me.BERTToolStripMenuItem.Size = New System.Drawing.Size(219, 24)
        Me.BERTToolStripMenuItem.Text = "BERT"
        '
        'UncasedToolStripMenuItem
        '
        Me.UncasedToolStripMenuItem.Name = "UncasedToolStripMenuItem"
        Me.UncasedToolStripMenuItem.Size = New System.Drawing.Size(180, 24)
        Me.UncasedToolStripMenuItem.Text = "Uncased"
        '
        'CasedToolStripMenuItem
        '
        Me.CasedToolStripMenuItem.Name = "CasedToolStripMenuItem"
        Me.CasedToolStripMenuItem.Size = New System.Drawing.Size(180, 24)
        Me.CasedToolStripMenuItem.Text = "Cased"
        '
        'BytePairEncodingToolStripMenuItem
        '
        Me.BytePairEncodingToolStripMenuItem.Name = "BytePairEncodingToolStripMenuItem"
        Me.BytePairEncodingToolStripMenuItem.Size = New System.Drawing.Size(219, 24)
        Me.BytePairEncodingToolStripMenuItem.Text = "_BytePairEncoding"
        '
        'TokenEncoderToolStripMenuItem
        '
        Me.TokenEncoderToolStripMenuItem.Name = "TokenEncoderToolStripMenuItem"
        Me.TokenEncoderToolStripMenuItem.Size = New System.Drawing.Size(219, 24)
        Me.TokenEncoderToolStripMenuItem.Text = "_TokenEncoder"
        '
        'HybridToolStripMenuItem
        '
        Me.HybridToolStripMenuItem.Name = "HybridToolStripMenuItem"
        Me.HybridToolStripMenuItem.Size = New System.Drawing.Size(219, 24)
        Me.HybridToolStripMenuItem.Text = "_Hybrid"
        '
        'ButtonImportModel
        '
        Me.ButtonImportModel.Image = CType(resources.GetObject("ButtonImportModel.Image"), System.Drawing.Image)
        Me.ButtonImportModel.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonImportModel.Name = "ButtonImportModel"
        Me.ButtonImportModel.Size = New System.Drawing.Size(129, 23)
        Me.ButtonImportModel.Text = "&Import Model"
        '
        'ButtonExportModel
        '
        Me.ButtonExportModel.Image = CType(resources.GetObject("ButtonExportModel.Image"), System.Drawing.Image)
        Me.ButtonExportModel.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonExportModel.Name = "ButtonExportModel"
        Me.ButtonExportModel.Size = New System.Drawing.Size(129, 23)
        Me.ButtonExportModel.Text = "Export Model"
        '
        'ToolStripDropDownButton2
        '
        Me.ToolStripDropDownButton2.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ButtonImportVocabulary, Me.ButtonExportVocabulary})
        Me.ToolStripDropDownButton2.Image = CType(resources.GetObject("ToolStripDropDownButton2.Image"), System.Drawing.Image)
        Me.ToolStripDropDownButton2.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripDropDownButton2.Name = "ToolStripDropDownButton2"
        Me.ToolStripDropDownButton2.Size = New System.Drawing.Size(123, 23)
        Me.ToolStripDropDownButton2.Text = "Vocabulary"
        '
        'ButtonImportVocabulary
        '
        Me.ButtonImportVocabulary.Image = CType(resources.GetObject("ButtonImportVocabulary.Image"), System.Drawing.Image)
        Me.ButtonImportVocabulary.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonImportVocabulary.Name = "ButtonImportVocabulary"
        Me.ButtonImportVocabulary.Size = New System.Drawing.Size(169, 23)
        Me.ButtonImportVocabulary.Text = "Import Vocabulary"
        '
        'ButtonExportVocabulary
        '
        Me.ButtonExportVocabulary.Image = CType(resources.GetObject("ButtonExportVocabulary.Image"), System.Drawing.Image)
        Me.ButtonExportVocabulary.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonExportVocabulary.Name = "ButtonExportVocabulary"
        Me.ButtonExportVocabulary.Size = New System.Drawing.Size(169, 23)
        Me.ButtonExportVocabulary.Text = "Export Vocabulary"
        '
        'toolStripSeparator
        '
        Me.toolStripSeparator.Name = "toolStripSeparator"
        Me.toolStripSeparator.Size = New System.Drawing.Size(6, 26)
        '
        'HelpToolStripButton
        '
        Me.HelpToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.HelpToolStripButton.Image = CType(resources.GetObject("HelpToolStripButton.Image"), System.Drawing.Image)
        Me.HelpToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.HelpToolStripButton.Name = "HelpToolStripButton"
        Me.HelpToolStripButton.Size = New System.Drawing.Size(23, 23)
        Me.HelpToolStripButton.Text = "He&lp"
        '
        'SplitContainer4
        '
        Me.SplitContainer4.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer4.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer4.Margin = New System.Windows.Forms.Padding(4)
        Me.SplitContainer4.Name = "SplitContainer4"
        '
        'SplitContainer4.Panel1
        '
        Me.SplitContainer4.Panel1.Controls.Add(Me.RTB_TEXT_IN)
        '
        'SplitContainer4.Panel2
        '
        Me.SplitContainer4.Panel2.Controls.Add(Me.GroupBox1)
        Me.SplitContainer4.Size = New System.Drawing.Size(878, 204)
        Me.SplitContainer4.SplitterDistance = 587
        Me.SplitContainer4.TabIndex = 0
        '
        'RTB_TEXT_IN
        '
        Me.RTB_TEXT_IN.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RTB_TEXT_IN.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RTB_TEXT_IN.EnableAutoDragDrop = True
        Me.RTB_TEXT_IN.Font = New System.Drawing.Font("Arial Monospace", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.RTB_TEXT_IN.Location = New System.Drawing.Point(0, 0)
        Me.RTB_TEXT_IN.Margin = New System.Windows.Forms.Padding(6, 4, 6, 4)
        Me.RTB_TEXT_IN.Name = "RTB_TEXT_IN"
        Me.RTB_TEXT_IN.Size = New System.Drawing.Size(587, 204)
        Me.RTB_TEXT_IN.TabIndex = 4
        Me.RTB_TEXT_IN.Text = ""
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.Color.Gray
        Me.GroupBox1.Controls.Add(Me.CorpusFileListBox)
        Me.GroupBox1.Controls.Add(Me.CorpusControlToolStrip)
        Me.GroupBox1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GroupBox1.Font = New System.Drawing.Font("Arial Nova Cond", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.GroupBox1.ForeColor = System.Drawing.SystemColors.AppWorkspace
        Me.GroupBox1.Location = New System.Drawing.Point(0, 0)
        Me.GroupBox1.Margin = New System.Windows.Forms.Padding(4)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Padding = New System.Windows.Forms.Padding(4)
        Me.GroupBox1.Size = New System.Drawing.Size(287, 204)
        Me.GroupBox1.TabIndex = 0
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Loaded Docs"
        '
        'CorpusFileListBox
        '
        Me.CorpusFileListBox.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.CorpusFileListBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.CorpusFileListBox.Font = New System.Drawing.Font("Arial Nova Cond", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.CorpusFileListBox.FormattingEnabled = True
        Me.CorpusFileListBox.ItemHeight = 14
        Me.CorpusFileListBox.Location = New System.Drawing.Point(4, 45)
        Me.CorpusFileListBox.Margin = New System.Windows.Forms.Padding(4)
        Me.CorpusFileListBox.Name = "CorpusFileListBox"
        Me.CorpusFileListBox.Size = New System.Drawing.Size(279, 155)
        Me.CorpusFileListBox.TabIndex = 2
        '
        'CorpusControlToolStrip
        '
        Me.CorpusControlToolStrip.BackColor = System.Drawing.Color.DimGray
        Me.CorpusControlToolStrip.Location = New System.Drawing.Point(4, 20)
        Me.CorpusControlToolStrip.Name = "CorpusControlToolStrip"
        Me.CorpusControlToolStrip.Size = New System.Drawing.Size(279, 25)
        Me.CorpusControlToolStrip.TabIndex = 1
        Me.CorpusControlToolStrip.Text = "Document Control"
        '
        'MultiLingualToolStripMenuItem
        '
        Me.MultiLingualToolStripMenuItem.Name = "MultiLingualToolStripMenuItem"
        Me.MultiLingualToolStripMenuItem.Size = New System.Drawing.Size(180, 24)
        Me.MultiLingualToolStripMenuItem.Text = "Multi Lingual"
        '
        'TokenizerModule
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Transparent
        Me.Controls.Add(Me.SplitContainer1)
        Me.Font = New System.Drawing.Font("Arial Nova Cond", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Margin = New System.Windows.Forms.Padding(4)
        Me.Name = "TokenizerModule"
        Me.Size = New System.Drawing.Size(878, 467)
        Me.SplitContainer1.Panel1.ResumeLayout(False)
        Me.SplitContainer1.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer1.ResumeLayout(False)
        Me.SplitContainer3.Panel1.ResumeLayout(False)
        Me.SplitContainer3.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer3, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer3.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.SplitContainer2.Panel1.ResumeLayout(False)
        Me.SplitContainer2.Panel1.PerformLayout()
        Me.SplitContainer2.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer2.ResumeLayout(False)
        Me.ToolStripInputDocControl.ResumeLayout(False)
        Me.ToolStripInputDocControl.PerformLayout()
        Me.SplitContainer4.Panel1.ResumeLayout(False)
        Me.SplitContainer4.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer4, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer4.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents SplitContainer1 As SplitContainer
    Friend WithEvents SplitContainer2 As SplitContainer
    Friend WithEvents ToolStripInputDocControl As ToolStrip
    Friend WithEvents toolStripSeparator As ToolStripSeparator
    Friend WithEvents HelpToolStripButton As ToolStripButton
    Friend WithEvents ToolStripDropDownButton3 As ToolStripDropDownButton
    Friend WithEvents ButtonTokenizeDocument As ToolStripButton
    Friend WithEvents ButtonTrainCorpus As ToolStripButton
    Friend WithEvents ToolStripDropDownButton1 As ToolStripDropDownButton
    Friend WithEvents ButtonImportModel As ToolStripButton
    Friend WithEvents ButtonExportModel As ToolStripButton
    Friend WithEvents ToolStripDropDownButton2 As ToolStripDropDownButton
    Friend WithEvents ButtonImportVocabulary As ToolStripButton
    Friend WithEvents ButtonExportVocabulary As ToolStripButton
    Friend WithEvents SplitContainer3 As SplitContainer
    Friend WithEvents RTB_DOC_OUT As RichTextBox
    Friend WithEvents SplitContainer4 As SplitContainer
    Friend WithEvents RTB_TEXT_IN As RichTextBox
    Friend WithEvents GroupBox1 As GroupBox
    Friend WithEvents CorpusFileListBox As ListBox
    Friend WithEvents CorpusControlToolStrip As ToolStrip
    Friend WithEvents GroupBox2 As GroupBox
    Friend WithEvents RTB_Info As RichTextBox
    Friend WithEvents ToolStripDropDownButton4 As ToolStripDropDownButton
    Friend WithEvents OpenFileToolStripButton As ToolStripButton
    Friend WithEvents OpenFolderToolStripButton As ToolStripButton
    Friend WithEvents SaveOutputToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripButton1 As ToolStripDropDownButton
    Friend WithEvents ToolStripButtonToChars As ToolStripButton
    Friend WithEvents ToolStripButtonToWords As ToolStripButton
    Friend WithEvents ToolStripButtonToSentences As ToolStripButton
    Friend WithEvents ButtonNewModel As ToolStripMenuItem
    Friend WithEvents BytePairEncodingToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents TokenEncoderToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents HybridToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents BERTToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents UncasedToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents CasedToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents MultiLingualToolStripMenuItem As ToolStripMenuItem
End Class
