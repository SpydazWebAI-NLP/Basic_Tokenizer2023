<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class ModelControlTokenizer
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ModelControlTokenizer))
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.SplitContainer2 = New System.Windows.Forms.SplitContainer()
        Me.SplitContainer4 = New System.Windows.Forms.SplitContainer()
        Me.RTB_DOC_OUT = New System.Windows.Forms.RichTextBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.RTB_Info = New System.Windows.Forms.RichTextBox()
        Me.ComboBoxModelList = New System.Windows.Forms.ComboBox()
        Me.ToolStripModelControl = New System.Windows.Forms.ToolStrip()
        Me.ButtonNewModel = New System.Windows.Forms.ToolStripButton()
        Me.ButtonImportModel = New System.Windows.Forms.ToolStripButton()
        Me.ButtonExportModel = New System.Windows.Forms.ToolStripButton()
        Me.ButtonImportVocabulary = New System.Windows.Forms.ToolStripButton()
        Me.ButtonExportVocabulary = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
        Me.NgramSizeText = New System.Windows.Forms.ToolStripTextBox()
        Me.SplitContainer3 = New System.Windows.Forms.SplitContainer()
        Me.ComboBoxSelectFile = New System.Windows.Forms.ComboBox()
        Me.ToolStripInputDocControl = New System.Windows.Forms.ToolStrip()
        Me.OpenFileToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.ButtonTokenizeDocument = New System.Windows.Forms.ToolStripButton()
        Me.OpenFolderToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.ButtonTrainCorpus = New System.Windows.Forms.ToolStripButton()
        Me.ButtonAddDoc = New System.Windows.Forms.ToolStripButton()
        Me.ButtonRemoveDOc = New System.Windows.Forms.ToolStripButton()
        Me.toolStripSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.HelpToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.RTB_TEXT_IN = New System.Windows.Forms.RichTextBox()
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer1.Panel1.SuspendLayout()
        Me.SplitContainer1.Panel2.SuspendLayout()
        Me.SplitContainer1.SuspendLayout()
        CType(Me.SplitContainer2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer2.Panel1.SuspendLayout()
        Me.SplitContainer2.Panel2.SuspendLayout()
        Me.SplitContainer2.SuspendLayout()
        CType(Me.SplitContainer4, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer4.Panel1.SuspendLayout()
        Me.SplitContainer4.Panel2.SuspendLayout()
        Me.SplitContainer4.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.ToolStripModelControl.SuspendLayout()
        CType(Me.SplitContainer3, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer3.Panel1.SuspendLayout()
        Me.SplitContainer3.Panel2.SuspendLayout()
        Me.SplitContainer3.SuspendLayout()
        Me.ToolStripInputDocControl.SuspendLayout()
        Me.SuspendLayout()
        '
        'SplitContainer1
        '
        Me.SplitContainer1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer1.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer1.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.SplitContainer1.Name = "SplitContainer1"
        Me.SplitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal
        '
        'SplitContainer1.Panel1
        '
        Me.SplitContainer1.Panel1.Controls.Add(Me.SplitContainer2)
        '
        'SplitContainer1.Panel2
        '
        Me.SplitContainer1.Panel2.Controls.Add(Me.SplitContainer3)
        Me.SplitContainer1.Size = New System.Drawing.Size(790, 823)
        Me.SplitContainer1.SplitterDistance = 395
        Me.SplitContainer1.SplitterWidth = 5
        Me.SplitContainer1.TabIndex = 0
        '
        'SplitContainer2
        '
        Me.SplitContainer2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer2.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer2.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.SplitContainer2.Name = "SplitContainer2"
        Me.SplitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal
        '
        'SplitContainer2.Panel1
        '
        Me.SplitContainer2.Panel1.Controls.Add(Me.SplitContainer4)
        '
        'SplitContainer2.Panel2
        '
        Me.SplitContainer2.Panel2.Controls.Add(Me.ComboBoxModelList)
        Me.SplitContainer2.Panel2.Controls.Add(Me.ToolStripModelControl)
        Me.SplitContainer2.Size = New System.Drawing.Size(790, 395)
        Me.SplitContainer2.SplitterDistance = 329
        Me.SplitContainer2.SplitterWidth = 5
        Me.SplitContainer2.TabIndex = 0
        '
        'SplitContainer4
        '
        Me.SplitContainer4.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer4.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer4.Name = "SplitContainer4"
        '
        'SplitContainer4.Panel1
        '
        Me.SplitContainer4.Panel1.Controls.Add(Me.RTB_DOC_OUT)
        '
        'SplitContainer4.Panel2
        '
        Me.SplitContainer4.Panel2.Controls.Add(Me.GroupBox1)
        Me.SplitContainer4.Size = New System.Drawing.Size(790, 329)
        Me.SplitContainer4.SplitterDistance = 546
        Me.SplitContainer4.TabIndex = 0
        '
        'RTB_DOC_OUT
        '
        Me.RTB_DOC_OUT.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RTB_DOC_OUT.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RTB_DOC_OUT.Font = New System.Drawing.Font("Arial Monospace", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.RTB_DOC_OUT.Location = New System.Drawing.Point(0, 0)
        Me.RTB_DOC_OUT.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.RTB_DOC_OUT.Name = "RTB_DOC_OUT"
        Me.RTB_DOC_OUT.Size = New System.Drawing.Size(546, 329)
        Me.RTB_DOC_OUT.TabIndex = 5
        Me.RTB_DOC_OUT.Text = ""
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.Color.Gray
        Me.GroupBox1.Controls.Add(Me.RTB_Info)
        Me.GroupBox1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GroupBox1.Font = New System.Drawing.Font("Arial Nova Cond", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.GroupBox1.Location = New System.Drawing.Point(0, 0)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(240, 329)
        Me.GroupBox1.TabIndex = 0
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Info"
        '
        'RTB_Info
        '
        Me.RTB_Info.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.RTB_Info.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RTB_Info.Font = New System.Drawing.Font("Arial Monospace", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.RTB_Info.Location = New System.Drawing.Point(3, 23)
        Me.RTB_Info.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.RTB_Info.Name = "RTB_Info"
        Me.RTB_Info.Size = New System.Drawing.Size(234, 303)
        Me.RTB_Info.TabIndex = 6
        Me.RTB_Info.Text = ""
        '
        'ComboBoxModelList
        '
        Me.ComboBoxModelList.BackColor = System.Drawing.Color.LemonChiffon
        Me.ComboBoxModelList.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ComboBoxModelList.Font = New System.Drawing.Font("Arial Monospace", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.ComboBoxModelList.FormattingEnabled = True
        Me.ComboBoxModelList.Location = New System.Drawing.Point(0, 34)
        Me.ComboBoxModelList.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.ComboBoxModelList.Name = "ComboBoxModelList"
        Me.ComboBoxModelList.Size = New System.Drawing.Size(790, 27)
        Me.ComboBoxModelList.TabIndex = 6
        '
        'ToolStripModelControl
        '
        Me.ToolStripModelControl.BackColor = System.Drawing.Color.Gray
        Me.ToolStripModelControl.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ButtonNewModel, Me.ButtonImportModel, Me.ButtonExportModel, Me.ButtonImportVocabulary, Me.ButtonExportVocabulary, Me.ToolStripSeparator1, Me.ToolStripButton5, Me.NgramSizeText})
        Me.ToolStripModelControl.Location = New System.Drawing.Point(0, 0)
        Me.ToolStripModelControl.Name = "ToolStripModelControl"
        Me.ToolStripModelControl.Padding = New System.Windows.Forms.Padding(0, 0, 2, 0)
        Me.ToolStripModelControl.Size = New System.Drawing.Size(790, 25)
        Me.ToolStripModelControl.TabIndex = 4
        Me.ToolStripModelControl.Text = "ToolStrip1"
        '
        'ButtonNewModel
        '
        Me.ButtonNewModel.Image = CType(resources.GetObject("ButtonNewModel.Image"), System.Drawing.Image)
        Me.ButtonNewModel.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonNewModel.Name = "ButtonNewModel"
        Me.ButtonNewModel.Size = New System.Drawing.Size(100, 22)
        Me.ButtonNewModel.Text = "&New Model"
        '
        'ButtonImportModel
        '
        Me.ButtonImportModel.Image = CType(resources.GetObject("ButtonImportModel.Image"), System.Drawing.Image)
        Me.ButtonImportModel.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonImportModel.Name = "ButtonImportModel"
        Me.ButtonImportModel.Size = New System.Drawing.Size(115, 22)
        Me.ButtonImportModel.Text = "&Import Model"
        '
        'ButtonExportModel
        '
        Me.ButtonExportModel.Image = CType(resources.GetObject("ButtonExportModel.Image"), System.Drawing.Image)
        Me.ButtonExportModel.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonExportModel.Name = "ButtonExportModel"
        Me.ButtonExportModel.Size = New System.Drawing.Size(115, 22)
        Me.ButtonExportModel.Text = "Export Model"
        '
        'ButtonImportVocabulary
        '
        Me.ButtonImportVocabulary.Image = CType(resources.GetObject("ButtonImportVocabulary.Image"), System.Drawing.Image)
        Me.ButtonImportVocabulary.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonImportVocabulary.Name = "ButtonImportVocabulary"
        Me.ButtonImportVocabulary.Size = New System.Drawing.Size(149, 22)
        Me.ButtonImportVocabulary.Text = "Import Vocabulary"
        '
        'ButtonExportVocabulary
        '
        Me.ButtonExportVocabulary.Image = CType(resources.GetObject("ButtonExportVocabulary.Image"), System.Drawing.Image)
        Me.ButtonExportVocabulary.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonExportVocabulary.Name = "ButtonExportVocabulary"
        Me.ButtonExportVocabulary.Size = New System.Drawing.Size(149, 22)
        Me.ButtonExportVocabulary.Text = "Export Vocabulary"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(6, 25)
        '
        'ToolStripButton5
        '
        Me.ToolStripButton5.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton5.Image = CType(resources.GetObject("ToolStripButton5.Image"), System.Drawing.Image)
        Me.ToolStripButton5.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripButton5.Name = "ToolStripButton5"
        Me.ToolStripButton5.Size = New System.Drawing.Size(23, 22)
        Me.ToolStripButton5.Text = "He&lp"
        '
        'NgramSizeText
        '
        Me.NgramSizeText.Font = New System.Drawing.Font("Segoe UI", 9.0!, System.Drawing.FontStyle.Bold)
        Me.NgramSizeText.Name = "NgramSizeText"
        Me.NgramSizeText.Size = New System.Drawing.Size(100, 25)
        Me.NgramSizeText.Text = "2"
        '
        'SplitContainer3
        '
        Me.SplitContainer3.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SplitContainer3.Location = New System.Drawing.Point(0, 0)
        Me.SplitContainer3.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.SplitContainer3.Name = "SplitContainer3"
        Me.SplitContainer3.Orientation = System.Windows.Forms.Orientation.Horizontal
        '
        'SplitContainer3.Panel1
        '
        Me.SplitContainer3.Panel1.BackColor = System.Drawing.Color.Gray
        Me.SplitContainer3.Panel1.Controls.Add(Me.ComboBoxSelectFile)
        Me.SplitContainer3.Panel1.Controls.Add(Me.ToolStripInputDocControl)
        '
        'SplitContainer3.Panel2
        '
        Me.SplitContainer3.Panel2.Controls.Add(Me.RTB_TEXT_IN)
        Me.SplitContainer3.Size = New System.Drawing.Size(790, 423)
        Me.SplitContainer3.SplitterDistance = 64
        Me.SplitContainer3.SplitterWidth = 5
        Me.SplitContainer3.TabIndex = 0
        '
        'ComboBoxSelectFile
        '
        Me.ComboBoxSelectFile.BackColor = System.Drawing.Color.LemonChiffon
        Me.ComboBoxSelectFile.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ComboBoxSelectFile.Font = New System.Drawing.Font("Arial Monospace", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.ComboBoxSelectFile.FormattingEnabled = True
        Me.ComboBoxSelectFile.Location = New System.Drawing.Point(0, 37)
        Me.ComboBoxSelectFile.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.ComboBoxSelectFile.Name = "ComboBoxSelectFile"
        Me.ComboBoxSelectFile.Size = New System.Drawing.Size(790, 27)
        Me.ComboBoxSelectFile.TabIndex = 4
        '
        'ToolStripInputDocControl
        '
        Me.ToolStripInputDocControl.BackColor = System.Drawing.Color.Gray
        Me.ToolStripInputDocControl.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.OpenFileToolStripButton, Me.ButtonTokenizeDocument, Me.OpenFolderToolStripButton, Me.ButtonTrainCorpus, Me.ButtonAddDoc, Me.ButtonRemoveDOc, Me.toolStripSeparator, Me.HelpToolStripButton})
        Me.ToolStripInputDocControl.Location = New System.Drawing.Point(0, 0)
        Me.ToolStripInputDocControl.Name = "ToolStripInputDocControl"
        Me.ToolStripInputDocControl.Padding = New System.Windows.Forms.Padding(0, 0, 2, 0)
        Me.ToolStripInputDocControl.Size = New System.Drawing.Size(790, 25)
        Me.ToolStripInputDocControl.TabIndex = 3
        Me.ToolStripInputDocControl.Text = "ToolStrip1"
        '
        'OpenFileToolStripButton
        '
        Me.OpenFileToolStripButton.Image = CType(resources.GetObject("OpenFileToolStripButton.Image"), System.Drawing.Image)
        Me.OpenFileToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.OpenFileToolStripButton.Name = "OpenFileToolStripButton"
        Me.OpenFileToolStripButton.Size = New System.Drawing.Size(90, 22)
        Me.OpenFileToolStripButton.Text = "&Open File"
        '
        'ButtonTokenizeDocument
        '
        Me.ButtonTokenizeDocument.Image = CType(resources.GetObject("ButtonTokenizeDocument.Image"), System.Drawing.Image)
        Me.ButtonTokenizeDocument.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonTokenizeDocument.Name = "ButtonTokenizeDocument"
        Me.ButtonTokenizeDocument.Size = New System.Drawing.Size(157, 22)
        Me.ButtonTokenizeDocument.Text = "Tokenize Document"
        '
        'OpenFolderToolStripButton
        '
        Me.OpenFolderToolStripButton.Image = CType(resources.GetObject("OpenFolderToolStripButton.Image"), System.Drawing.Image)
        Me.OpenFolderToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.OpenFolderToolStripButton.Name = "OpenFolderToolStripButton"
        Me.OpenFolderToolStripButton.Size = New System.Drawing.Size(107, 22)
        Me.OpenFolderToolStripButton.Text = "&Open Folder"
        '
        'ButtonTrainCorpus
        '
        Me.ButtonTrainCorpus.Image = CType(resources.GetObject("ButtonTrainCorpus.Image"), System.Drawing.Image)
        Me.ButtonTrainCorpus.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonTrainCorpus.Name = "ButtonTrainCorpus"
        Me.ButtonTrainCorpus.Size = New System.Drawing.Size(111, 22)
        Me.ButtonTrainCorpus.Text = "Train Corpus"
        '
        'ButtonAddDoc
        '
        Me.ButtonAddDoc.Image = CType(resources.GetObject("ButtonAddDoc.Image"), System.Drawing.Image)
        Me.ButtonAddDoc.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonAddDoc.Name = "ButtonAddDoc"
        Me.ButtonAddDoc.Size = New System.Drawing.Size(82, 22)
        Me.ButtonAddDoc.Text = "Add Doc"
        '
        'ButtonRemoveDOc
        '
        Me.ButtonRemoveDOc.Image = CType(resources.GetObject("ButtonRemoveDOc.Image"), System.Drawing.Image)
        Me.ButtonRemoveDOc.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ButtonRemoveDOc.Name = "ButtonRemoveDOc"
        Me.ButtonRemoveDOc.Size = New System.Drawing.Size(109, 22)
        Me.ButtonRemoveDOc.Text = "Remove Doc"
        '
        'toolStripSeparator
        '
        Me.toolStripSeparator.Name = "toolStripSeparator"
        Me.toolStripSeparator.Size = New System.Drawing.Size(6, 25)
        '
        'HelpToolStripButton
        '
        Me.HelpToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.HelpToolStripButton.Image = CType(resources.GetObject("HelpToolStripButton.Image"), System.Drawing.Image)
        Me.HelpToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.HelpToolStripButton.Name = "HelpToolStripButton"
        Me.HelpToolStripButton.Size = New System.Drawing.Size(23, 22)
        Me.HelpToolStripButton.Text = "He&lp"
        '
        'RTB_TEXT_IN
        '
        Me.RTB_TEXT_IN.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RTB_TEXT_IN.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RTB_TEXT_IN.EnableAutoDragDrop = True
        Me.RTB_TEXT_IN.Font = New System.Drawing.Font("Arial Monospace", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.RTB_TEXT_IN.Location = New System.Drawing.Point(0, 0)
        Me.RTB_TEXT_IN.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.RTB_TEXT_IN.Name = "RTB_TEXT_IN"
        Me.RTB_TEXT_IN.Size = New System.Drawing.Size(790, 354)
        Me.RTB_TEXT_IN.TabIndex = 2
        Me.RTB_TEXT_IN.Text = ""
        '
        'ModelControlTokenizer
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(10.0!, 15.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Transparent
        Me.Controls.Add(Me.SplitContainer1)
        Me.Font = New System.Drawing.Font("Arial Monospace", 11.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(2, Byte))
        Me.Margin = New System.Windows.Forms.Padding(5, 3, 5, 3)
        Me.Name = "ModelControlTokenizer"
        Me.Size = New System.Drawing.Size(790, 823)
        Me.SplitContainer1.Panel1.ResumeLayout(False)
        Me.SplitContainer1.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer1.ResumeLayout(False)
        Me.SplitContainer2.Panel1.ResumeLayout(False)
        Me.SplitContainer2.Panel2.ResumeLayout(False)
        Me.SplitContainer2.Panel2.PerformLayout()
        CType(Me.SplitContainer2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer2.ResumeLayout(False)
        Me.SplitContainer4.Panel1.ResumeLayout(False)
        Me.SplitContainer4.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer4, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer4.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.ToolStripModelControl.ResumeLayout(False)
        Me.ToolStripModelControl.PerformLayout()
        Me.SplitContainer3.Panel1.ResumeLayout(False)
        Me.SplitContainer3.Panel1.PerformLayout()
        Me.SplitContainer3.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer3, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer3.ResumeLayout(False)
        Me.ToolStripInputDocControl.ResumeLayout(False)
        Me.ToolStripInputDocControl.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents SplitContainer1 As Windows.Forms.SplitContainer
    Friend WithEvents SplitContainer2 As Windows.Forms.SplitContainer
    Friend WithEvents SplitContainer3 As Windows.Forms.SplitContainer
    Friend WithEvents RTB_TEXT_IN As Windows.Forms.RichTextBox
    Friend WithEvents ToolStripModelControl As Windows.Forms.ToolStrip
    Friend WithEvents ButtonNewModel As Windows.Forms.ToolStripButton
    Friend WithEvents ButtonExportModel As Windows.Forms.ToolStripButton
    Friend WithEvents ButtonImportModel As Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator1 As Windows.Forms.ToolStripSeparator
    Friend WithEvents ToolStripButton5 As Windows.Forms.ToolStripButton
    Friend WithEvents ButtonExportVocabulary As Windows.Forms.ToolStripButton
    Friend WithEvents ButtonImportVocabulary As Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripInputDocControl As Windows.Forms.ToolStrip
    Friend WithEvents OpenFileToolStripButton As Windows.Forms.ToolStripButton
    Friend WithEvents OpenFolderToolStripButton As Windows.Forms.ToolStripButton
    Friend WithEvents toolStripSeparator As Windows.Forms.ToolStripSeparator
    Friend WithEvents ButtonRemoveDOc As Windows.Forms.ToolStripButton
    Friend WithEvents ButtonAddDoc As Windows.Forms.ToolStripButton
    Friend WithEvents HelpToolStripButton As Windows.Forms.ToolStripButton
    Friend WithEvents ButtonTokenizeDocument As Windows.Forms.ToolStripButton
    Friend WithEvents ButtonTrainCorpus As Windows.Forms.ToolStripButton
    Friend WithEvents ComboBoxModelList As Windows.Forms.ComboBox
    Friend WithEvents ComboBoxSelectFile As Windows.Forms.ComboBox
    Friend WithEvents SplitContainer4 As Windows.Forms.SplitContainer
    Friend WithEvents RTB_DOC_OUT As Windows.Forms.RichTextBox
    Friend WithEvents GroupBox1 As Windows.Forms.GroupBox
    Friend WithEvents RTB_Info As Windows.Forms.RichTextBox
    Friend WithEvents NgramSizeText As Windows.Forms.ToolStripTextBox
End Class
