Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports SpydazWebAI.TokenizerModels.BaseModels

Public Class ModelControlTokenizer
    Private TokenizerControl As New TokenizerControlAPI

    Private Sub ButtonImportVocabulary_Click(sender As Object, e As EventArgs) Handles ButtonImportVocabulary.Click
        TokenizerControl.LoadVocabularyList()
        RTB_Info.Text &= ": Vocabulary Loaded :" & vbNewLine
        For Each item In TokenizerControl.Vocabulary
            RTB_Info.Text &= item & vbNewLine
        Next

    End Sub

    Private Sub OpenFileToolStripButton_Click(sender As Object, e As EventArgs) Handles OpenFileToolStripButton.Click
        TokenizerControl.OpenDocForProcessing()
        RTB_TEXT_IN.Text = TokenizerControl.CurrentDoc

        RTB_Info.Text &= "File Loaded :" & TokenizerControl.CurrentFile & vbNewLine
    End Sub

    Private Sub OpenFolderToolStripButton_Click(sender As Object, e As EventArgs) Handles OpenFolderToolStripButton.Click
        TokenizerControl.OpenTrainingCorpus()
        For Each item In TokenizerControl.CorpusFilenames
            Me.ComboBoxSelectFile.Items.Add(item)
        Next
        RTB_Info.Text &= "File Loaded :" & TokenizerControl.CurrentFile & vbNewLine

    End Sub

    Private Sub ComboBoxSelectFile_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBoxSelectFile.SelectedIndexChanged
        TokenizerControl.CurrentDoc = TokenizerControl.Corpus(ComboBoxSelectFile.SelectedIndex)
        TokenizerControl.CurrentFile = ComboBoxModelList.SelectedItem.text
        RTB_Info.Text &= "File Loaded :" & TokenizerControl.CurrentFile & vbNewLine

    End Sub

    Private Sub ModelControlTokenizer_Load(sender As Object, e As EventArgs) Handles Me.Load
        For Each item In TokenizerControl.GetTokenizerLst
            ComboBoxModelList.Items.Add(item)
        Next
    End Sub
    Public iTokenizer As Tokenizer
    Private TokenizerTypeStr As String

    Private Sub ButtonExportModel_Click(sender As Object, e As EventArgs) Handles ButtonExportModel.Click

    End Sub

    Private Sub ButtonExportVocabulary_Click(sender As Object, e As EventArgs) Handles ButtonExportVocabulary.Click

    End Sub

    Private Sub ButtonTrainCorpus_Click(sender As Object, e As EventArgs) Handles ButtonTrainCorpus.Click

    End Sub

    Private Sub ButtonAddDoc_Click(sender As Object, e As EventArgs) Handles ButtonAddDoc.Click

    End Sub

    Private Sub ButtonRemoveDOc_Click(sender As Object, e As EventArgs) Handles ButtonRemoveDOc.Click

    End Sub
End Class
Public Class TokenizerControlAPI
    Public CorpusPath As String
    Public Corpus As List(Of String)
    Public CorpusFilenames As List(Of String)
    Public CurrentDoc As String
    Public CurrentFile As String
    Public CurrentTokenizedDoc As String
    Public CurrentTokenizer As Tokenizer
    Public Tokenizers As List(Of String)
    Public Vocabulary As List(Of String)

    Public Shared Sub ExportTokenizer(ByRef PretrainedTokenizer As Tokenizer)
        Dim saveFileDialog As New SaveFileDialog()

        saveFileDialog.Filter = "model Files|*.bin"
        saveFileDialog.Title = "Save model to File"

        If saveFileDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedFilePath As String = saveFileDialog.FileName
            ModelExporter(PretrainedTokenizer, selectedFilePath)

        End If
    End Sub

    Public Shared Function ImportTokenizer(ByRef PretrainedTokenizer As Tokenizer) As Tokenizer
        Dim openFileDialog As New OpenFileDialog()

        openFileDialog.Filter = "Tokenizer Models Files|*.bin"
        openFileDialog.Title = "Select a Tokenizer Model"
        If openFileDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedFilePath As String = openFileDialog.FileName

            PretrainedTokenizer = ModelImporter(PretrainedTokenizer, selectedFilePath)

        End If
        Return PretrainedTokenizer
    End Function

    Public Shared Function LoadListFromFile(filePath As String) As List(Of String)
        Dim strings As New List(Of String)

        Try
            If File.Exists(filePath) Then
                Dim lines As String() = File.ReadAllLines(filePath)
                strings.AddRange(lines)
            Else
                Console.WriteLine("File not found: " & filePath)
            End If
        Catch ex As Exception
            Console.WriteLine("An error occurred: " & ex.Message)
        End Try

        Return strings
    End Function

    Public Shared Function LoadTextFilesFromDirectory(directoryPath As String) As List(Of String)
        Dim textList As New List(Of String)

        Try
            If Directory.Exists(directoryPath) Then
                Dim textFiles As String() = Directory.GetFiles(directoryPath, "*.txt")

                For Each filePath As String In textFiles
                    Dim text As String = File.ReadAllText(filePath)
                    textList.Add(text)

                Next
            Else
                MessageBox.Show("Directory not found: " & directoryPath, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        Catch ex As Exception
            MessageBox.Show("An error occurred: " & ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

        Return textList
    End Function

    Public Shared Function LoadTextFromFile(filePath As String) As String
        Dim text As String = String.Empty

        Try
            If File.Exists(filePath) Then
                text = File.ReadAllText(filePath)
            Else
                MessageBox.Show("File not found: " & filePath, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        Catch ex As Exception
            MessageBox.Show("An error occurred: " & ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

        Return text
    End Function

    Public Shared Sub ModelExporter(ByRef Model As Object, Filename As String)
        Dim path As String = Application.StartupPath

        Dim FileStream As New System.IO.FileStream(Filename, System.IO.FileMode.CreateNew)
        Dim Formatter As New BinaryFormatter
        Formatter.Serialize(Model, FileStream)
        FileStream.Close()

    End Sub

    Public Shared Function ModelImporter(ByRef Model As Object, ByRef Filename As String) As Object
        Dim FileStream As New System.IO.FileStream(Filename, System.IO.FileMode.Open)
        Dim Formatter As New BinaryFormatter
        Model = Formatter.Deserialize(FileStream)
        FileStream.Close()

        Return Model
    End Function

    Public Shared Sub SaveListToFile(strings As List(Of String), filePath As String)
        Try
            File.WriteAllLines(filePath, strings)
            Console.WriteLine("Strings saved to file: " & filePath)
        Catch ex As Exception
            Console.WriteLine("An error occurred: " & ex.Message)
        End Try
    End Sub

    Public Shared Sub SaveTextsToFiles(textStrings As List(Of String), fileNames As List(Of String), directoryPath As String)
        If textStrings.Count <> fileNames.Count Then
            MessageBox.Show("Number of text strings and filenames should match.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return
        End If

        Try
            If Not Directory.Exists(directoryPath) Then
                Directory.CreateDirectory(directoryPath)
            End If

            For i As Integer = 0 To textStrings.Count - 1
                Dim filePath As String = Path.Combine(directoryPath, fileNames(i) & ".txt")
                File.WriteAllText(filePath, textStrings(i))
            Next

            MessageBox.Show("Texts saved to files in directory: " & directoryPath, "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Catch ex As Exception
            MessageBox.Show("An error occurred: " & ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Public Shared Sub SaveTextToFile(text As String, filePath As String)
        Try
            File.WriteAllText(filePath, text)
            MessageBox.Show("Text saved to file: " & filePath, "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Catch ex As Exception
            MessageBox.Show("An error occurred: " & ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Public Function GetTokenizerLst() As List(Of String)
        Dim lst As New List(Of String)
        lst.Add("WORDPIECE")
        lst.Add("BPE")
        lst.Add("CharGram")
        lst.Add("WordGram")
        lst.Add("SentenceGram")
        lst.Add("WORD")
        lst.Add("SENTENCE")
        lst.Add("PARAGRAPH")
        lst.Add("TOKEN_IDS")
        lst.Add("Positional_Tokens")

        Return lst
    End Function

    Public Function LoadCorpusFromDirectory(directoryPath As String) As List(Of String)
        Dim textList As New List(Of String)

        Try
            If Directory.Exists(directoryPath) Then
                Dim textFiles As String() = Directory.GetFiles(directoryPath, "*.txt")

                For Each filePath As String In textFiles
                    Dim text As String = File.ReadAllText(filePath)
                    CorpusFilenames.Add(filePath)
                    CorpusPath = filePath
                    textList.Add(text)
                Next
            Else
                Console.WriteLine("Directory not found: " & directoryPath)
            End If
        Catch ex As Exception
            Console.WriteLine("An error occurred: " & ex.Message)
        End Try

        Return textList
    End Function

    Public Sub LoadVocabularyList()
        Dim openFileDialog As New OpenFileDialog()

        openFileDialog.Filter = "txt Files|*.txt"
        openFileDialog.Title = "Select a Vocabulary list"

        If openFileDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedFilePath As String = openFileDialog.FileName

            Vocabulary = LoadListFromFile(selectedFilePath)

        End If
    End Sub

    Public Sub OpenDocForProcessing()
        Dim openFileDialog As New OpenFileDialog()

        openFileDialog.Filter = "Text Files|*.txt"
        openFileDialog.Title = "Select a Text File"

        If openFileDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedFilePath As String = openFileDialog.FileName

            Dim text As String = LoadTextFromFile(selectedFilePath)
            If Not String.IsNullOrEmpty(text) Then
                CurrentDoc = text
                CurrentFile = selectedFilePath
            End If
        End If
    End Sub

    Public Sub OpenTrainingCorpus()
        Dim folderBrowserDialog As New FolderBrowserDialog()

        folderBrowserDialog.Description = "Select a Directory"

        If folderBrowserDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedDirectoryPath As String = folderBrowserDialog.SelectedPath

            Dim textList As List(Of String) = LoadCorpusFromDirectory(selectedDirectoryPath)
            'Filenames were previously added
            Corpus = textList  ' Now you can process the textList as needed.
        End If
    End Sub

    Public Sub SaveTokenizedDoc()
        Dim saveFileDialog As New SaveFileDialog()

        saveFileDialog.Filter = "Text Files|*.txt"
        saveFileDialog.Title = "Save Text to File"

        If saveFileDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedFilePath As String = saveFileDialog.FileName
            SaveTextToFile(CurrentTokenizedDoc, selectedFilePath)

        End If
    End Sub

End Class