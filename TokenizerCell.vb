Imports System.IO
Imports SpydazWebAI.TokenizerModels
Imports SpydazWebAI.TokenizerModels.BaseModels
Imports SpydazWebAI.TokenizerModels.BertModels
Imports SpydazWebAI.TokenizerModels.BertModels.Base

Public Class TokenizerCell
    Private CurrentDoc As String
    Private CurrentFile As String
    Private Corpus As List(Of String)
    Private CorpusFilenames As Object
    Private CorpusPath As String
    Private CurrentTokenizedDoc As String
    Private CurrentVocabulary As List(Of String)
    Public CurrentModel As Tokenizer
    Private CurrentBertModel As Bert_Base

    Private Function LoadCorpusFromDirectory(directoryPath As String) As List(Of String)
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
    Public Sub LoadTokenizer(ByRef iTokenizerType As TokenizerType)
        Select Case iTokenizerType

            Case TokenizerType._BytePairEncoding
                CurrentModel = New BytePairEncoding
            Case TokenizerType._Hybrid
                CurrentModel = New HybridTokenizer
            Case TokenizerType._TokenEncoder
                CurrentModel = New TokenEncoder

            Case TokenizerType._BertUnCased
                CurrentBertModel = New BertUncasedLargeTokenizer
            Case TokenizerType._BertCased
                CurrentBertModel = New BertCasedLargeTokenizer
            Case TokenizerType._BertMultilingual
                CurrentBertModel = New BertMultilingualTokenizer

        End Select
    End Sub
    Private Sub OpenFileToolStripButton_Click(sender As Object, e As EventArgs) Handles OpenFileToolStripButton.Click
        OpenDocForProcessing()
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
    Private Sub OpenFolderToolStripButton_Click(sender As Object, e As EventArgs) Handles OpenFolderToolStripButton.Click
        OpenTrainingCorpus()
    End Sub
    Private Sub SaveOutputToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveOutputToolStripMenuItem.Click
        CurrentTokenizedDoc = RTB_DOC_OUT.Text
        SaveTokenizedDoc()
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
    Private Sub ButtonImportVocabulary_Click(sender As Object, e As EventArgs) Handles ButtonImportVocabulary.Click
        CurrentVocabulary = LoadVocabularyList()
    End Sub
    Private Sub ButtonExportVocabulary_Click(sender As Object, e As EventArgs) Handles ButtonExportVocabulary.Click
        SaveVocabulary()
    End Sub
    Public Sub SaveVocabulary()
        Dim saveFileDialog As New SaveFileDialog()

        saveFileDialog.Filter = "Text Files|*.txt"
        saveFileDialog.Title = "Save Text to File"

        If saveFileDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedFilePath As String = saveFileDialog.FileName

            SaveListToFile(CurrentVocabulary, selectedFilePath)

        End If
    End Sub

    Private Sub ButtonTrainCorpus_Click(sender As Object, e As EventArgs) Handles ButtonTrainCorpus.Click
        Train(Corpus)
        RTB_Info.Text = "Trained Corpus" & vbNewLine
        RTB_Info.Text &= vbNewLine & "Corpus Count :" & Corpus.Count
    End Sub

    Private Sub ButtonTokenizeDocument_Click(sender As Object, e As EventArgs) Handles ButtonTokenizeDocument.Click
        Dim Tokens As List(Of String) = Forwards(CurrentDoc)
        DisplayTokenizedOutput(Tokens)
    End Sub
    Public Sub Train(ByRef Corpus As List(Of String))

    End Sub
    Public Function Forwards(ByRef Doc As String) As List(Of String)
        Dim Tokens As New List(Of String)


        Return Tokens
    End Function

    Private Sub ToolStripButtonToChars_Click(sender As Object, e As EventArgs) Handles ToolStripButtonToChars.Click


        Dim Tokens As List(Of String) = TokenizerBase.TokenizeToCharacter(CurrentDoc)
        DisplayTokenizedOutput(Tokens)
    End Sub

    Private Sub DisplayTokenizedOutput(Tokens As List(Of String))
        RTB_DOC_OUT.Clear()
        RTB_Info.Text = "Tokenized Text" & vbNewLine
        For Each item In Tokens
            RTB_DOC_OUT.Text &= item & ", "
        Next
        RTB_Info.Text &= vbNewLine & "Token Count :" & Tokens.Count
    End Sub

    Private Sub ToolStripButtonToWords_Click(sender As Object, e As EventArgs) Handles ToolStripButtonToWords.Click
        Dim Tokens As List(Of String) = TokenizerBase.TokenizeToWord(CurrentDoc)
        DisplayTokenizedOutput(Tokens)
    End Sub

    Private Sub ToolStripButtonToSentences_Click(sender As Object, e As EventArgs) Handles ToolStripButtonToSentences.Click
        Dim Tokens As List(Of String) = TokenizerBase.TokenizeToSentence(CurrentDoc)
        DisplayTokenizedOutput(Tokens)
    End Sub


End Class
