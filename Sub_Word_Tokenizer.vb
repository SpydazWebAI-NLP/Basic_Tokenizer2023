Imports System.IO

Namespace BPE_Tokenizer

    Public Class Sub_Word_Tokenizer

        Public Enum Type
            _Char
            _Word
            _Sentence
            _Paragraph
            _Sub_Word
            _Sub_Sentence
            _Sub_Paragraph
            _BPE
        End Enum

        Public Enum TokenType
            GramaticalPunctuation
            EncapuslationPunctuationStart
            EncapuslationPunctuationEnd
            MoneyPunctuation
            MathPunctuation
            CodePunctuation
            AlphaBet
            Number
            Symbol
            SeperatorPunctuation
            Ignore
            Word
            Sentence
            Character
            Ngram
            WordGram
            SentenceGram
            BitWord
            Punctuation
            whitespace
        End Enum

        Public Function ExportModel() As Dictionary(Of String, Integer)
            Return Vocabulary
        End Function

        Public Property Vocabulary As Dictionary(Of String, Integer)

        Public Sub New()
            Vocabulary = New Dictionary(Of String, Integer)
        End Sub

        Public Shared Function GetDocumentVocabulary(ByRef Tokens As List(Of String)) As Dictionary(Of String, Integer)
            'Return Uni-grams
            Return iTrain.GetNgramCounts(Tokens, 1)
        End Function

        ''' <summary>
        ''' Tokenizes to Ngrams
        ''' </summary>
        ''' <param name="Document"></param>
        ''' <param name="Ngram_TokenizerType"></param>
        ''' <param name="nGramSize"></param>
        ''' <returns></returns>
        Public Shared Function Tokenize(Document As String, ByRef Ngram_TokenizerType As Type, ByRef nGramSize As Integer) As List(Of String)
            Tokenize = New List(Of String)
            Select Case Ngram_TokenizerType
                Case Type._Char
                    Return iTokenize.Tokenize(Document, Type._Char, nGramSize)
                Case Type._Word
                    Return iTokenize.Tokenize(Document, Type._Word, nGramSize)
                Case Type._Paragraph
                    Return iTokenize.Tokenize(Document, Type._Paragraph, nGramSize)
                Case Type._Sentence
                    Return iTokenize.Tokenize(Document, Type._Sentence, nGramSize)

            End Select

        End Function

        ''' <summary>
        ''' Tokenizer to Sub_Word
        ''' </summary>
        ''' <param name="Document"></param>
        ''' <param name="TokenizerType"></param>
        ''' <param name="Vocabulary"></param>
        ''' <returns></returns>
        Public Shared Function Tokenize(Document As String, ByRef TokenizerType As Type, ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
            Tokenize = New List(Of String)
            Select Case TokenizerType

                Case Type._Sub_Word
                    Return iTokenize.Tokenize(Document, Type._Sub_Word, Vocabulary)
                Case Type._Sub_Sentence
                    Return iTokenize.Tokenize(Document, Type._Sub_Sentence, Vocabulary)

                Case Type._Sub_Paragraph
                    Return iTokenize.Tokenize(Document, Type._Sub_Paragraph, Vocabulary)
                Case Type._BPE
                    Return iTokenize.Tokenize(Document, Type._BPE, Vocabulary)

            End Select

        End Function

        ''' <summary>
        ''' Tokenizes
        ''' </summary>
        ''' <param name="Document"></param>
        ''' <param name="TokenizerType"></param>
        ''' <returns></returns>
        Public Shared Function Tokenize(Document As String, ByRef TokenizerType As Type) As List(Of String)
            Tokenize = New List(Of String)
            Select Case TokenizerType
                Case Type._Char
                    Return iTokenize.Tokenize(Document, Type._Char)
                Case Type._Word
                    Return iTokenize.Tokenize(Document, Type._Word)
                Case Type._Paragraph
                    Return iTokenize.Tokenize(Document, Type._Paragraph)
                Case Type._Sentence
                    Return iTokenize.Tokenize(Document, Type._Sentence)

            End Select

        End Function

        Public Sub New(vocabulary As Dictionary(Of String, Integer))
            If vocabulary Is Nothing Then
                Throw New ArgumentNullException(NameOf(vocabulary))
            End If

            Me.Vocabulary = vocabulary
        End Sub

        Public Shared Function Forward(ByRef Document As String, ByRef Vocab As Dictionary(Of String, Integer)) As List(Of String)
            Forward = New List(Of String)
            'Using Vocabulary Tokenizer

            'should be tokenized using SubWord Tokenization
            ' - based on the sub-words in the vocabulary
            'NOTHING SHOULD BE ADDED TO VOCABULARY (except Unknown CHARS)
            '- In the forwards pass it is assumed that it is trained ,#
            Dim iTokenizer As New iTokenize(Vocab)

            Return iTokenizer.Tokenize(Document)
        End Function

        Public Function Forward(ByRef Document As String) As List(Of String)
            Forward = New List(Of String)
            'Using Vocabulary Tokenizer

            'should be tokenized using SubWord Tokenization
            ' - based on the sub-words in the vocabulary
            'NOTHING SHOULD BE ADDED TO VOCABULARY (except Unknown CHARS)
            '- In the forwards pass it is assumed that it is trained ,#
            Dim iTokenizer As New iTokenize(Vocabulary)

            Return iTokenizer.Tokenize(Document)
        End Function

        ''' <summary>
        ''' Trains the vocabulary for the tokenizer
        ''' </summary>
        ''' <param name="Corpus"></param>
        ''' <param name="Vocabulary"></param>
        ''' <param name="Threshold"></param>
        ''' <returns></returns>
        Public Function TrainVocabulary(ByRef Corpus As List(Of String), ByRef Vocabulary As Dictionary(Of String, Integer), ByRef Threshold As Integer) As Dictionary(Of String, Integer)
            Dim trainer As New iTrain(Vocabulary)

            Return trainer.Train(Corpus)
        End Function

        Private Class iTokenize

            ''' <summary>
            ''' Tokenizes to Ngrams
            ''' </summary>
            ''' <param name="Document"></param>
            ''' <param name="Ngram_TokenizerType"></param>
            ''' <param name="nGramSize"></param>
            ''' <returns></returns>
            Public Shared Function Tokenize(Document As String, ByRef Ngram_TokenizerType As Type, ByRef nGramSize As Integer) As List(Of String)
                Tokenize = New List(Of String)
                Select Case Ngram_TokenizerType
                    Case Type._Char
                        Return TokenizeToNgrams.TokenizetoCharacter(Document, nGramSize)
                    Case Type._Word
                        Return TokenizeToNgrams.TokenizetoWord(Document, nGramSize)
                    Case Type._Paragraph
                        Return TokenizeToNgrams.TokenizetoParagraph(Document, nGramSize)
                    Case Type._Sentence
                        Return TokenizeToNgrams.TokenizetoSentence(Document, nGramSize)

                End Select

            End Function

            ''' <summary>
            ''' Tokenizer to Sub_Word
            ''' </summary>
            ''' <param name="Document"></param>
            ''' <param name="TokenizerType"></param>
            ''' <param name="Vocabulary"></param>
            ''' <returns></returns>
            Public Shared Function Tokenize(Document As String, ByRef TokenizerType As Type, ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
                Tokenize = New List(Of String)
                Select Case TokenizerType

                    Case Type._Sub_Word
                        Return TokenizeToSubWords.TokenizeToWord(Document, Vocabulary)
                    Case Type._Sub_Sentence
                        Return TokenizeToSubWords.TokenizeToSentence(Document, Vocabulary)
                    Case Type._Sub_Sentence
                        Return TokenizeToSubWords.TokenizeToParagraph(Document, Vocabulary)
                    Case Type._BPE
                        Return TokenizeToBPE(Document, Vocabulary)

                End Select

            End Function

            ''' <summary>
            ''' Tokenizes
            ''' </summary>
            ''' <param name="Document"></param>
            ''' <param name="TokenizerType"></param>
            ''' <returns></returns>
            Public Shared Function Tokenize(Document As String, ByRef TokenizerType As Type) As List(Of String)
                Tokenize = New List(Of String)
                Select Case TokenizerType
                    Case Type._Char
                        Return BasicTokenizer.TokenizeToCharacter(Document)
                    Case Type._Word
                        Return BasicTokenizer.TokenizeToWord(Document)
                    Case Type._Paragraph
                        Return BasicTokenizer.TokenizeToParagraph(Document)
                    Case Type._Sentence
                        Return BasicTokenizer.TokenizeToSentence(Document)

                End Select

            End Function

            Public Property Vocabulary As Dictionary(Of String, Integer)

            Public Sub New()
                Vocabulary = New Dictionary(Of String, Integer)
            End Sub

            Public Sub New(ByRef iVocabulary As Dictionary(Of String, Integer))
                Vocabulary = iVocabulary
            End Sub

            ''' <summary>
            ''' Tokenizer Using Vocabulary(BPE)
            ''' </summary>
            ''' <param name="Document"></param>
            ''' <returns></returns>
            Public Function Tokenize(Document As String) As List(Of String)
                Return TokenizeToBPE(Document, Vocabulary)
            End Function

            ''' <summary>
            ''' Main Function For Encoding, Takes a document and encodes the
            ''' document based on the vocabulary supplied
            ''' using the Byte Pair Encoding system
            ''' </summary>
            ''' <param name="Document"></param>
            ''' <param name="Vocabulary"></param>
            ''' <returns></returns>
            Private Shared Function TokenizeToBPE(ByVal Document As String, ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
                ' Tokenize a word into subwords using the BPE algorithm based on the learned Vocabulary
                Dim subwords As List(Of String) = New List(Of String)

                ' Start with the word as a single subword
                Dim subword As String = ""
                For Each c As Char In Document
                    subword += c
                    If Vocabulary.ContainsKey(subword.ToLower()) Then
                        ' If the subword is in the Vocabulary, add it to the list of subwords
                        subwords.Add(subword.ToLower())
                        ' Reset the subword for the next iteration
                        subword = ""
                    End If
                Next

                ' If there is a remaining part of the word that is not in the Vocabulary,
                ' add it as an <unk> subword
                If Not String.IsNullOrEmpty(subword) Then
                    subwords.Add("<unk>")
                End If

                Return subwords
            End Function

            Public Class BasicTokenizer

                Public Shared Function TokenizeToCharacter(Document As String) As List(Of String)
                    Document = Document.ToLower()
                    Dim characters As Char() = Document.ToCharArray()
                    TokenizeToCharacter = New List(Of String)
                    For Each item In characters
                        TokenizeToCharacter.Add(item)
                    Next
                End Function

                Public Shared Function TokenizeToWord(Document As String) As List(Of String)
                    Document = Document.ToLower()
                    Document = Document.SpacePunctuation
                    Return Document.Split({" ", ".", ",", ";", ":", "!", "?"}, StringSplitOptions.RemoveEmptyEntries).ToList
                End Function

                Public Shared Function TokenizeToSentence(Document As String) As List(Of String)
                    Document = Document.ToLower()
                    Document = Document.SpacePunctuation
                    Return Split(Document, ".").ToList
                End Function

                Public Shared Function TokenizeToParagraph(Document As String) As List(Of String)
                    Document = Document.ToLower()

                    Return Split(Document, vbNewLine).ToList
                End Function

            End Class

            Private Class TokenizeToNgrams

                Public Shared Function TokenizetoCharacter(Document As String, n As Integer) As List(Of String)
                    TokenizetoCharacter = New List(Of String)
                    Document = Document.ToLower()
                    Document = Document.SpacePunctuation

                    ' Generate character n-grams
                    For i As Integer = 0 To Document.Length - n
                        Dim ngram As String = Document.Substring(i, n)
                        TokenizetoCharacter.Add(ngram)
                    Next

                End Function

                Public Shared Function TokenizetoWord(ByRef text As String, n As Integer) As List(Of String)
                    TokenizetoWord = New List(Of String)
                    text = text.ToLower()
                    text = text.SpacePunctuation

                    ' Split the clean text into individual words
                    Dim words() As String = text.Split({" ", ".", ",", ";", ":", "!", "?"}, StringSplitOptions.RemoveEmptyEntries)

                    ' Generate n-grams from the words
                    For i As Integer = 0 To words.Length - n
                        Dim ngram As String = String.Join(" ", words.Skip(i).Take(n))
                        TokenizetoWord.Add(ngram)
                    Next

                End Function

                Public Shared Function TokenizetoParagraph(text As String, n As Integer) As List(Of String)
                    TokenizetoParagraph = New List(Of String)

                    ' Split the text into paragraphs
                    Dim paragraphs() As String = text.Split({Environment.NewLine & Environment.NewLine}, StringSplitOptions.RemoveEmptyEntries)

                    ' Generate paragraph n-grams
                    For i As Integer = 0 To paragraphs.Length - n
                        Dim ngram As String = String.Join(Environment.NewLine & Environment.NewLine, paragraphs.Skip(i).Take(n))
                        TokenizetoParagraph.Add(ngram)
                    Next

                    Return TokenizetoParagraph
                End Function

                Public Shared Function TokenizetoSentence(text As String, n As Integer) As List(Of String)
                    Dim tokens As New List(Of String)

                    ' Split the text into sentences
                    Dim sentences() As String = text.Split({".", "!", "?"}, StringSplitOptions.RemoveEmptyEntries)

                    ' Generate sentence n-grams
                    For i As Integer = 0 To sentences.Length - n
                        Dim ngram As String = String.Join(" ", sentences.Skip(i).Take(n))
                        tokens.Add(ngram)
                    Next

                    Return tokens
                End Function

            End Class

            Public Class TokenizeToSubWords

                Public Shared Function TokenizeToWord(ByRef Document As String, ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
                    ' Tokenize the input Document into subwords using BPE based on the learned Vocabulary
                    Dim tokens As List(Of String) = New List(Of String)()
                    Document = Document.ToLower()
                    Document = Document.SpacePunctuation
                    ' Step 1: Tokenize the Document into words
                    Dim words As List(Of String) = iTokenize.Tokenize(Document, Type._Word)

                    ' Step 2: Process each word and tokenize into subwords using BPE
                    For Each word As String In words
                        Dim subwords As List(Of String) = TokenizeToBPE(word, Vocabulary)
                        tokens.AddRange(subwords)
                    Next

                    Return tokens
                End Function

                Public Shared Function TokenizeToSentence(ByRef Document As String, ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
                    ' Tokenize the input Document into subwords using BPE based on the learned Vocabulary
                    Dim tokens As List(Of String) = New List(Of String)()
                    Document = Document.ToLower()
                    Document = Document.SpacePunctuation
                    ' Step 1: Tokenize the Document into words
                    Dim words As List(Of String) = iTokenize.Tokenize(Document, Type._Sentence)

                    ' Step 2: Process each word and tokenize into subwords using BPE
                    For Each word As String In words
                        Dim subwords As List(Of String) = TokenizeToBPE(word, Vocabulary)
                        tokens.AddRange(subwords)
                    Next

                    Return tokens
                End Function

                Public Shared Function TokenizeToParagraph(ByRef Document As String, ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
                    ' Tokenize the input Document into subwords using BPE based on the learned Vocabulary
                    Dim tokens As List(Of String) = New List(Of String)
                    Document = Document.ToLower()
                    Document = Document.SpacePunctuation
                    ' Step 1: Tokenize the Document into words
                    Dim words As List(Of String) = iTokenize.Tokenize(Document, Type._Paragraph)

                    ' Step 2: Process each word and tokenize into subwords using BPE
                    For Each word As String In words
                        Dim subwords As List(Of String) = TokenizeToBPE(word, Vocabulary)
                        tokens.AddRange(subwords)
                    Next

                    Return tokens
                End Function

            End Class

        End Class

        Private Class iTrain
            Inherits Sub_Word_Tokenizer

            Public Sub New(vocabulary As Dictionary(Of String, Integer))
                MyBase.New(vocabulary)
            End Sub

            Public Shared Function GetHighFreq(ByRef Vocabulary As Dictionary(Of String, Integer), ByRef Threshold As Integer) As List(Of String)
                Dim HighFreq As New List(Of String)
                For Each item In Vocabulary
                    If item.Value > Threshold Then
                        HighFreq.Add(item.Key)
                    End If
                Next
                Return HighFreq
            End Function

            Public Shared Function GetNgramCounts(Tokens As List(Of String), N As Integer) As Dictionary(Of String, Integer)
                Dim NgramCounts As New Dictionary(Of String, Integer)

                For Each word In Tokens

                    For i As Integer = 0 To Tokens.Count - N
                        Dim Ngram As String = Tokens(i) & Tokens(i + 1)

                        If NgramCounts.ContainsKey(Ngram) Then
                            NgramCounts(Ngram) += 1
                        Else
                            NgramCounts.Add(Ngram, 1)
                        End If
                    Next
                Next

                Return NgramCounts
            End Function

            Public Shared Function Train(ByRef Corpus As List(Of String), ByRef Vocabulary As Dictionary(Of String, Integer)) As Dictionary(Of String, Integer)
                '3> Note Frequent n-grams can be of any size
                '      (sentence word paragraph, even segments)
                '4> Note when training with large corpuses the vocabulary will contain tokens of many sizes and elements,
                'Vocabulary created from training will contain the tokeniDs used for
                'later embedding models.
                Dim tokens As New List(Of String)
                Dim iTokenizer As New iTokenize(Vocabulary)
                For Each item In Corpus
                    tokens = iTokenizer.Tokenize(item)
                    ' Output the tokenized document
                    For Each itemToken As String In tokens
                        Console.Write(itemToken & " ")
                    Next
                Next

                'Trained
                Return Vocabulary
            End Function

            Public Function Train(ByRef Corpus As List(Of String)) As Dictionary(Of String, Integer)
                '3> Note Frequent n-grams can be of any size
                '      (sentence word paragraph, even segments)
                '4> Note when training with large corpuses the vocabulary will contain tokens of many sizes and elements,
                'Vocabulary created from training will contain the tokeniDs used for
                'later embedding models.
                Dim tokens As New List(Of String)
                Dim iTokenizer As New iTokenize(Vocabulary)
                For Each item In Corpus
                    tokens = iTokenizer.Tokenize(item)
                    ' Output the tokenized document
                    For Each itemToken As String In tokens
                        Console.Write(itemToken & " ")
                    Next
                Next

                'Trained
                Return Vocabulary
            End Function

            Public Shared Function Train_BPE_Vocabulary(corpus As List(Of List(Of String)), ByRef Vocabulary As Dictionary(Of String, Integer), MaxMergeOperations As Integer)
                ' Initialize the vocabulary with word-level subword units
                For Each sentence In corpus
                    For Each word In sentence
                        If Vocabulary.ContainsKey(word) Then
                            Vocabulary(word) += 1
                        Else
                            Vocabulary.Add(word, 1)
                        End If
                    Next
                Next

                Dim mergeOperationsCount As Integer = 0

                While mergeOperationsCount < MaxMergeOperations
                    ' Compute the frequency of subword units in the vocabulary
                    Dim subwordFrequencies As New Dictionary(Of String, Integer)

                    For Each subword In Vocabulary.Keys
                        Dim subwordUnits = subword.Split(" ")
                        For Each unit In subwordUnits
                            If subwordFrequencies.ContainsKey(unit) Then
                                subwordFrequencies(unit) += Vocabulary(subword)
                            Else
                                subwordFrequencies.Add(unit, Vocabulary(subword))
                            End If
                        Next
                    Next

                    ' Find the most frequent pair of subword units
                    Dim mostFrequentPair As KeyValuePair(Of String, Integer) = subwordFrequencies.OrderByDescending(Function(pair) pair.Value).FirstOrDefault()

                    If mostFrequentPair.Value < 2 Then
                        ' Stop merging if the frequency of the most frequent pair is less than 2
                        Exit While
                    End If

                    ' Merge the most frequent pair into a new subword unit
                    Dim newSubwordUnit = mostFrequentPair.Key.Replace(" ", "")

                    ' Update the vocabulary by replacing occurrences of the merged subword pair with the new subword unit
                    Dim updatedVocabulary As New Dictionary(Of String, Integer)

                    For Each subword In Vocabulary.Keys
                        Dim mergedSubword = subword.Replace(mostFrequentPair.Key, newSubwordUnit)
                        updatedVocabulary(mergedSubword) = Vocabulary(subword)
                    Next

                    Vocabulary = updatedVocabulary
                    mergeOperationsCount += 1

                End While
                Return Vocabulary
            End Function

            Public Function GetVocabulary() As List(Of String)
                Return Vocabulary.Keys.ToList()
            End Function

        End Class

        Public Function ReadTextFilesFromDirectory(directoryPath As String) As List(Of String)
            Dim fileList As New List(Of String)()

            Try
                Dim txtFiles As String() = Directory.GetFiles(directoryPath, "*.txt")

                For Each filePath As String In txtFiles
                    Dim fileContent As String = File.ReadAllText(filePath)
                    fileList.Add(fileContent)
                Next
            Catch ex As Exception
                ' Handle any exceptions that may occur while reading the files.
                Console.WriteLine("Error: " & ex.Message)
            End Try

            Return fileList
        End Function

        Public Class PunctuationMarkers
            Public Shared ReadOnly SeperatorPunctuation() As String = {" ", ",", "|"}
            Public Shared ReadOnly Symbols() As String = {"@", "#", "$", "%", "&", "*", "+", "=", "^", "_", "~", "§", "°", "¿", "¡"}
            Public Shared ReadOnly EncapuslationPunctuationEnd() As String = {"}", "]", ">", ")"}
            Public Shared ReadOnly EncapuslationPunctuationStart() As String = {"{", "[", "<", "("}
            Public Shared ReadOnly GramaticalPunctuation() As String = {".", "?", "!", ":", ";", ","}
            Public Shared ReadOnly MathPunctuation = New String() {"+", "-", "*", "/", "=", "<", ">", "≤", "≥", "±", "≈", "≠", "%", "‰", "‱", "^", "_", "√", "∛", "∜", "∫", "∬", "∭", "∮", "∯", "∰", "∇", "∂", "∆", "∏", "∑", "∐", "⨀", "⨁", "⨂", "⨃", "⨄", "∫", "∬", "∭", "∮", "∯", "∰", "∇", "∂", "∆", "∏", "∑", "∐", "⨀", "⨁", "⨂", "⨃", "⨄"}
            Public Shared ReadOnly MoneyPunctuation() As String = {"$", "€", "£", "¥", "₹", "₽", "₿"}
            Public Shared ReadOnly CodePunctuation() As String = {"\", "#", "@", "^"}

            Public Shared ReadOnly Delimiters() As Char = {CType(" ", Char), CType(".", Char),
                    CType(",", Char), CType("?", Char),
                    CType("!", Char), CType(";", Char),
                    CType(":", Char), Chr(10), Chr(13), vbTab}

            Public ReadOnly Property SentenceEndPunctuation As List(Of String)
                Get
                    Dim markers() As String = {".", ";", ":", "!", "?"}
                    Return markers.ToList
                End Get
            End Property

            Public Shared ReadOnly Property Punctuation As List(Of String)
                Get
                    Dim x As New List(Of String)
                    x.AddRange(SeperatorPunctuation)
                    x.AddRange(Symbols)
                    x.AddRange(EncapuslationPunctuationStart)
                    x.AddRange(EncapuslationPunctuationEnd)
                    x.AddRange(MoneyPunctuation)
                    x.AddRange(MathPunctuation)
                    x.AddRange(GramaticalPunctuation)
                    x.AddRange(CodePunctuation)
                    x.AddRange(Delimiters)
                    Return x.Distinct.ToList
                End Get
            End Property

        End Class

        Public Function GetList(ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
            GetList = New List(Of String)
            For Each item In Vocabulary
                GetList.Add(item.Key)
            Next
        End Function

        Public Function GetVocab(ByRef Tokens As List(Of String)) As List(Of String)
            Return Tokens.Distinct.ToList
        End Function

        Public Shared Function FindFrequentCharNgrams(Tokens As List(Of String), N As Integer, ByRef Freq_threshold As Integer) As List(Of String)
            Dim NgramCounts As New Dictionary(Of String, Integer)

            For Each word In Tokens
                Dim characters As List(Of String) = iTokenize.Tokenize(word, Type._Char)

                For Each ngram In iTrain.GetNgramCounts(characters, N)
                    'Update Dictionary
                    If NgramCounts.ContainsKey(ngram.Key) Then

                        NgramCounts(ngram.Key) += ngram.Value
                    Else
                        NgramCounts.Add(ngram.Key, ngram.Value)
                    End If

                Next
            Next

            Return iTrain.GetHighFreq(NgramCounts, Freq_threshold)
        End Function

        Public Shared Function FindFrequentTokenNgrams(Tokens As List(Of String), N As Integer, ByRef Freq_threshold As Integer) As List(Of String)
            Dim NgramCounts As Dictionary(Of String, Integer) = iTrain.GetNgramCounts(Tokens, N)

            Dim frequentWordNgrams As List(Of String) = iTrain.GetHighFreq(NgramCounts, Freq_threshold)

            Return frequentWordNgrams
        End Function

        Public Class RemoveToken

            Public Function ExtractEncapsulated(ByRef Userinput As String) As String
                ExtractEncapsulated = Userinput
                If ContainsEncapsulated(ExtractEncapsulated) = True Then
                    If ExtractEncapsulated.Contains("(") = True And ExtractEncapsulated.Contains(")") = True Then
                        ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("(", ")")
                    End If
                    If Userinput.Contains("[") = True And Userinput.Contains("]") = True Then
                        ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("[", "]")
                    End If
                    If Userinput.Contains("{") = True And Userinput.Contains("}") = True Then
                        ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("{", "}")
                    End If
                    If Userinput.Contains("<") = True And Userinput.Contains(">") = True Then
                        ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("<", ">")
                    End If
                End If
            End Function

            Public Function ContainsEncapsulated(ByRef Userinput As String) As Boolean
                Dim Start = False
                Dim Ending = False
                ContainsEncapsulated = False
                For Each item In PunctuationMarkers.EncapuslationPunctuationStart
                    If Userinput.Contains(item) = True Then Start = True
                Next
                For Each item In PunctuationMarkers.EncapuslationPunctuationEnd
                    If Userinput.Contains(item) = True Then Ending = True
                Next
                If Start And Ending = True Then
                    ContainsEncapsulated = True
                End If
            End Function

            Public Shared Function RemoveBrackets(ByRef Txt As String) As String
                'Brackets
                Txt = Txt.Replace("(", "")
                Txt = Txt.Replace("{", "")
                Txt = Txt.Replace("}", "")
                Txt = Txt.Replace("[", "")
                Txt = Txt.Replace("]", "")
                Return Txt
            End Function

            Public Shared Function RemoveDoubleSpace(ByRef txt As String, Item As String) As String
                Return txt.Replace(Item, "  " & Item & " ")
            End Function

            Public Shared Function RemoveMathsSymbols(ByRef Txt As String) As String
                'Maths Symbols
                Txt = Txt.Replace("+", "")
                Txt = Txt.Replace("=", "")
                Txt = Txt.Replace("-", "")
                Txt = Txt.Replace("/", "")
                Txt = Txt.Replace("*", "")
                Txt = Txt.Replace("<", "")
                Txt = Txt.Replace(">", "")
                Txt = Txt.Replace("%", "")
                Return Txt
            End Function

            Public Shared Function RemovePunctuation(ByRef Txt As String) As String
                'Punctuation
                Txt = Txt.Replace(",", "")
                Txt = Txt.Replace(".", "")
                Txt = Txt.Replace(";", "")
                Txt = Txt.Replace("'", "")
                Txt = Txt.Replace("_", "")
                Txt = Txt.Replace("?", "")
                Txt = Txt.Replace("!", "")
                Txt = Txt.Replace("&", "")
                Txt = Txt.Replace(":", "")

                Return Txt
            End Function

            Public Shared Function RemoveStopWords(ByRef txt As String, ByRef StopWrds As List(Of String)) As String
                For Each item In StopWrds
                    txt = txt.Replace(item, "")
                Next
                Return txt
            End Function

            Public Shared Function RemoveSymbols(ByRef Txt As String) As String
                'Basic Symbols
                Txt = Txt.Replace("£", "")
                Txt = Txt.Replace("$", "")
                Txt = Txt.Replace("^", "")
                Txt = Txt.Replace("@", "")
                Txt = Txt.Replace("#", "")
                Txt = Txt.Replace("~", "")
                Txt = Txt.Replace("\", "")
                Return Txt
            End Function

            Public Shared Function RemoveTokenType(ByRef UserStr As String, ByRef nType As TokenType) As String
                Dim AlphaBet() As String = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}
                Dim Number() As String = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
"30", "40", "50", "60", "70", "80", "90", "00", "000", "0000", "00000", "000000", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
"nineteen", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "hundred", "thousand", "million", "Billion"}

                Select Case nType
                    Case TokenType.GramaticalPunctuation
                        For Each item In PunctuationMarkers.GramaticalPunctuation
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.AlphaBet
                        For Each item In AlphaBet
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.CodePunctuation
                        For Each item In PunctuationMarkers.CodePunctuation
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.EncapuslationPunctuationEnd
                        For Each item In PunctuationMarkers.EncapuslationPunctuationEnd
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.EncapuslationPunctuationStart
                        For Each item In PunctuationMarkers.EncapuslationPunctuationStart
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.Ignore
                    Case TokenType.MathPunctuation
                        For Each item In PunctuationMarkers.MathPunctuation
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.MoneyPunctuation
                        For Each item In PunctuationMarkers.MoneyPunctuation
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.Number
                        For Each item In Number
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next
                    Case TokenType.SeperatorPunctuation
                        For Each item In PunctuationMarkers.SeperatorPunctuation
                            If UCase(UserStr).Contains(UCase(item)) = True Then
                                UserStr = UCase(UserStr).Remove(UCase(item))
                            End If
                        Next

                End Select
                Return UserStr
            End Function

        End Class

        Public Class GetContext

            Public Shared Function GetContext(ByRef corpus As List(Of List(Of String)), ByRef WindowSize As Integer) As List(Of String)
                Dim contextWords As New List(Of String)
                For Each doc In corpus

                    ' Iterate over each sentence in the corpus
                    For Each sentence In doc
                        Dim Words() = Split(sentence, " ")
                        ' Iterate over each word in the sentence
                        For targetIndex = 0 To sentence.Length - 1
                            Dim targetWord As String = sentence(targetIndex)

                            ' Get the context words within the window
                            contextWords = GetContextWordsByIndex(Words.ToList, targetIndex, WindowSize)
                        Next
                    Next

                Next
                Return contextWords
            End Function

            Private Shared Function GetContextWordsByIndex(ByVal sentence As List(Of String), ByVal targetIndex As Integer, ByRef Windowsize As Integer) As List(Of String)
                Dim contextWords As New List(Of String)

                For i = Math.Max(0, targetIndex - Windowsize) To Math.Min(sentence.Count - 1, targetIndex + Windowsize)
                    If i <> targetIndex Then
                        contextWords.Add(sentence(i))
                    End If
                Next

                Return contextWords
            End Function

        End Class

        Public Class CalcSubWords

            Public Shared Function TokenizeToSubTokens(word As String, ByRef Vocabulary As List(Of String)) As List(Of String)
                Dim tokens As New List(Of String)

                While word.Length > 0
                    Dim prefixFound = False
                    For i = word.Length To 1 Step -1
                        Dim subword = word.Substring(0, i)
                        If Vocabulary.Contains(subword) Then
                            tokens.Add(subword)
                            word = word.Substring(i)
                            prefixFound = True
                            Exit For
                        End If
                    Next

                    If Not prefixFound Then
                        tokens.Add("[UNK]")
                        word = String.Empty
                    End If
                End While

                Return tokens
            End Function

            Public Shared Function ReplaceMergedPair(tokens As List(Of String), newUnit As String) As List(Of String)
                Dim mergedTokens As List(Of String) = New List(Of String)

                For Each token As String In tokens
                    Dim replacedToken As String = token.Replace(newUnit, " " & newUnit & " ")
                    mergedTokens.AddRange(replacedToken.Split(" ").ToList())
                Next

                Return mergedTokens
            End Function

            Public Shared Function ComputePairFrequencies(ByRef VocabularyWithFrequency As Dictionary(Of String, Integer)) As Dictionary(Of String, Integer)
                Dim pairFrequencies As Dictionary(Of String, Integer) = New Dictionary(Of String, Integer)

                For Each token As String In VocabularyWithFrequency.Keys
                    Dim tokenChars As List(Of Char) = token.ToList()

                    For i As Integer = 0 To tokenChars.Count - 2
                        Dim pair As String = tokenChars(i) & tokenChars(i + 1)

                        If Not pairFrequencies.ContainsKey(pair) Then
                            pairFrequencies.Add(pair, VocabularyWithFrequency(token))
                        Else
                            pairFrequencies(pair) += VocabularyWithFrequency(token)
                        End If
                    Next
                Next

                Return pairFrequencies
            End Function

            Public Shared Function SplitIntoSubwords(token As String, ByRef ngramLength As Integer) As List(Of String)
                Dim subwordUnits As List(Of String) = New List(Of String)

                For i As Integer = 0 To token.Length - ngramLength
                    Dim subword As String = token.Substring(i, ngramLength)
                    subwordUnits.Add(subword)
                Next

                Return subwordUnits
            End Function

            Public Shared Function GenerateNGramList(n As Integer) As List(Of String)
                Dim ngramList As New List(Of String)
                Dim letters As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

                GenerateNGrams("", n, letters, ngramList)

                Return ngramList
            End Function

            Public Shared Sub GenerateNGrams(prefix As String, n As Integer, letters As String, ngramList As List(Of String))
                If n = 0 Then
                    ngramList.Add(prefix)
                    Return
                End If

                For Each letter As Char In letters
                    Dim newPrefix As String = prefix & letter
                    GenerateNGrams(newPrefix, n - 1, letters, ngramList)
                Next
            End Sub

            Public Shared Function GenerateWordPairs(ByVal words As String()) As IEnumerable(Of Tuple(Of String, String))
                Dim wordPairs As New List(Of Tuple(Of String, String))

                For i As Integer = 0 To words.Length - 2
                    For j As Integer = i + 1 To words.Length - 1
                        wordPairs.Add(Tuple.Create(words(i), words(j)))
                    Next
                Next

                Return wordPairs
            End Function

            ''' <summary>
            ''' Calculates the PMI scores for each pair of words in a given corpus.
            ''' </summary>
            ''' <param name="corpus">The corpus containing a list of documents.</param>
            ''' <returns>A dictionary containing the PMI scores for each pair of words.</returns>
            Public Shared Function CalculatePMI(corpus As List(Of List(Of String))) As Dictionary(Of String, Dictionary(Of String, Double))
                Dim wordCount As New Dictionary(Of String, Integer)()
                Dim coOccurrenceCount As New Dictionary(Of String, Dictionary(Of String, Integer))()
                Dim totalWordCount As Integer = 0

                ' Count word occurrences and co-occurrences
                For Each document As List(Of String) In corpus
                    For Each word As String In document
                        ' Increment word count
                        If wordCount.ContainsKey(word) Then
                            wordCount(word) += 1
                        Else
                            wordCount.Add(word, 1)
                        End If

                        totalWordCount += 1

                        ' Increment co-occurrence count
                        If Not coOccurrenceCount.ContainsKey(word) Then
                            coOccurrenceCount.Add(word, New Dictionary(Of String, Integer)())
                        End If

                        For Each otherWord As String In document
                            If otherWord <> word Then
                                If coOccurrenceCount(word).ContainsKey(otherWord) Then
                                    coOccurrenceCount(word)(otherWord) += 1
                                Else
                                    coOccurrenceCount(word).Add(otherWord, 1)
                                End If
                            End If
                        Next
                    Next
                Next

                ' Calculate PMI scores
                Dim pmiScores As New Dictionary(Of String, Dictionary(Of String, Double))()

                For Each word As String In wordCount.Keys
                    pmiScores.Add(word, New Dictionary(Of String, Double)())

                    For Each otherWord As String In coOccurrenceCount(word).Keys
                        Dim coOccurrence = coOccurrenceCount(word)(otherWord)
                        Dim pWord = wordCount(word) / totalWordCount
                        Dim pOtherWord = wordCount(otherWord) / totalWordCount
                        Dim pCoOccurrence = coOccurrence / totalWordCount
                        Dim pmi = Math.Log(pCoOccurrence / (pWord * pOtherWord), 2)

                        pmiScores(word).Add(otherWord, pmi)
                    Next
                Next

                Return pmiScores
            End Function

            Public Shared Function TrainWordGrams(trainingData As List(Of String),
                                           ByRef wordgramCounts As Dictionary(Of List(Of String), Integer)) As Dictionary(Of List(Of String), Double)
                On Error Resume Next
                ' Preprocess training data and tokenize into wordgrams
                Dim wordgrams As New List(Of List(Of String))

                For Each sentence As String In trainingData
                    Dim tokens As List(Of String) = Sub_Word_Tokenizer.Tokenize(sentence, Sub_Word_Tokenizer.Type._Word)
                    For i As Integer = 0 To tokens.Count - 1
                        Dim wordgram As List(Of String) = tokens.Skip(i).Take(1)
                        wordgrams.Add(wordgram)
                    Next
                Next

                ' Count wordgrams
                For Each wordgram As List(Of String) In wordgrams
                    If wordgramCounts.ContainsKey(wordgram) Then
                        wordgramCounts(wordgram) += 1
                    Else
                        wordgramCounts.Add(wordgram, 1)
                    End If
                Next
                Dim wordgramProbabilities As New Dictionary(Of List(Of String), Double)

                ' Calculate wordgram probabilities
                Dim totalCount As Integer = wordgramCounts.Values.Sum()
                For Each wordgram As List(Of String) In wordgramCounts.Keys
                    Dim count As Integer = wordgramCounts(wordgram)
                    Dim probability As Double = count / totalCount
                    wordgramProbabilities.Add(wordgram, probability)
                Next
                Return wordgramProbabilities
            End Function

            Public Shared Function GenerateNextWord(wordgram As List(Of String), ByRef Vocabulary As Dictionary(Of List(Of String), Integer), ByRef wordgramProbabilities As Dictionary(Of List(Of String), Double), ngramSize As Integer) As String
                Dim random As New Random()
                Dim candidates As New List(Of String)
                Dim probabilities As New List(Of Double)

                ' Collect candidate words and their probabilities
                For Each candidateWordgram As List(Of String) In Vocabulary.Keys
                    If candidateWordgram.GetRange(0, ngramSize - 1).SequenceEqual(wordgram) Then
                        Dim candidateWord As String = candidateWordgram.Last()
                        Dim probability As Double = wordgramProbabilities(candidateWordgram)
                        candidates.Add(candidateWord)
                        probabilities.Add(probability)
                    End If
                Next

                ' Randomly select a candidate word based on probabilities
                Dim totalProbability As Double = probabilities.Sum()
                Dim randomValue As Double = random.NextDouble() * totalProbability
                Dim cumulativeProbability As Double = 0

                For i As Integer = 0 To candidates.Count - 1
                    cumulativeProbability += probabilities(i)
                    If randomValue <= cumulativeProbability Then
                        Return candidates(i)
                    End If
                Next

                Return ""
            End Function

            Public Shared Function GenerateSentence(ByRef Vocabulary As Dictionary(Of List(Of String), Integer), ByRef wordgramProbabilities As Dictionary(Of List(Of String), Double)) As String
                On Error Resume Next
                Dim sentence As New List(Of String)
                Dim random As New Random()

                ' Start the sentence with a random wordgram
                Dim randomIndex As Integer = random.Next(0, Vocabulary.Count)
                Dim currentWordgram As List(Of String) = Vocabulary.Keys(randomIndex)
                sentence.AddRange(currentWordgram)

                ' Generate subsequent words based on wordgram probabilities
                While Vocabulary.ContainsKey(currentWordgram)
                    Dim nextWord As String = GenerateNextWord(currentWordgram, Vocabulary, wordgramProbabilities, 2)
                    If nextWord = "" Then
                        Exit While
                    End If
                    sentence.Add(nextWord)

                    ' Backoff to lower-order wordgrams if necessary
                    If currentWordgram.Count > 1 Then
                        currentWordgram.RemoveAt(0)
                    Else
                        Exit While
                    End If
                    currentWordgram.Add(nextWord)
                End While

                Return String.Join(" ", sentence)
            End Function

        End Class

    End Class
    Module Ext

        Public Function ReadTextFilesFromDirectory(directoryPath As String) As List(Of String)
            Dim fileList As New List(Of String)()

            Try
                Dim txtFiles As String() = Directory.GetFiles(directoryPath, "*.txt")

                For Each filePath As String In txtFiles
                    Dim fileContent As String = File.ReadAllText(filePath)
                    fileList.Add(fileContent)
                Next
            Catch ex As Exception
                ' Handle any exceptions that may occur while reading the files.
                Console.WriteLine("Error: " & ex.Message)
            End Try

            Return fileList
        End Function

        <Runtime.CompilerServices.Extension()>
        Public Function SpaceItems(ByRef txt As String, Item As String) As String
            Return txt.Replace(Item, " " & Item & " ")
        End Function

        Public Class PunctuationMarkers
            Public Shared ReadOnly SeperatorPunctuation() As String = {" ", ",", "|"}
            Public Shared ReadOnly Symbols() As String = {"@", "#", "$", "%", "&", "*", "+", "=", "^", "_", "~", "§", "°", "¿", "¡"}
            Public Shared ReadOnly EncapuslationPunctuationEnd() As String = {"}", "]", ">", ")"}
            Public Shared ReadOnly EncapuslationPunctuationStart() As String = {"{", "[", "<", "("}
            Public Shared ReadOnly GramaticalPunctuation() As String = {".", "?", "!", ":", ";", ","}
            Public Shared ReadOnly MathPunctuation = New String() {"+", "-", "*", "/", "=", "<", ">", "≤", "≥", "±", "≈", "≠", "%", "‰", "‱", "^", "_", "√", "∛", "∜", "∫", "∬", "∭", "∮", "∯", "∰", "∇", "∂", "∆", "∏", "∑", "∐", "⨀", "⨁", "⨂", "⨃", "⨄", "∫", "∬", "∭", "∮", "∯", "∰", "∇", "∂", "∆", "∏", "∑", "∐", "⨀", "⨁", "⨂", "⨃", "⨄"}
            Public Shared ReadOnly MoneyPunctuation() As String = {"$", "€", "£", "¥", "₹", "₽", "₿"}
            Public Shared ReadOnly CodePunctuation() As String = {"\", "#", "@", "^"}

            Public Shared ReadOnly Delimiters() As Char = {CType(" ", Char), CType(".", Char),
                    CType(",", Char), CType("?", Char),
                    CType("!", Char), CType(";", Char),
                    CType(":", Char), Chr(10), Chr(13), vbTab}

            Public ReadOnly Property SentenceEndPunctuation As List(Of String)
                Get
                    Dim markers() As String = {".", ";", ":", "!", "?"}
                    Return markers.ToList
                End Get
            End Property

            Public Shared ReadOnly Property Punctuation As List(Of String)
                Get
                    Dim x As New List(Of String)
                    x.AddRange(SeperatorPunctuation)
                    x.AddRange(Symbols)
                    x.AddRange(EncapuslationPunctuationStart)
                    x.AddRange(EncapuslationPunctuationEnd)
                    x.AddRange(MoneyPunctuation)
                    x.AddRange(MathPunctuation)
                    x.AddRange(GramaticalPunctuation)
                    x.AddRange(CodePunctuation)
                    x.AddRange(Delimiters)
                    Return x.Distinct.ToList
                End Get
            End Property

        End Class

        <Runtime.CompilerServices.Extension()>
        Public Function SpacePunctuation(ByRef Txt As String) As String
            For Each item In PunctuationMarkers.Punctuation
                Txt = SpaceItems(Txt, item)
            Next

            Return Txt
        End Function

        <Runtime.CompilerServices.Extension()>
        Public Function ExtractStringBetween(ByVal value As String, ByVal strStart As String, ByVal strEnd As String) As String
            If Not String.IsNullOrEmpty(value) Then
                Dim i As Integer = value.IndexOf(strStart)
                Dim j As Integer = value.IndexOf(strEnd)
                Return value.Substring(i, j - i)
            Else
                Return value
            End If
        End Function

    End Module
End Namespace