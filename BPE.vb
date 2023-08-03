Namespace Tokenizers
    Public Class TokenizerWordPiece
        Private ReadOnly corpus As List(Of String)
        Private vocabulary As Dictionary(Of String, Integer)
        Private maxVocabSize As Integer
        Private ReadOnly maxSubwordLength As Integer



        Public Sub New()
        End Sub
        Public Sub New(corpus As List(Of String))
            Me.corpus = corpus
            Me.vocabulary = New Dictionary(Of String, Integer)
            Me.maxVocabSize = 1000000
            Me.maxSubwordLength = 20
        End Sub
        Public Sub New(corpus As List(Of String), vocabulary As Dictionary(Of String, Integer), maxVocabSize As Integer, maxSubwordLength As Integer)
            If corpus Is Nothing Then
                Throw New ArgumentNullException(NameOf(corpus))
            End If

            If vocabulary Is Nothing Then
                Throw New ArgumentNullException(NameOf(vocabulary))
            End If

            Me.corpus = corpus
            Me.vocabulary = vocabulary
            Me.maxVocabSize = maxVocabSize
            Me.maxSubwordLength = maxSubwordLength
        End Sub
        Public Sub Train()
            Dim subwordCounts As New Dictionary(Of String, Integer)

            ' Count subword occurrences in the corpus
            For Each sentence As String In corpus
                Dim tokens As List(Of String) = Tokenize(sentence)

                For Each token As String In tokens
                    If subwordCounts.ContainsKey(token) Then
                        subwordCounts(token) += 1
                    Else
                        subwordCounts.Add(token, 1)
                    End If
                Next
            Next

            ' Sort subwords by frequency and add them to the vocabulary
            Dim sortedSubwords = subwordCounts.OrderByDescending(Function(pair) pair.Value)

            For Each pair In sortedSubwords.Take(maxVocabSize)
                vocabulary.Add(pair.Key, vocabulary.Count)
            Next
        End Sub


        Public Function GetVocabulary() As Dictionary(Of String, Integer)
            Return vocabulary
        End Function
        Public Function Tokenize(text As String) As List(Of String)
            Dim tokens As New List(Of String)
            Dim index As Integer = 0

            While index < text.Length
                Dim subwordLength As Integer = Math.Min(maxSubwordLength, text.Length - index)
                Dim subword As String = text.Substring(index, subwordLength)

                While subwordLength > 0 AndAlso Not vocabulary.ContainsKey(subword)
                    subwordLength -= 1
                    subword = text.Substring(index, subwordLength)
                End While

                tokens.Add(subword)
                index += subwordLength
            End While

            Return tokens
        End Function
        Public Shared Function CalculateWordPieceFrequency(ByVal subword As String, ByVal mergedWord As String) As Integer
            Dim occurrences As Integer = 0
            Dim index As Integer = -1

            While True
                index = mergedWord.IndexOf(subword, index + 1)
                If index = -1 Then
                    Exit While
                End If

                ' Check if the found index is part of a valid word (not a subword of another word)
                If index = 0 OrElse mergedWord(index - 1) = " "c Then
                    Dim endIndex As Integer = index + subword.Length
                    If endIndex = mergedWord.Length OrElse mergedWord(endIndex) = " "c Then
                        occurrences += 1
                    End If
                End If
            End While

            Return occurrences
        End Function


    End Class
    Public Class TokenizerBPE
        Public Class BpeSubwordPair
            Public Property Subword1 As String
            Public Property Subword2 As String
            Public Property Frequency As Integer

            Public Sub New(subword1 As String, subword2 As String, frequency As Integer)
                Me.Subword1 = subword1
                Me.Subword2 = subword2
                Me.Frequency = frequency
            End Sub
        End Class
        Public Class BpeVocabulary
            Inherits Dictionary(Of String, Integer)
        End Class
        Private Sub New()
            ' Private constructor to prevent instantiation without parameters
        End Sub



        Public Shared Function TrainBpeModel(corpus As List(Of String), numMerges As Integer) As BpeVocabulary
            ' Tokenize the corpus at the character level to get the initial vocabulary
            Dim characterLevelVocabulary As BpeVocabulary = TokenizeCorpusToCharacterLevel(corpus)

            ' Merge the most frequent pairs of subwords iteratively
            For i As Integer = 0 To numMerges - 1
                Dim mostFrequentPair As BpeSubwordPair = FindMostFrequentPair(characterLevelVocabulary)
                If mostFrequentPair Is Nothing Then
                    Exit For
                End If

                Dim newSubword As String = mostFrequentPair.Subword1 + mostFrequentPair.Subword2
                characterLevelVocabulary = MergeSubwordPair(characterLevelVocabulary, mostFrequentPair, newSubword)
            Next

            Return characterLevelVocabulary
        End Function

        Private Shared Function TokenizeCorpusToCharacterLevel(corpus As List(Of String)) As BpeVocabulary
            Dim characterLevelVocabulary As New BpeVocabulary()

            For Each document As String In corpus
                For Each character As Char In document
                    Dim subword As String = character.ToString()

                    If characterLevelVocabulary.ContainsKey(subword) Then
                        characterLevelVocabulary(subword) += 1
                    Else
                        characterLevelVocabulary.Add(subword, 1)
                    End If
                Next
            Next

            Return characterLevelVocabulary
        End Function

        Private Shared Function FindMostFrequentPair(vocabulary As BpeVocabulary) As BpeSubwordPair
            Dim mostFrequentPair As BpeSubwordPair = Nothing
            Dim maxFrequency As Integer = 0

            For Each subword1 As String In vocabulary.Keys
                For Each subword2 As String In vocabulary.Keys
                    If subword1 <> subword2 Then
                        Dim pairFrequency As Integer = CalculatePairFrequency(vocabulary, subword1, subword2)
                        If pairFrequency > maxFrequency Then
                            maxFrequency = pairFrequency
                            mostFrequentPair = New BpeSubwordPair(subword1, subword2, pairFrequency)
                        End If
                    End If
                Next
            Next

            Return mostFrequentPair
        End Function

        Private Shared Function CalculatePairFrequency(vocabulary As BpeVocabulary, subword1 As String, subword2 As String) As Integer
            Dim pairFrequency As Integer = 0

            For Each word As String In vocabulary.Keys
                Dim mergedWord As String = word.Replace(subword1 + subword2, subword1 + subword2.ToLower())
                Dim occurrences As Integer = 0
                Dim index As Integer = -1

                While True
                    index = mergedWord.IndexOf(subword1 + subword2.ToLower(), index + 1)
                    If index = -1 Then
                        Exit While
                    End If
                    occurrences += 1
                End While


                pairFrequency += occurrences * vocabulary(word)
            Next

            Return pairFrequency
        End Function

        Private Shared Function MergeSubwordPair(vocabulary As BpeVocabulary, pairToMerge As BpeSubwordPair, newSubword As String) As BpeVocabulary
            Dim newVocabulary As New BpeVocabulary()

            For Each subword As String In vocabulary.Keys
                Dim mergedSubword As String = subword.Replace(pairToMerge.Subword1 + pairToMerge.Subword2, newSubword)
                newVocabulary(mergedSubword) = vocabulary(subword)
            Next

            Return newVocabulary
        End Function
    End Class
    Public Class TokenizerBitWord
        Public Property Vocabulary As Dictionary(Of String, Integer)
        Public Sub New()
            Vocabulary = New Dictionary(Of String, Integer)
        End Sub
        Public Function Tokenize(ByRef Corpus As List(Of String)) As List(Of String)
            Dim tokens As New List(Of String)
            Dim Subword As String = ""

            Dim UnknownDocs As New List(Of String)
            'SubDoc Vocabulary Tokenizer
            For Each doc In Corpus
                For i = 0 To doc.Count - 1
                    Subword &= doc(i)
                    If Vocabulary.ContainsKey(Subword.ToLower()) Then
                        tokens.Add(Subword)
                        Subword = ""
                    End If

                Next
                'Save unknowns
                If Subword <> "" Then
                    UnknownDocs.Add(Subword)
                End If
            Next
            'Unknown paragraphs
            Dim UnknownParagraphs As New List(Of String)
            If UnknownDocs.Count > 0 Then
                For Each doc In UnknownDocs
                    Dim Para As List(Of String) = BasicTokenizer.TokenizeToParagraph(doc)
                    For Each item In Para
                        Subword = ""

                        Subword += item
                        If Vocabulary.ContainsKey(Subword.ToLower) Then
                            ' If the subword is in the Vocabulary, add it to the list of subwords
                            tokens.Add(Subword.ToLower)
                            ' Reset the subword for the next iteration
                            Subword = ""
                        End If
                        'Save unknowns
                        If Subword <> "" Then
                            UnknownParagraphs.Add(Subword)
                        End If
                    Next

                Next
            End If
            'Unknown Sentences
            Dim UnknownSents As New List(Of String)
            If UnknownParagraphs.Count > 0 Then
                For Each sent In UnknownParagraphs
                    Dim Sents As List(Of String) = BasicTokenizer.TokenizeToSentence(sent)


                    For Each item In Sents
                        Subword = ""

                        Subword += item
                        If Vocabulary.ContainsKey(Subword.ToLower) Then
                            ' If the subword is in the Vocabulary, add it to the list of subwords
                            tokens.Add(Subword.ToLower)
                            ' Reset the subword for the next iteration
                            Subword = ""
                        End If
                        'Save unknowns
                        If Subword <> "" Then
                            UnknownSents.Add(Subword)
                        End If
                    Next
                Next
            End If
            'Unknown Words
            Dim UnknownWords As New List(Of String)
            If UnknownSents.Count > 0 Then
                For Each Word In UnknownSents
                    Dim Words As List(Of String) = BasicTokenizer.TokenizeToWord(Word)
                    For Each item In Words
                        Subword = ""

                        Subword += item
                        If Vocabulary.ContainsKey(Subword.ToLower) Then
                            ' If the subword is in the Vocabulary, add it to the list of subwords
                            tokens.Add(Subword.ToLower)
                            ' Reset the subword for the next iteration
                            Subword = ""
                        End If
                        'Save unknowns
                        If Subword <> "" Then
                            UnknownWords.Add(Subword)
                        End If
                    Next

                Next

            End If
            'Unknown Words
            Dim UnknownChars As New List(Of String)
            If UnknownWords.Count > 0 Then
                For Each iChar In UnknownWords
                    Dim Chars As List(Of String) = BasicTokenizer.TokenizeToCharacter(iChar)
                    For Each item In Chars
                        Subword = ""

                        Subword += item
                        If Vocabulary.ContainsKey(Subword.ToLower) Then
                            ' If the subword is in the Vocabulary, add it to the list of subwords
                            tokens.Add(Subword.ToLower)
                            ' Reset the subword for the next iteration
                            Subword = ""
                        End If
                        'Save unknowns
                        If Subword <> "" Then
                            UnknownChars.Add(Subword)
                        End If
                    Next

                Next

            End If

            For Each unkChar In UnknownChars
                Vocabulary.Add(unkChar, 1)
            Next

            Console.WriteLine("Recognized Tokens")
            For Each tok In tokens
                Console.WriteLine("Token =" & tok)
            Next

            Console.WriteLine("UnRecognized Tokens")
            For Each tok In UnknownChars
                Console.WriteLine("Token =" & tok)
            Next
            Return tokens
        End Function

        Public Sub Train(corpus As List(Of String), MaxMergeOperations As Integer)
            ' Initialize the vocabulary with word-level subword units
            Tokenize(corpus)
            Dim mergeOperationsCount As Integer = 0

            While mergeOperationsCount < MaxMergeOperations
                ' Compute the frequency of subword units in the vocabulary
                Dim subwordFrequencies As New Dictionary(Of String, Integer)

                For Each subword In Vocabulary.Keys
                    Dim subwordUnits = BasicTokenizer.TokenizeToCharacter(subword)
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
                Dim newSubwordUnit = mostFrequentPair.Key

                ' Update the vocabulary by replacing occurrences of the merged subword pair with the new subword unit
                Dim updatedVocabulary As New Dictionary(Of String, Integer)

                For Each subword In Vocabulary.Keys
                    Dim mergedSubword = subword.Replace(mostFrequentPair.Key, newSubwordUnit)
                    updatedVocabulary(mergedSubword) = Vocabulary(subword)
                Next

                Vocabulary = updatedVocabulary
                mergeOperationsCount += 1

            End While

        End Sub

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

    End Class
    Public Class TokenizerPositional
        Private iStopWords As List(Of String)

        Private Function RemoveStopWords(ByVal tokens As List(Of Token)) As List(Of Token)
            Return tokens.Where(Function(token) Not StopWords.Contains(token.Value)).ToList()
        End Function
        Public Property StopWordRemovalEnabled As Boolean

        Public Property StopWords As List(Of String)
            Get
                Return iStopWords
            End Get
            Set(value As List(Of String))
                iStopWords = value
            End Set
        End Property
        Public Structure Token
            ''' <summary>
            ''' Initializes a new instance of the Token structure.
            ''' </summary>
            ''' <param name="type">The type of the token.</param>
            ''' <param name="value">The string value of the token.</param>
            Public Sub New(ByVal type As String, ByVal value As String)
                Me.Type = type
                Me.Value = value
            End Sub

            Public Sub New(ByVal type As TokenType, ByVal value As String, ByVal startPosition As Integer, ByVal endPosition As Integer)
                Me.Type = type
                Me.Value = value
                Me.StartPosition = startPosition
                Me.EndPosition = endPosition
            End Sub

            Public Property EndPosition As Integer
            Public Property StartPosition As Integer
            Public Property Type As TokenType
            Public Property Value As String
        End Structure

        ''' <summary>
        ''' Returns Tokens With Positions
        ''' </summary>
        ''' <param name="input"></param>
        ''' <returns></returns>
        Public Shared Function TokenizeByCharacter(ByVal input As String) As List(Of Token)
            Dim characters As Char() = input.ToCharArray()
            Dim tokens As New List(Of Token)
            Dim currentPosition As Integer = 0

            For Each character As Char In characters
                Dim startPosition As Integer = currentPosition
                Dim endPosition As Integer = currentPosition
                Dim token As New Token(TokenType.Character, character.ToString(), startPosition, endPosition)
                tokens.Add(token)
                currentPosition += 1
            Next

            Return tokens
        End Function

        ''' <summary>
        ''' Returns Tokens With Positions
        ''' </summary>
        ''' <param name="input"></param>
        ''' <returns></returns>
        Public Shared Function TokenizeBySentence(ByVal input As String) As List(Of Token)
            Dim sentences As String() = input.Split("."c)
            Dim tokens As New List(Of Token)
            Dim currentPosition As Integer = 0

            For Each sentence As String In sentences
                Dim startPosition As Integer = currentPosition
                Dim endPosition As Integer = currentPosition + sentence.Length - 1
                Dim token As New Token(TokenType.Sentence, sentence, startPosition, endPosition)
                tokens.Add(token)
                currentPosition = endPosition + 2 ' Account for the period and the space after the sentence
            Next

            Return tokens
        End Function

        ''' <summary>
        ''' Returns Tokens With Positions
        ''' </summary>
        ''' <param name="input"></param>
        ''' <returns></returns>
        Public Shared Function TokenizeByWord(ByVal input As String) As List(Of Token)
            Dim words As String() = input.Split(" "c)
            Dim tokens As New List(Of Token)
            Dim currentPosition As Integer = 0

            For Each word As String In words
                Dim startPosition As Integer = currentPosition
                Dim endPosition As Integer = currentPosition + word.Length - 1
                Dim token As New Token(TokenType.Word, word, startPosition, endPosition)
                tokens.Add(token)
                currentPosition = endPosition + 2 ' Account for the space between words
            Next

            Return tokens
        End Function

        ''' <summary>
        ''' Pure basic Tokenizer to Tokens
        ''' </summary>
        ''' <param name="Corpus"></param>
        ''' <param name="tokenizationOption">Type Of Tokenization</param>
        ''' <returns></returns>
        Public Shared Function TokenizeInput(ByRef Corpus As List(Of String), tokenizationOption As TokenizerType) As List(Of Token)
            Dim ivocabulary As New List(Of Token)

            For Each Doc In Corpus
                Select Case tokenizationOption
                    Case TokenizerType._Char
                        ivocabulary.AddRange(TokenizeByCharacter(Doc.ToLower))
                    Case TokenizerType._Word
                        ivocabulary.AddRange(TokenizeByWord(Doc.ToLower))
                    Case TokenizerType._Sentence
                        ivocabulary.AddRange(TokenizeBySentence(Doc.ToLower))


                End Select
            Next

            Return ivocabulary
        End Function

    End Class
    Public Class TokenizerTokenID
        Public TokenToId As New Dictionary(Of String, Integer)
        Private idToToken As New Dictionary(Of Integer, String)
        Private nextId As Integer = 0

        Private vocab As New Dictionary(Of String, Integer)
        Public Sub New(ByRef Vocabulary As Dictionary(Of String, Integer))
            vocab = Vocabulary
            TokenToId = New Dictionary(Of String, Integer)
            idToToken = New Dictionary(Of Integer, String)
        End Sub

        ''' <summary>
        ''' Pure Tokenizer (will tokenize based on the Tokenizer model settings)
        ''' </summary>
        ''' <param name="text"></param>
        ''' <returns></returns>
        Public Function TokenizeToTokenIDs(text As String) As List(Of Integer)
            Dim tokens = TokenizerPositional.TokenizeByWord(text)
            Dim tokenIds As New List(Of Integer)

            For Each itoken In tokens
                Dim tokenId As Integer
                If TokenToId.ContainsKey(itoken.Value) Then
                    tokenId = TokenToId(itoken.Value)
                Else
                    'Not registered

                    tokenId = TokenToId(itoken.Value)

                End If
                tokenIds.Add(tokenId)

            Next

            Return tokenIds
        End Function

        Private Sub AddTokenID(text As String)

            If Not vocab.ContainsKey(text) Then
                vocab(text) = nextId
                nextId += 1
                TokenToId = vocab.ToDictionary(Function(x) x.Key, Function(x) x.Value)
                idToToken = TokenToId.ToDictionary(Function(x) x.Value, Function(x) x.Key)
            End If


        End Sub

        ''' <summary>
        ''' Given  a Set of Token ID Decode the Tokens 
        ''' </summary>
        ''' <param name="tokenIds"></param>
        ''' <returns></returns>
        Public Function Detokenize(tokenIds As List(Of Integer)) As String
            Dim tokens As New List(Of String)

            For Each tokenId As Integer In tokenIds
                tokens.Add(idToToken(tokenId))
            Next

            Return String.Join(" ", tokens)
        End Function
    End Class
End Namespace

