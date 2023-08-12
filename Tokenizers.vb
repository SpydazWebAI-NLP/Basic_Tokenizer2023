Imports System.IO

Namespace Models
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
Public Module Extensions
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
                Return x.Distinct.ToList
            End Get
        End Property

    End Class
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
    Public Enum TokenizerType
        _Char
        _Word
        _Sentence
        _Paragraph
        _BPE
        _Wordpiece
        _Token
        _TokenID
    End Enum
    Public Class RemoveToken



        Private Shared ReadOnly AlphaBet() As String = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}
        Private Shared ReadOnly Number() As String = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
"30", "40", "50", "60", "70", "80", "90", "00", "000", "0000", "00000", "000000", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
"nineteen", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "hundred", "thousand", "million", "Billion"}
        Private iStopWords As New List(Of String)

        Private Shared Function AddSuffix(ByRef Str As String, ByVal Suffix As String) As String
            Return Str & Suffix
        End Function
        Public Function GetValidTokens(ByRef InputStr As String) As String
            Dim EndStr As Integer = InputStr.Length
            Dim CharStr As String = ""
            For i = 0 To EndStr - 1
                If GetTokenType(InputStr(i)) <> TokenType.Ignore Then
                    CharStr = AddSuffix(CharStr, InputStr(i))
                Else

                End If
            Next
            Return CharStr
        End Function
        Public Function GetTokenType(ByRef CharStr As String) As TokenType
            For Each item In PunctuationMarkers.SeperatorPunctuation
                If CharStr = item Then Return TokenType.SeperatorPunctuation
            Next
            For Each item In PunctuationMarkers.GramaticalPunctuation
                If CharStr = item Then Return TokenType.GramaticalPunctuation
            Next
            For Each item In PunctuationMarkers.EncapuslationPunctuationStart
                If CharStr = item Then Return TokenType.EncapuslationPunctuationStart
            Next
            For Each item In PunctuationMarkers.EncapuslationPunctuationEnd
                If CharStr = item Then Return TokenType.EncapuslationPunctuationEnd
            Next
            For Each item In PunctuationMarkers.MoneyPunctuation
                If CharStr = item Then Return TokenType.MoneyPunctuation
            Next
            For Each item In PunctuationMarkers.MathPunctuation
                If CharStr = item Then Return TokenType.MathPunctuation
            Next
            For Each item In PunctuationMarkers.CodePunctuation
                If CharStr = item Then Return TokenType.CodePunctuation
            Next
            For Each item In AlphaBet
                If CharStr = item Then Return TokenType.AlphaBet
            Next
            For Each item In Number
                If CharStr = item Then Return TokenType.Number
            Next
            Return TokenType.Ignore
        End Function

        Public Function GetEncapsulated(ByRef Userinput As String) As List(Of String)
            GetEncapsulated = New List(Of String)
            Do Until ContainsEncapsulated(Userinput) = False
                GetEncapsulated.Add(ExtractEncapsulated(Userinput))
            Loop
        End Function
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
    Public Class FrequentTerms

        Public Shared Function FindFrequentBigrams(words As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim bigramCounts As New Dictionary(Of String, Integer)

            For i As Integer = 0 To words.Count - 2
                Dim bigram As String = words(i) & " " & words(i + 1)

                If bigramCounts.ContainsKey(bigram) Then
                    bigramCounts(bigram) += 1
                Else
                    bigramCounts.Add(bigram, 1)
                End If
            Next

            Dim frequentBigrams As New List(Of String)

            For Each pair In bigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentBigrams.Add(pair.Key)
                End If
            Next

            Return frequentBigrams
        End Function

        Public Shared Function FindFrequentCharacterBigrams(words As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim bigramCounts As New Dictionary(Of String, Integer)

            For Each word In words
                Dim characters As Char() = word.ToCharArray()

                For i As Integer = 0 To characters.Length - 2
                    Dim bigram As String = characters(i) & characters(i + 1)

                    If bigramCounts.ContainsKey(bigram) Then
                        bigramCounts(bigram) += 1
                    Else
                        bigramCounts.Add(bigram, 1)
                    End If
                Next
            Next

            Dim frequentCharacterBigrams As New List(Of String)

            For Each pair In bigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentCharacterBigrams.Add(pair.Key)
                End If
            Next

            Return frequentCharacterBigrams
        End Function

        Public Shared Function FindFrequentCharacterTrigrams(words As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim trigramCounts As New Dictionary(Of String, Integer)

            For Each word In words
                Dim characters As Char() = word.ToCharArray()

                For i As Integer = 0 To characters.Length - 3
                    Dim trigram As String = characters(i) & characters(i + 1) & characters(i + 2)

                    If trigramCounts.ContainsKey(trigram) Then
                        trigramCounts(trigram) += 1
                    Else
                        trigramCounts.Add(trigram, 1)
                    End If
                Next
            Next

            Dim frequentCharacterTrigrams As New List(Of String)

            For Each pair In trigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentCharacterTrigrams.Add(pair.Key)
                End If
            Next

            Return frequentCharacterTrigrams
        End Function

        Public Shared Function FindFrequentSentenceBigrams(sentences As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim bigramCounts As New Dictionary(Of String, Integer)

            For i As Integer = 0 To sentences.Count - 2
                Dim bigram As String = sentences(i) & " " & sentences(i + 1)

                If bigramCounts.ContainsKey(bigram) Then
                    bigramCounts(bigram) += 1
                Else
                    bigramCounts.Add(bigram, 1)
                End If
            Next

            Dim frequentSentenceBigrams As New List(Of String)

            For Each pair In bigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentSentenceBigrams.Add(pair.Key)
                End If
            Next

            Return frequentSentenceBigrams
        End Function

        Public Shared Function FindFrequentSentenceTrigrams(sentences As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim trigramCounts As New Dictionary(Of String, Integer)

            For i As Integer = 0 To sentences.Count - 3
                Dim trigram As String = sentences(i) & " " & sentences(i + 1) & " " & sentences(i + 2)

                If trigramCounts.ContainsKey(trigram) Then
                    trigramCounts(trigram) += 1
                Else
                    trigramCounts.Add(trigram, 1)
                End If
            Next

            Dim frequentSentenceTrigrams As New List(Of String)

            For Each pair In trigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentSentenceTrigrams.Add(pair.Key)
                End If
            Next

            Return frequentSentenceTrigrams
        End Function

        Public Shared Function FindFrequentTrigrams(words As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim trigramCounts As New Dictionary(Of String, Integer)

            For i As Integer = 0 To words.Count - 3
                Dim trigram As String = words(i) & " " & words(i + 1) & " " & words(i + 2)

                If trigramCounts.ContainsKey(trigram) Then
                    trigramCounts(trigram) += 1
                Else
                    trigramCounts.Add(trigram, 1)
                End If
            Next

            Dim frequentTrigrams As New List(Of String)

            For Each pair In trigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentTrigrams.Add(pair.Key)
                End If
            Next

            Return frequentTrigrams
        End Function

        Public Shared Function FindFrequentWordBigrams(sentences As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim bigramCounts As New Dictionary(Of String, Integer)

            For Each sentence In sentences
                Dim words As String() = sentence.Split(" "c)

                For i As Integer = 0 To words.Length - 2
                    Dim bigram As String = words(i) & " " & words(i + 1)

                    If bigramCounts.ContainsKey(bigram) Then
                        bigramCounts(bigram) += 1
                    Else
                        bigramCounts.Add(bigram, 1)
                    End If
                Next
            Next

            Dim frequentWordBigrams As New List(Of String)

            For Each pair In bigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentWordBigrams.Add(pair.Key)
                End If
            Next

            Return frequentWordBigrams
        End Function

        Public Shared Function FindFrequentWordTrigrams(sentences As List(Of String), Optional Param_Freq As Integer = 1) As List(Of String)
            Dim trigramCounts As New Dictionary(Of String, Integer)

            For Each sentence In sentences
                Dim words As String() = sentence.Split(" "c)

                For i As Integer = 0 To words.Length - 3
                    Dim trigram As String = words(i) & " " & words(i + 1) & " " & words(i + 2)

                    If trigramCounts.ContainsKey(trigram) Then
                        trigramCounts(trigram) += 1
                    Else
                        trigramCounts.Add(trigram, 1)
                    End If
                Next
            Next

            Dim frequentWordTrigrams As New List(Of String)

            For Each pair In trigramCounts
                If pair.Value > Param_Freq Then ' Adjust the threshold as needed
                    frequentWordTrigrams.Add(pair.Key)
                End If
            Next

            Return frequentWordTrigrams
        End Function


        Public Shared Function FindFrequentCharNgrams(Tokens As List(Of String), N As Integer, ByRef Freq_threshold As Integer) As List(Of String)
            Dim NgramCounts As New Dictionary(Of String, Integer)

            For Each word In Tokens
                Dim characters As List(Of String) = Tokenizer.TokenizeToCharacter(word)

                For Each ngram In GetTokenGramCounts(characters, N)
                    'Update Dictionary
                    If NgramCounts.ContainsKey(ngram.Key) Then

                        NgramCounts(ngram.Key) += ngram.Value
                    Else
                        NgramCounts.Add(ngram.Key, ngram.Value)
                    End If

                Next
            Next

            Return Tokenizer.GetHighFreq(NgramCounts, Freq_threshold)
        End Function
        Public Shared Function GetTokenGramCounts(Tokens As List(Of String), N As Integer) As Dictionary(Of String, Integer)
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
        Public Shared Function GetFrequentTokenNgrams(Tokens As List(Of String), N As Integer, ByRef Freq_threshold As Integer) As List(Of String)
            Dim NgramCounts As Dictionary(Of String, Integer) = GetTokenGramCounts(Tokens, N)

            Dim frequentWordNgrams As List(Of String) = Tokenizer.GetHighFreq(NgramCounts, Freq_threshold)

            Return frequentWordNgrams
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
    <Runtime.CompilerServices.Extension()>
    Public Function SpaceItems(ByRef txt As String, Item As String) As String
        Return txt.Replace(Item, " " & Item & " ")
    End Function
    <Runtime.CompilerServices.Extension()>
    Public Function SplitIntoSubwords(token As String, ByRef ngramLength As Integer) As List(Of String)
        Dim subwordUnits As List(Of String) = New List(Of String)

        For i As Integer = 0 To token.Length - ngramLength
            Dim subword As String = token.Substring(i, ngramLength)
            subwordUnits.Add(subword)
        Next

        Return subwordUnits
    End Function
    <Runtime.CompilerServices.Extension()>
    Public Function SpacePunctuation(ByRef Txt As String) As String
        For Each item In PunctuationMarkers.Punctuation
            Txt = SpaceItems(Txt, item)
        Next

        Return Txt
    End Function
    <Runtime.CompilerServices.Extension()>
    Public Function Tokenize(Document As String, ByRef TokenizerType As TokenizerType) As List(Of String)
        Tokenize = New List(Of String)
        Select Case TokenizerType
            Case TokenizerType._Sentence
                Return Tokenizer.TokenizeToSentence(Document)
            Case TokenizerType._Word
                Return Tokenizer.TokenizeToWord(Document)
            Case TokenizerType._Char
                Return Tokenizer.TokenizeToCharacter(Document)

        End Select

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
    <Runtime.CompilerServices.Extension()>
    Public Function ReplaceMergedPair(tokens As List(Of String), newUnit As String) As List(Of String)
        Dim mergedTokens As List(Of String) = New List(Of String)

        For Each token As String In tokens
            Dim replacedToken As String = token.Replace(newUnit, " " & newUnit & " ")
            mergedTokens.AddRange(replacedToken.Split(" ").ToList())
        Next

        Return mergedTokens
    End Function
    Public Class ReservedWords
        Public Shared Function IdentifyReservedWords(ByRef Input As String) As String
            Dim reservedWords As List(Of String) = GetReservedWords()

            For Each word In reservedWords
                Input = Input.Replace(word, UCase(word))
            Next

            Return Input
        End Function
        Private Shared Function GetReservedWords() As List(Of String)
            Dim reservedWords As New List(Of String)()

            ' Add VB.NET reserved words to the list
            reservedWords.Add("AddHandler")
            reservedWords.Add("AddressOf")
            reservedWords.Add("Alias")
            reservedWords.Add("And")
            reservedWords.Add("AndAlso")
            reservedWords.Add("As")
            reservedWords.Add("Boolean")
            reservedWords.Add("ByRef")
            reservedWords.Add("Byte")
            reservedWords.Add("ByVal")
            reservedWords.Add("Call")
            reservedWords.Add("Case")
            reservedWords.Add("Catch")
            reservedWords.Add("CBool")
            reservedWords.Add("CByte")
            reservedWords.Add("CChar")
            reservedWords.Add("CDate")
            reservedWords.Add("CDbl")
            reservedWords.Add("CDec")
            reservedWords.Add("Char")
            reservedWords.Add("CInt")
            reservedWords.Add("Class")
            reservedWords.Add("CLng")
            reservedWords.Add("CObj")
            reservedWords.Add("Continue")
            reservedWords.Add("CSByte")
            reservedWords.Add("CShort")
            reservedWords.Add("CSng")
            reservedWords.Add("CStr")
            reservedWords.Add("CType")
            reservedWords.Add("CUInt")
            reservedWords.Add("CULng")
            reservedWords.Add("CUShort")
            reservedWords.Add("Date")
            reservedWords.Add("Decimal")
            reservedWords.Add("Declare")
            reservedWords.Add("Default")
            reservedWords.Add("Delegate")
            reservedWords.Add("Dim")
            reservedWords.Add("DirectCast")
            reservedWords.Add("Do")
            reservedWords.Add("Double")
            reservedWords.Add("Each")
            reservedWords.Add("Else")
            reservedWords.Add("ElseIf")
            reservedWords.Add("End")
            reservedWords.Add("EndIf")
            reservedWords.Add("Enum")
            reservedWords.Add("Erase")
            reservedWords.Add("Error")
            reservedWords.Add("Event")
            reservedWords.Add("Exit")
            reservedWords.Add("False")
            reservedWords.Add("Finally")
            reservedWords.Add("For")
            reservedWords.Add("Friend")
            reservedWords.Add("Function")
            reservedWords.Add("Get")
            reservedWords.Add("GetType")
            reservedWords.Add("GetXMLNamespace")
            reservedWords.Add("Global")
            reservedWords.Add("GoSub")
            reservedWords.Add("GoTo")
            reservedWords.Add("Handles")
            reservedWords.Add("If")
            reservedWords.Add("Implements")
            reservedWords.Add("Imports")
            reservedWords.Add("In")
            reservedWords.Add("Inherits")
            reservedWords.Add("Integer")
            reservedWords.Add("Interface")
            reservedWords.Add("Is")
            reservedWords.Add("IsNot")
            reservedWords.Add("Let")
            reservedWords.Add("Lib")
            reservedWords.Add("Like")
            reservedWords.Add("Long")
            reservedWords.Add("Loop")
            reservedWords.Add("Me")
            reservedWords.Add("Mod")
            reservedWords.Add("Module")
            reservedWords.Add("MustInherit")
            reservedWords.Add("MustOverride")
            reservedWords.Add("MyBase")
            reservedWords.Add("MyClass")
            reservedWords.Add("Namespace")
            reservedWords.Add("Narrowing")
            reservedWords.Add("New")
            reservedWords.Add("Next")
            reservedWords.Add("Not")
            reservedWords.Add("Nothing")
            reservedWords.Add("NotInheritable")
            reservedWords.Add("NotOverridable")
            reservedWords.Add("Object")
            reservedWords.Add("Of")
            reservedWords.Add("On")
            reservedWords.Add("Operator")
            reservedWords.Add("Option")
            reservedWords.Add("Optional")
            reservedWords.Add("Or")
            reservedWords.Add("OrElse")
            reservedWords.Add("Overloads")
            reservedWords.Add("Overridable")
            reservedWords.Add("Overrides")
            reservedWords.Add("ParamArray")
            reservedWords.Add("Partial")
            reservedWords.Add("Private")
            reservedWords.Add("Property")
            reservedWords.Add("Protected")
            reservedWords.Add("Public")
            reservedWords.Add("RaiseEvent")
            reservedWords.Add("ReadOnly")
            reservedWords.Add("ReDim")
            reservedWords.Add("RemoveHandler")
            reservedWords.Add("Resume")
            reservedWords.Add("Return")
            reservedWords.Add("SByte")
            reservedWords.Add("Select")
            reservedWords.Add("Set")
            reservedWords.Add("Shadows")
            reservedWords.Add("Shared")
            reservedWords.Add("Short")
            reservedWords.Add("Single")
            reservedWords.Add("Static")
            reservedWords.Add("Step")
            reservedWords.Add("Stop")
            reservedWords.Add("String")
            reservedWords.Add("Structure")
            reservedWords.Add("Sub")
            reservedWords.Add("SyncLock")
            reservedWords.Add("Then")
            reservedWords.Add("Throw")
            reservedWords.Add("To")
            reservedWords.Add("True")
            reservedWords.Add("Try")
            reservedWords.Add("TryCast")
            reservedWords.Add("TypeOf")
            reservedWords.Add("UInteger")
            reservedWords.Add("ULong")
            reservedWords.Add("UShort")
            reservedWords.Add("Using")
            reservedWords.Add("Variant")
            reservedWords.Add("Wend")
            reservedWords.Add("When")
            reservedWords.Add("While")
            reservedWords.Add("Widening")
            reservedWords.Add("With")
            reservedWords.Add("WithEvents")
            reservedWords.Add("WriteOnly")
            reservedWords.Add("Xor")

            Return reservedWords
        End Function

        ''' <summary>
        ''' Checks if string is a reserved VBscipt Keyword
        ''' </summary>
        ''' <param name="keyword"></param>
        ''' <returns></returns>
        Public Shared Function IsReservedWord(ByVal keyword As String) As Boolean
            Dim IsReserved = False
            Select Case LCase(keyword)
                Case "and" : IsReserved = True
                Case "as" : IsReserved = True
                Case "boolean" : IsReserved = True
                Case "byref" : IsReserved = True
                Case "byte" : IsReserved = True
                Case "byval" : IsReserved = True
                Case "call" : IsReserved = True
                Case "case" : IsReserved = True
                Case "class" : IsReserved = True
                Case "const" : IsReserved = True
                Case "currency" : IsReserved = True
                Case "debug" : IsReserved = True
                Case "dim" : IsReserved = True
                Case "do" : IsReserved = True
                Case "double" : IsReserved = True
                Case "each" : IsReserved = True
                Case "else" : IsReserved = True
                Case "elseif" : IsReserved = True
                Case "empty" : IsReserved = True
                Case "end" : IsReserved = True
                Case "endif" : IsReserved = True
                Case "enum" : IsReserved = True
                Case "eqv" : IsReserved = True
                Case "event" : IsReserved = True
                Case "exit" : IsReserved = True
                Case "false" : IsReserved = True
                Case "for" : IsReserved = True
                Case "function" : IsReserved = True
                Case "get" : IsReserved = True
                Case "goto" : IsReserved = True
                Case "if" : IsReserved = True
                Case "imp" : IsReserved = True
                Case "implements" : IsReserved = True
                Case "in" : IsReserved = True
                Case "integer" : IsReserved = True
                Case "is" : IsReserved = True
                Case "let" : IsReserved = True
                Case "like" : IsReserved = True
                Case "long" : IsReserved = True
                Case "loop" : IsReserved = True
                Case "lset" : IsReserved = True
                Case "me" : IsReserved = True
                Case "mod" : IsReserved = True
                Case "new" : IsReserved = True
                Case "next" : IsReserved = True
                Case "not" : IsReserved = True
                Case "nothing" : IsReserved = True
                Case "null" : IsReserved = True
                Case "on" : IsReserved = True
                Case "option" : IsReserved = True
                Case "optional" : IsReserved = True
                Case "or" : IsReserved = True
                Case "paramarray" : IsReserved = True
                Case "preserve" : IsReserved = True
                Case "private" : IsReserved = True
                Case "public" : IsReserved = True
                Case "raiseevent" : IsReserved = True
                Case "redim" : IsReserved = True
                Case "rem" : IsReserved = True
                Case "resume" : IsReserved = True
                Case "rset" : IsReserved = True
                Case "select" : IsReserved = True
                Case "set" : IsReserved = True
                Case "shared" : IsReserved = True
                Case "single" : IsReserved = True
                Case "static" : IsReserved = True
                Case "stop" : IsReserved = True
                Case "sub" : IsReserved = True
                Case "then" : IsReserved = True
                Case "to" : IsReserved = True
                Case "true" : IsReserved = True
                Case "type" : IsReserved = True
                Case "typeof" : IsReserved = True
                Case "until" : IsReserved = True
                Case "variant" : IsReserved = True
                Case "wend" : IsReserved = True
                Case "while" : IsReserved = True
                Case "with" : IsReserved = True
                Case "xor" : IsReserved = True
            End Select
            Return IsReserved
        End Function


    End Class
    Public Class Word2WordMatrix
        Private matrix As Dictionary(Of String, Dictionary(Of String, Integer))

        Public Sub New()
            matrix = New Dictionary(Of String, Dictionary(Of String, Integer))
        End Sub
        Public Shared Function CreateDataGridView(matrix As Dictionary(Of String, Dictionary(Of String, Double))) As DataGridView
            Dim dataGridView As New DataGridView()
            dataGridView.Dock = DockStyle.Fill
            dataGridView.AutoGenerateColumns = False
            dataGridView.AllowUserToAddRows = False

            ' Add columns to the DataGridView
            Dim wordColumn As New DataGridViewTextBoxColumn()
            wordColumn.HeaderText = "Word"
            wordColumn.DataPropertyName = "Word"
            dataGridView.Columns.Add(wordColumn)

            For Each contextWord As String In matrix.Keys
                Dim contextColumn As New DataGridViewTextBoxColumn()
                contextColumn.HeaderText = contextWord
                contextColumn.DataPropertyName = contextWord
                dataGridView.Columns.Add(contextColumn)
            Next

            ' Populate the DataGridView with the matrix data
            For Each word As String In matrix.Keys
                Dim rowValues As New List(Of Object)
                rowValues.Add(word)

                For Each contextWord As String In matrix.Keys
                    Dim count As Object = If(matrix(word).ContainsKey(contextWord), matrix(word)(contextWord), 0)
                    rowValues.Add(count)
                Next

                dataGridView.Rows.Add(rowValues.ToArray())
            Next

            Return dataGridView
        End Function
        Public Function iCoOccurrenceMatrix(text As String, entityList As List(Of String), windowSize As Integer) As Dictionary(Of String, Dictionary(Of String, Integer))
            Dim CoOccurrenceMatrix As New Dictionary(Of String, Dictionary(Of String, Integer))

            Dim words() As String = text.Split(" "c)
            For i As Integer = 0 To words.Length - 1
                If entityList.Contains(words(i).ToLower()) Then
                    Dim entity As String = words(i)
                    If Not CoOccurrenceMatrix.ContainsKey(entity) Then
                        CoOccurrenceMatrix(entity) = New Dictionary(Of String, Integer)()
                    End If

                    For j As Integer = i - windowSize To i + windowSize
                        If j >= 0 AndAlso j < words.Length AndAlso i <> j AndAlso entityList.Contains(words(j).ToLower()) Then
                            Dim coOccurringEntity As String = words(j)
                            If Not CoOccurrenceMatrix(entity).ContainsKey(coOccurringEntity) Then
                                CoOccurrenceMatrix(entity)(coOccurringEntity) = 0
                            End If

                            CoOccurrenceMatrix(entity)(coOccurringEntity) += 1
                        End If
                    Next
                End If
            Next

            Return CoOccurrenceMatrix
        End Function

        Public Shared Function CreateDataGridView(matrix As Dictionary(Of String, Dictionary(Of String, Integer))) As DataGridView
            Dim dataGridView As New DataGridView()
            dataGridView.Dock = DockStyle.Fill
            dataGridView.AutoGenerateColumns = False
            dataGridView.AllowUserToAddRows = False

            ' Add columns to the DataGridView
            Dim wordColumn As New DataGridViewTextBoxColumn()
            wordColumn.HeaderText = "Word"
            wordColumn.DataPropertyName = "Word"
            dataGridView.Columns.Add(wordColumn)

            For Each contextWord As String In matrix.Keys
                Dim contextColumn As New DataGridViewTextBoxColumn()
                contextColumn.HeaderText = contextWord
                contextColumn.DataPropertyName = contextWord
                dataGridView.Columns.Add(contextColumn)
            Next

            ' Populate the DataGridView with the matrix data
            For Each word As String In matrix.Keys
                Dim rowValues As New List(Of Object)()
                rowValues.Add(word)

                For Each contextWord As String In matrix.Keys
                    Dim count As Integer = If(matrix(word).ContainsKey(contextWord), matrix(word)(contextWord), 0)
                    rowValues.Add(count)
                Next

                dataGridView.Rows.Add(rowValues.ToArray())
            Next

            Return dataGridView
        End Function

        Public Sub AddDocument(document As String, contextWindow As Integer)
            Dim words As String() = document.Split({" "c}, StringSplitOptions.RemoveEmptyEntries)

            For i As Integer = 0 To words.Length - 1
                Dim currentWord As String = words(i)

                If Not matrix.ContainsKey(currentWord) Then
                    matrix(currentWord) = New Dictionary(Of String, Integer)()
                End If

                For j As Integer = Math.Max(0, i - contextWindow) To Math.Min(words.Length - 1, i + contextWindow)
                    If i <> j Then
                        Dim contextWord As String = words(j)

                        If Not matrix(currentWord).ContainsKey(contextWord) Then
                            matrix(currentWord)(contextWord) = 0
                        End If

                        matrix(currentWord)(contextWord) += 1
                    End If
                Next
            Next
        End Sub
        Public Shared Sub Main()
            ' Fill the matrix with your data
            Dim documents As List(Of String) = New List(Of String)()
            documents.Add("This is the first document.")
            documents.Add("The second document is here.")
            documents.Add("And this is the third document.")

            Dim contextWindow As Integer = 1
            Dim matrixBuilder As New Word2WordMatrix()

            For Each document As String In documents
                matrixBuilder.AddDocument(document, contextWindow)
            Next

            Dim wordWordMatrix As Dictionary(Of String, Dictionary(Of String, Integer)) = matrixBuilder.GetWordWordMatrix()

            ' Create the DataGridView control
            Dim dataGridView As DataGridView = Word2WordMatrix.CreateDataGridView(wordWordMatrix)

            ' Create a form and add the DataGridView to it
            Dim form As New Form()
            form.Text = "Word-Word Matrix"
            form.Size = New Size(800, 600)
            form.Controls.Add(dataGridView)

            ' Display the form
            Application.Run(form)
        End Sub
        Public Function GetWordWordMatrix() As Dictionary(Of String, Dictionary(Of String, Integer))
            Return matrix
        End Function
    End Class
    Public Class WordListReader
        Private wordList As List(Of String)

        Public Sub New(filePath As String)
            wordList = New List(Of String)()
            ReadWordList(filePath)
        End Sub

        Private Sub ReadWordList(filePath As String)
            Using reader As New StreamReader(filePath)
                While Not reader.EndOfStream
                    Dim line As String = reader.ReadLine()
                    If Not String.IsNullOrEmpty(line) Then
                        wordList.Add(line.Trim.ToLower)
                    End If
                End While
            End Using
        End Sub

        Public Function GetWords() As List(Of String)
            Return wordList
        End Function
        ' Usage Example:
        Public Shared Sub Main()
            ' Assuming you have a wordlist file named 'words.txt' in the same directory
            Dim corpusRoot As String = "."
            Dim wordlistPath As String = Path.Combine(corpusRoot, "wordlist.txt")

            Dim wordlistReader As New WordListReader(wordlistPath)
            Dim words As List(Of String) = wordlistReader.GetWords()

            For Each word As String In words
                Console.WriteLine(word)
            Next
            Console.ReadLine()
            ' Rest of your code...
        End Sub


    End Class
    Public Class WorGrams
        Public Shared Function ComputePairFrequencies(ByRef Vocabulary As Dictionary(Of String, Integer)) As Dictionary(Of String, Integer)
            Dim pairFrequencies As Dictionary(Of String, Integer) = New Dictionary(Of String, Integer)

            For Each token As String In Vocabulary.Keys
                Dim tokenChars As List(Of Char) = token.ToList()

                For i As Integer = 0 To tokenChars.Count - 2
                    Dim pair As String = tokenChars(i) & tokenChars(i + 1)

                    If Not pairFrequencies.ContainsKey(pair) Then
                        pairFrequencies.Add(pair, Vocabulary(token))
                    Else
                        pairFrequencies(pair) += Vocabulary(token)
                    End If
                Next
            Next

            Return pairFrequencies
        End Function
        Public Shared Function TrainWordGrams(trainingData As List(Of String),
                                           ByRef wordgramCounts As Dictionary(Of List(Of String), Integer)) As Dictionary(Of List(Of String), Double)
            On Error Resume Next
            ' Preprocess training data and tokenize into wordgrams
            Dim wordgrams As New List(Of List(Of String))

            For Each sentence As String In trainingData
                Dim tokens As List(Of String) = Tokenizer.TokenizeToWord(sentence)
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
        Public Shared Function GenerateWordPairs(ByVal words As List(Of String)) As IEnumerable(Of Tuple(Of String, String))
            Dim wordPairs As New List(Of Tuple(Of String, String))

            For i As Integer = 0 To words.Count - 2
                For j As Integer = i + 1 To words.Count - 1
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
        Public Shared Function CalculatePMI(corpus As List(Of List(Of String))) As Dictionary(Of String, Dictionary(Of String, Integer))
            Dim wordCount As New Dictionary(Of String, Integer)
            Dim coOccurrenceCount As New Dictionary(Of String, Dictionary(Of String, Integer))
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
                        coOccurrenceCount.Add(word, New Dictionary(Of String, Integer))
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
            Dim pmiScores As New Dictionary(Of String, Dictionary(Of String, Integer))()

            For Each word As String In wordCount.Keys
                pmiScores.Add(word, New Dictionary(Of String, Integer)())

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

    End Class
End Module
Public Class Tokenizer
    Public Property Vocabulary As Dictionary(Of String, Integer)
    Public ReadOnly Property PairFrequencies As Dictionary(Of String, Integer) = ComputePairFrequencies()
    Public ReadOnly Property maxSubwordLen As Integer = Me.Vocabulary.Max(Function(token) token.Key.Length)
    Private ReadOnly unkToken As String = "<Unk>"
    ''' <summary>
    ''' Defines max entries in vocabulary before Pruning Rare Words
    ''' </summary>
    ''' <returns></returns>
    Public Property VocabularyPruneValue As Integer = 100000

    Public Sub New()
        Vocabulary = New Dictionary(Of String, Integer)

    End Sub
    Public Function GetVocabulary() As List(Of String)
        Return Vocabulary.Keys.ToList()
    End Function

    Public Sub New(vocabulary As Dictionary(Of String, Integer), Optional vocabularyPruneValue As Integer = 1000000)
        Me.Vocabulary = vocabulary
        Me.VocabularyPruneValue = vocabularyPruneValue
    End Sub

    Private Function TokenizeWordPiece(text As String) As List(Of String)
        Dim tokens As New List(Of String)
        Dim pos As Integer = 0

        While pos < text.Length
            Dim foundSubword As Boolean = False
            Dim subword As String = ""

            For subwordLen As Integer = Math.Min(Me.maxSubwordLen, text.Length - pos) To 1 Step -1
                subword = text.Substring(pos, subwordLen)

                If Vocabulary.Keys.Contains(subword) Then
                    tokens.Add(subword)
                    pos += subwordLen
                    foundSubword = True
                    Exit For
                End If
            Next

            ' If no subword from the vocabulary matches, split into WordPiece tokens
            If Not foundSubword Then
                Dim wordPieceTokens As List(Of String) = TokenizeBitWord(subword)
                tokens.AddRange(wordPieceTokens)
                UpdateVocabulary(subword)
                pos += subword.Length
            End If
        End While

        Return tokens
    End Function
    Private Function TokenizeBitWord(subword As String) As List(Of String)
        Dim wordPieceTokens As New List(Of String)
        Dim startIdx As Integer = 0

        While startIdx < subword.Length
            Dim endIdx As Integer = subword.Length
            Dim foundSubword As Boolean = False

            While startIdx < endIdx
                Dim candidate As String = subword.Substring(startIdx, endIdx - startIdx)
                Dim isLast = endIdx = subword.Length

                If Vocabulary.Keys.Contains(candidate) OrElse isLast Then
                    wordPieceTokens.Add(candidate)
                    startIdx = endIdx
                    foundSubword = True
                    Exit While
                End If

                endIdx -= 1
            End While

            ' If no subword from the vocabulary matches, break the subword into smaller parts
            If Not foundSubword Then
                wordPieceTokens.Add("<unk>")
                startIdx += 1
            End If
        End While

        Return wordPieceTokens
    End Function
    Private Shared Function TokenizeBitWord(subword As String, ByRef Vocab As Dictionary(Of String, Integer)) As List(Of String)

        Dim wordPieceTokens As New List(Of String)
        Dim startIdx As Integer = 0

        While startIdx < subword.Length
            Dim endIdx As Integer = subword.Length
            Dim foundSubword As Boolean = False

            While startIdx < endIdx
                Dim candidate As String = subword.Substring(startIdx, endIdx - startIdx)
                Dim isLast = endIdx = subword.Length

                If Vocab.Keys.Contains(candidate) OrElse isLast Then
                    wordPieceTokens.Add(candidate)
                    startIdx = endIdx
                    foundSubword = True
                    Exit While
                End If

                endIdx -= 1
            End While

            ' If no subword from the vocabulary matches, break the subword into smaller parts
            If Not foundSubword Then
                wordPieceTokens.Add("<unk>")
                startIdx += 1
            End If
        End While

        Return wordPieceTokens
    End Function

    Private Function TokenizeBPE(ByVal text As String) As List(Of String)
        Dim tokens As New List(Of String)

        While text.Length > 0
            Dim foundToken As Boolean = False

            ' Find the longest token in the vocabulary that matches the start of the text
            For Each subword In Vocabulary.OrderByDescending(Function(x) x.Key.Length)
                If text.StartsWith(subword.Key) Then
                    tokens.Add(subword.Key)
                    text = text.Substring(subword.Key.Length)
                    foundToken = True
                    Exit For
                End If
            Next

            ' If no token from the vocabulary matches, break the text into subwords
            If Not foundToken Then
                Dim subwordFound As Boolean = False
                Dim subword As String = ""
                ' Divide the text into subwords starting from the longest possible length
                For length = Math.Min(text.Length, 20) To 1 Step -1
                    subword = text.Substring(0, length)

                    ' Check if the subword is in the vocabulary
                    If Vocabulary.Keys(subword) Then
                        tokens.Add(subword)
                        text = text.Substring(length)
                        subwordFound = True
                        Exit For
                    End If
                Next

                ' If no subword from the vocabulary matches,
                'Learn On the fly, But 
                If Not subwordFound Then
                    '    Throw New Exception("Unrecognized subword in the text.")
                    tokens.AddRange(TokenizeBitWord(unkToken & subword))
                    UpdateVocabulary(subword)

                End If
            End If
        End While

        Return tokens
    End Function
    Private Class NgramTokenizer

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

            ' Split the text into Clauses
            Dim Clauses() As String = text.Split({".", ",", ";", ":", "!", "?"}, StringSplitOptions.RemoveEmptyEntries)

            ' Generate sentence n-grams
            For i As Integer = 0 To Clauses.Length - n
                Dim ngram As String = String.Join(" ", Clauses.Skip(i).Take(n))
                tokens.Add(ngram)
            Next

            Return tokens
        End Function

    End Class
    Private Class BasicTokenizer

        Public Shared Function TokenizeToCharacter(Document As String) As List(Of String)
            TokenizeToCharacter = New List(Of String)
            Document = Document.ToLower()
            For i = 0 To Document.Length - 1
                TokenizeToCharacter.Add(Document(i))
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
            Return Document.Split({".", ",", ";", ":", "!", "?"}, StringSplitOptions.RemoveEmptyEntries).ToList
        End Function

        Public Shared Function TokenizeToParagraph(Document As String) As List(Of String)
            Document = Document.ToLower()
            Return Split(Document, vbNewLine).ToList
        End Function

    End Class
    Public Sub Add_Vocabulary(initialVocabulary As List(Of String))

        For Each word In initialVocabulary

            UpdateVocabulary(word)

        Next

    End Sub
    Public Sub Initialize_Vocabulary(initialVocabulary As List(Of String), n As Integer)

        For Each word In initialVocabulary
            For i As Integer = 0 To word.Length - n
                UpdateVocabulary(word.Substring(i, n))
            Next
        Next

    End Sub
    Private Function ComputePairFrequencies() As Dictionary(Of String, Integer)
        Dim pairFrequencies As Dictionary(Of String, Integer) = New Dictionary(Of String, Integer)

        For Each token As String In Vocabulary.Keys
            Dim tokenChars As List(Of Char) = token.ToList()

            For i As Integer = 0 To tokenChars.Count - 2
                Dim pair As String = tokenChars(i) & tokenChars(i + 1)

                If Not pairFrequencies.ContainsKey(pair) Then
                    pairFrequencies.Add(pair, Vocabulary(token))
                Else
                    Dim value = pairFrequencies(pair)
                    value += Vocabulary(token)
                    pairFrequencies.Remove(pair)
                    pairFrequencies.Add(pair, value)


                End If
            Next
        Next

        Return pairFrequencies
    End Function

    Private Sub UpdateFrequencyDictionary(mergedSubword As String)
        PairFrequencies.Remove("")
        For i As Integer = 0 To mergedSubword.Length - 2
            Dim bigram As String = mergedSubword.Substring(i, 2)
            If PairFrequencies.ContainsKey(bigram) Then
                PairFrequencies(bigram) += 1
            Else
                PairFrequencies.Add(bigram, 1)
            End If
        Next
    End Sub
    Public Sub UpdateVocabulary(ByRef Term As String)
        If Vocabulary.Keys.Contains(Term) = True Then
            Dim value = Vocabulary(Term)
            value += 1
            Vocabulary.Remove(Term)
            Vocabulary.Add(Term, value)
        Else
            Vocabulary.Add(Term, 1)
        End If

    End Sub
    Public Shared Function UpdateCorpusWithMergedToken(ByRef corpus As List(Of String), pair As String) As List(Of String)
        ' Update the text corpus with the merged token for the next iteration.
        Return corpus.ConvertAll(Function(text) text.Replace(pair, pair.Replace(" ", "_")))
    End Function
    Public Sub Prune(pruningThreshold As Integer)

        Dim minimumVocabularySize As Integer = VocabularyPruneValue
        If Vocabulary.Count > minimumVocabularySize Then
            PruneVocabulary(pruningThreshold)
        End If

    End Sub
    Private Sub PruneVocabulary(threshold As Integer)
        ' Create a list to store tokens to be removed.
        Dim tokensToRemove As New List(Of String)

        ' Iterate through the vocabulary and identify tokens to prune.
        For Each token In Vocabulary
            Dim tokenId As Integer = token.Value
            Dim tokenFrequency As Integer = Vocabulary(token.Key)

            ' Prune the token if it has frequency below the threshold (1) and is not recent (has a lower ID).
            If tokenFrequency <= threshold AndAlso tokenId < Vocabulary.Count - 1 Then
                tokensToRemove.Add(token.Key)
            End If
        Next

        ' Remove the identified tokens from the vocabulary.
        For Each tokenToRemove In tokensToRemove
            Vocabulary.Remove(tokenToRemove)
        Next

        Console.WriteLine("Pruning completed. Vocabulary size after pruning: " & Vocabulary.Count)
        Console.ReadLine()
    End Sub
    Public Sub Train(text As String, Epochs As Integer)
        ' Tokenize the text into individual characters

        Dim Bits As List(Of String) = TokenizeBitWord(text)
        For Each bit As String In Bits
            UpdateVocabulary(bit)
        Next


        ' Train BPE using merging strategy
        Dim numMerges As Integer = Epochs ' Define the number of merges, you can adjust it as needed
        For mergeIndex As Integer = 0 To numMerges - 1
            MergeMostFrequentBigram()
            MergeMostFrequentPair(FindMostFrequentPair.Key)
        Next

        Prune(1)
    End Sub
    Public Function Tokenize(singleDocument As String, isWordPiece As Boolean) As List(Of String)
        ' Tokenize the document using the current vocabulary.
        Dim tokens As List(Of String) = If(isWordPiece, Tokenize(singleDocument, True), Tokenize(singleDocument, False))
        If tokens.Contains(unkToken) = True Then
            tokens = TrainAndTokenize(singleDocument, isWordPiece, 1)
        End If
        Return tokens
    End Function
    Private Function TrainAndTokenize(singleDocument As String, isWordPiece As Boolean, Epochs As Integer) As List(Of String)
        ' Tokenize the document using the current vocabulary.
        Dim tokens As List(Of String) = If(isWordPiece, Tokenize(singleDocument, True), Tokenize(singleDocument, False))

        ' Train the tokenizer using the same document.
        If isWordPiece Then
            TrainWordPiece(singleDocument, Epochs)
        Else
            TrainBPE(singleDocument, Epochs)
        End If

        ' Re-tokenize the document with the updated vocabulary.
        Return If(isWordPiece, TokenizeWordPiece(singleDocument), TokenizeBPE(singleDocument))
    End Function
    Public Sub Train(text As String, isWordPiece As Boolean, Epochs As Integer)
        If isWordPiece Then
            TrainWordPiece(text, Epochs)
        Else
            TrainBPE(text, Epochs)
        End If
        Prune(1)
    End Sub
    Private Sub TrainWordPiece(text As String, Epochs As Integer)
        ' Tokenize the text into individual characters
        Dim Bits As List(Of String) = TokenizeWordPiece(text)
        For Each bit As String In Bits
            UpdateVocabulary(bit)
        Next

        ' Train WordPiece using merging strategy
        Dim numMerges As Integer = Epochs ' Define the number of merges, you can adjust it as needed
        For mergeIndex As Integer = 0 To numMerges - 1
            MergeMostFrequentBigram()
            MergeMostFrequentPair(FindMostFrequentPair.Key)
        Next
    End Sub
    Private Sub TrainBPE(text As String, Epochs As Integer)
        ' Tokenize the text into individual characters
        Dim Bits As List(Of String) = TokenizeBPE(text)
        For Each bit As String In Bits
            UpdateVocabulary(bit)
        Next

        ' Train BPE using merging strategy
        Dim numMerges As Integer = Epochs ' Define the number of merges, you can adjust it as needed
        For mergeIndex As Integer = 0 To numMerges - 1
            MergeMostFrequentBigram()
            MergeMostFrequentPair(FindMostFrequentPair.Key)
        Next
    End Sub
    Private Function FindMostFrequentPair() As KeyValuePair(Of String, Integer)
        ' Find the most frequent character pair from the frequency counts.
        Return PairFrequencies.Aggregate(Function(x, y) If(x.Value > y.Value, x, y))
    End Function
    Private Sub MergeMostFrequentPair(pair As String)
        ' Merge the most frequent character pair into a new subword unit.
        Dim mergedToken As String = pair.Replace(" ", "_")
        UpdateVocabulary(mergedToken)

    End Sub
    Private Sub MergeMostFrequentBigram()
        Dim mostFrequentBigram As String = GetMostFrequentBigram()
        If mostFrequentBigram IsNot Nothing Then
            Dim mergedSubword As String = mostFrequentBigram.Replace("", " ")

            UpdateVocabulary(mergedSubword)

        End If
    End Sub
    Private Function GetMostFrequentBigram() As String
        Dim mostFrequentBigram As String = Nothing
        Dim maxFrequency As Integer = 0

        For Each bigram In PairFrequencies.Keys
            If PairFrequencies(bigram) > maxFrequency Then
                mostFrequentBigram = bigram
                maxFrequency = PairFrequencies(bigram)
            End If
        Next

        Return mostFrequentBigram
    End Function

    Public Shared Function FindFrequentCharacterBigrams(Vocab As List(Of String), ByRef Freq_Threshold As Integer) As List(Of String)
        Dim bigramCounts As New Dictionary(Of String, Integer)

        For Each word In Vocab
            Dim characters As Char() = word.ToCharArray()

            For i As Integer = 0 To characters.Length - 2
                Dim bigram As String = characters(i) & characters(i + 1)

                If bigramCounts.ContainsKey(bigram) Then
                    bigramCounts(bigram) += 1
                Else
                    bigramCounts.Add(bigram, 1)
                End If
            Next
        Next

        Dim frequentCharacterBigrams As New List(Of String)

        For Each pair In bigramCounts
            If pair.Value > Freq_Threshold Then ' Adjust the threshold as needed
                frequentCharacterBigrams.Add(pair.Key)
            End If
        Next

        Return frequentCharacterBigrams
    End Function
    Public Shared Function GetHighFreq(ByRef Vocabulary As Dictionary(Of String, Integer), ByRef Threshold As Integer) As List(Of String)
        Dim HighFreq As New List(Of String)
        For Each item In Vocabulary
            If item.Value > Threshold Then
                HighFreq.Add(item.Key)
            End If
        Next
        Return HighFreq
    End Function
    Public Shared Function TokenizeToCharacter(text As String) As List(Of String)
        Return BasicTokenizer.TokenizeToCharacter(text)
    End Function
    Public Shared Function TokenizeToWord(text As String) As List(Of String)
        Return BasicTokenizer.TokenizeToWord(text)
    End Function
    Public Shared Function TokenizeToSentence(text As String) As List(Of String)
        Return BasicTokenizer.TokenizeToSentence(text)
    End Function
    Public Shared Function TokenizeToSentenceGram(text As String, ByRef n As Integer) As List(Of String)
        Return NgramTokenizer.TokenizetoSentence(text, n)
    End Function
    Public Shared Function TokenizeToWordGram(text As String, ByRef n As Integer) As List(Of String)
        Return NgramTokenizer.TokenizetoWord(text, n)
    End Function
    Public Shared Function TokenizeToNGram(text As String, ByRef n As Integer) As List(Of String)
        Return NgramTokenizer.TokenizetoCharacter(text, n)
    End Function
    Public Shared Function TokenizeToBitWord(text As String, ByRef Vocab As Dictionary(Of String, Integer)) As List(Of String)
        Dim Words = Tokenizer.TokenizeToWord(text)
        Dim Tokens As New List(Of String)
        For Each item In Words
            Tokens.AddRange(TokenizeBitWord(item, Vocab))
        Next
        Return Tokens
    End Function
End Class

Namespace Examples
    Public Class TokenizerExample
        Public Sub Main()
            Dim Corpus As List(Of String) = GetDocumentCorpus()
            Dim sentences As New List(Of String) From {
            "I love apples.",
            "Bananas are tasty."}
            Dim Tokenizer As New Tokenizer
            For Each item In Corpus
                Tokenizer.Train(item, 5)
            Next

            For Each item In sentences
                Console.WriteLine("Document =" & item)
                Dim Tokens = Tokenizer.Tokenize(item, True)

                For Each Tok In Tokens
                    Console.WriteLine("TOKEN =" & Tok)
                Next

            Next

        End Sub
        ''' <summary>
        ''' When no lists are available, A mixed corpus of documents 
        ''' </summary>
        ''' <returns></returns>
        Public Function GetDocumentCorpus() As List(Of String)
            ' Load paragraphs based on different topics
            Dim paragraphs As New List(Of String)
            Dim sentences As New List(Of String) From {
            "The quick brown fox jumped over the sly lazy dog",
            "Bananas are tasty.",
            "I love apples.",
            "I enjoy eating bananas.",
            "Kiwi is a delicious fruit.", "Bananas are tasty.",
            "I love apples.", "I enjoy eating bananas.",
            "Kiwi is a delicious fruit.", "I love apples.",
            "I enjoy eating bananas.",
            "Kiwi is a delicious fruit.", "I love apples.",
            "I enjoy eating bananas.",
            "Kiwi is a delicious fruit.", "Bananas are tasty.", "Fisherman, like to fish in the sea, every the Fisher has fished in every place he is fishing.",
        "the lowest of the lower of the lowered tempo of the music",
        "the higher and highest and",
        "I was running, she ran after me, he was run down, until he was finished",
        "it was the the end came and the party ended."
    }
            ' Computer Science Topics
            paragraphs.Add("Computer Science is the study of computation and information processing.")
            paragraphs.Add("Algorithms and data structures are fundamental concepts in computer science.")
            paragraphs.Add("Computer networks enable communication and data exchange between devices.")
            paragraphs.Add("Artificial Intelligence is a branch of computer science that focuses on creating intelligent machines.")
            paragraphs.Add("Software engineering is the discipline of designing, developing, and maintaining software systems.")

            ' NLP Topics
            paragraphs.Add("Natural Language Processing (NLP) is a subfield of artificial intelligence.")
            paragraphs.Add("NLP techniques enable computers to understand, interpret, and generate human language.")
            paragraphs.Add("Named Entity Recognition (NER) is a common task in NLP.")
            paragraphs.Add("Machine Translation is the task of translating text from one language to another.")
            paragraphs.Add("Sentiment analysis aims to determine the sentiment or opinion expressed in a piece of text.")

            paragraphs.Add("The quick brown fox jumps over the lazy dog.")
            paragraphs.Add("The cat and the dog are best friends.")
            paragraphs.Add("Programming languages are used to write computer programs.")
            paragraphs.Add("Natural Language Processing (NLP) is a subfield of artificial intelligence.")
            paragraphs.Add("Machine learning algorithms can be used for sentiment analysis.")
            ' Train the model on a corpus of text
            Dim trainingData As New List(Of String)
            trainingData.Add("Hello")
            trainingData.Add("Hi there")
            trainingData.Add("How are you?")
            trainingData.Add("What's up?")
            trainingData.Add("I'm doing well, thanks!")
            trainingData.Add("Not too bad, how about you?")
            trainingData.Add("Great! What can I help you with?")
            trainingData.Add("I need some assistance")
            trainingData.Add("Can you provide me with information?")
            trainingData.Add("Sure, what do you need?")
            trainingData.Add("Can you tell me about your services?")
            trainingData.Add("We offer a wide range of services to cater to your needs.")
            trainingData.Add("What are the payment options?")
            trainingData.Add("We accept all major credit cards and PayPal.")
            trainingData.Add("Do you have any ongoing promotions?")
            trainingData.Add("Yes, we have a special discount available for new customers.")
            trainingData.Add("How long does shipping take?")
            trainingData.Add("Shipping usually takes 3-5 business days.")
            trainingData.Add("What is your return policy?")
            trainingData.Add("We offer a 30-day return policy for unused items.")
            trainingData.Add("Can you recommend a good restaurant nearby?")
            trainingData.Add("Sure! There's a great Italian restaurant called 'La Bella Vita' just a few blocks away.")
            trainingData.Add("What movies are currently playing?")
            trainingData.Add("The latest releases include 'Avengers: Endgame' and 'The Lion King'.")
            trainingData.Add("What time does the museum open?")
            trainingData.Add("The museum opens at 9:00 AM.")
            trainingData.Add("How do I reset my password?")
            trainingData.Add("You can reset your password by clicking on the 'Forgot Password' link on the login page.")
            trainingData.Add("What are the system requirements for this software?")
            trainingData.Add("The system requirements are listed on our website under the 'Support' section.")
            trainingData.Add("Can you provide technical support?")
            trainingData.Add("Yes, we have a dedicated support team available 24/7 to assist you.")
            trainingData.Add("What is the best way to contact customer service?")
            trainingData.Add("You can reach our customer service team by phone, email, or live chat.")
            trainingData.Add("How do I cancel my subscription?")
            trainingData.Add("To cancel your subscription, please go to your account settings and follow the instructions.")
            trainingData.Add("What are the available colors for this product?")
            trainingData.Add("The available colors are red, blue, and green.")
            trainingData.Add("Do you offer international shipping?")
            trainingData.Add("Yes, we offer international shipping to select countries.")
            trainingData.Add("Can I track my order?")
            trainingData.Add("Yes, you can track your order by entering the tracking number on our website.")
            trainingData.Add("What is your privacy policy?")
            trainingData.Add("Our privacy policy can be found on our website under the 'Privacy' section.")
            trainingData.Add("How do I request a refund?")
            trainingData.Add("To request a refund, please contact our customer service team with your order details.")
            trainingData.Add("What are the opening hours?")
            trainingData.Add("We are open from Monday to Friday, 9:00 AM to 6:00 PM.")
            trainingData.Add("Is there a warranty for this product?")
            trainingData.Add("Yes, this product comes with a one-year warranty.")
            trainingData.Add("Can I schedule an appointment?")
            trainingData.Add("Yes, you can schedule an appointment by calling our office.")
            trainingData.Add("Do you have any vegetarian options?")
            trainingData.Add("Yes, we have a dedicated vegetarian menu.")
            trainingData.Add("What is your company's mission statement?")
            trainingData.Add("Our mission is to provide high-quality products and excellent customer service.")
            trainingData.Add("How can I apply for a job at your company?")
            trainingData.Add("You can apply for a job by submitting your resume through our online application form.")
            'movie dialogues
            trainingData.Add("Luke: I am your father.")
            trainingData.Add("Darth Vader: Noooo!")
            trainingData.Add("Han Solo: May the Force be with you.")
            trainingData.Add("Princess Leia: I love you.")
            trainingData.Add("Han Solo: I know.")
            trainingData.Add("Yoda: Do or do not. There is no try.")
            trainingData.Add("Obi-Wan Kenobi: You were the chosen one!")
            trainingData.Add("Anakin Skywalker: I hate you!")
            trainingData.Add("Marty McFly: Great Scott!")
            trainingData.Add("Doc Brown: Roads? Where we're going, we don't need roads.")
            trainingData.Add("Tony Stark: I am Iron Man.")
            trainingData.Add("Peter Parker: With great power comes great responsibility.")
            trainingData.Add("Bruce Wayne: I'm Batman.")
            trainingData.Add("Alfred Pennyworth: Why do we fall? So we can learn to pick ourselves up.")
            trainingData.Add("Sherlock Holmes: Elementary, my dear Watson.")
            trainingData.Add("Dr. John Watson: It is a capital mistake to theorize before one has data.")
            trainingData.Add("James Bond: The name's Bond. James Bond.")
            trainingData.Add("Harry Potter: I solemnly swear that I am up to no good.")
            trainingData.Add("Ron Weasley: Bloody hell!")
            trainingData.Add("Hermione Granger: It's LeviOsa, not LevioSA.")
            trainingData.Add("Gandalf: You shall not pass!")
            trainingData.Add("Frodo Baggins: I will take the ring, though I do not know the way.")
            trainingData.Add("Samwise Gamgee: I can't carry it for you, but I can carry you!")
            trainingData.Add("Dumbledore: Happiness can be found even in the darkest of times.")
            trainingData.Add("Severus Snape: Always.")


            paragraphs.AddRange(trainingData)

            Dim inputTexts As String() = {
                "John Doe is a software developer from New York. He specializes in Python programming.",
                "Mary Smith is an artist from Los Angeles. She loves to paint landscapes.",
                "Peter Johnson is a doctor from Chicago. He works at a local hospital.",
                "Sara Williams is a teacher from Boston. She teaches English literature.",
                "David Brown is a musician from Seattle. He plays the guitar in a band.",
                "I am a software developer with 5 years of experience. I have expertise in Python and Java.",
        "As a data scientist, I have a Ph.D. in Machine Learning and 8 years of experience.",
        "I am a web developer skilled in Java and Python. I have worked at Microsoft for 10 years.",
        "I am an electrical engineer with a Master's degree and 8 years of experience in power systems.",
        "As a nurse, I have a Bachelor's degree in Nursing and 5 years of experience in a hospital setting.",
        "I am a graphic designer with expertise in Adobe Photoshop and Illustrator. I have worked freelance for 5 years.",
        "As a teacher, I have a Bachelor's degree in Education and 8 years of experience in primary schools.",
        "I am a mechanical engineer with a Ph.D. in Robotics and 10 years of experience in autonomous systems.",
        "As a lawyer, I have a Juris Doctor degree and 5 years of experience in corporate law.",
        "I am a marketing specialist with expertise in digital marketing and social media management. I have worked at Google for 8 years.",
        "As a chef, I have culinary training and 5 years of experience in high-end restaurants.",
        "I am a financial analyst with a Master's degree in Finance and 8 years of experience in investment banking.",
        "I am a software developer with 5 years of experience. I have expertise in Python and Java.",
        "As a data scientist, I have a Ph.D. in Machine Learning and 8 years of experience.",
        "I am a web developer skilled in Java and Python. I have worked at Microsoft for 10 years.",
        "I am an electrical engineer with a Master's degree and 8 years of experience in power systems.",
        "As a nurse, I have a Bachelor's degree in Nursing and 5 years of experience in a hospital setting.",
        "I am a graphic designer with expertise in Adobe Photoshop and Illustrator. I have worked freelance for 5 years.",
        "As a teacher, I have a Bachelor's degree in Education and 8 years of experience in primary schools.",
        "I am a mechanical engineer with a Ph.D. in Robotics and 10 years of experience in autonomous systems.",
        "As a lawyer, I have a Juris Doctor degree and 5 years of experience in corporate law.",
        "I am a marketing specialist with expertise in digital marketing and social media management. I have worked at Google for 8 years.",
        "As a chef, I have culinary training and 5 years of experience in high-end restaurants.",
        "I am a financial analyst with a Master's degree in Finance and 8 years of experience in investment banking.",
        "I am a software developer with 5 years of experience. I have expertise in Python and Java.",
        "As a data scientist, I have a Ph.D. in Machine Learning and 8 years of experience.",
        "I am a web developer skilled in Java and Python. I have worked at Microsoft for 10 years.",
        "I am an electrical engineer with a Master's degree and 8 years of experience in power systems.",
        "As a nurse, I have a Bachelor's degree in Nursing and 5 years of experience in a hospital setting.",
        "I am a graphic designer with expertise in Adobe Photoshop and Illustrator. I have worked freelance for 5 years.",
        "As a teacher, I have a Bachelor's degree in Education and 8 years of experience in primary schools.",
        "I am a mechanical engineer with a Ph.D. in Robotics and 10 years of experience in autonomous systems.",
        "As a lawyer, I have a Juris Doctor degree and 5 years of experience in corporate law.",
        "I am a marketing specialist with expertise in digital marketing and social media management. I have worked at Google for 8 years.",
        "As a chef, I have culinary training and 5 years of experience in high-end restaurants.",
        "I am a financial analyst with a Master's degree in Finance and 8 years of experience in investment banking."
    }
            paragraphs.AddRange(inputTexts)
            Dim NLP As String = "Natural language processing (NLP) Is a field Of artificial intelligence that focuses On the interaction between computers And humans Using natural language. It combines linguistics, computer science, And machine learning To enable computers To understand, interpret, And generate human language.

Machine learning is a subset of artificial intelligence that deals with the development of algorithms and models that allow computers to learn and make predictions or decisions without being explicitly programmed. It plays a crucial role in various applications, including NLP.

In recent news, researchers at XYZ University have developed a new deep learning algorithm for sentiment analysis in NLP. The algorithm achieved state-of-the-art results on multiple datasets and has the potential to improve various NLP tasks.

Another significant development in the computer science industry is the introduction of GPT-3, a powerful language model developed by OpenAI. GPT-3 utilizes advanced machine learning techniques to generate human-like text and has shown promising results in various language-related tasks.

Key people in the data science and AI industry include Andrew Ng, the founder of deeplearning.ai and a prominent figure in the field of machine learning, and Yann LeCun, the director of AI Research at Facebook and a pioneer in deep learning.

These are just a few examples of the vast field of NLP, machine learning, and the latest developments in the computer science industry."
            paragraphs.Add(NLP)
            paragraphs.AddRange(sentences)
            Return paragraphs
        End Function

    End Class
End Namespace