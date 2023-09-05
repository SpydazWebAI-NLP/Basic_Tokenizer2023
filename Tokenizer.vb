Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Text.RegularExpressions
Imports SpydazWebAI.Token
Imports SpydazWebAI.TokenizerModels.BaseModels
Imports SpydazWebAI.TokenizerModels.BertModels.Base

Namespace TokenizerModels
    Namespace BaseModels
        Public MustInherit Class iTokenizer
            Public _vocabularyDict As Dictionary(Of String, Integer)
            Private ivocab As New List(Of String)
            Public Property _vocabulary As List(Of String)
                Get
                    Return getTokenlist(_vocabularyDict)
                End Get
                Set(value As List(Of String))
                    ivocab = value
                End Set
            End Property
            Public Shared Function CalculatePairFrequency(vocabulary As Dictionary(Of String, Integer), subword1 As String, subword2 As String) As Integer
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

            Public Shared Function FindMostFrequentPair(vocabulary As Dictionary(Of String, Integer)) As SubWord_Pair
                Dim mostFrequentPair As SubWord_Pair = Nothing
                Dim maxFrequency As Integer = 0

                For Each subword1 As String In vocabulary.Keys
                    For Each subword2 As String In vocabulary.Keys
                        If subword1 <> subword2 Then
                            Dim pairFrequency As Integer = CalculatePairFrequency(vocabulary, subword1, subword2)
                            If pairFrequency > maxFrequency Then
                                maxFrequency = pairFrequency
                                mostFrequentPair = New SubWord_Pair(subword1, subword2, pairFrequency)
                            End If
                        End If
                    Next
                Next

                Return mostFrequentPair
            End Function

            Public Shared Function MergeSubwordPair(vocabulary As Dictionary(Of String, Integer), pairToMerge As SubWord_Pair, newSubword As String) As Dictionary(Of String, Integer)
                Dim newVocabulary As New Dictionary(Of String, Integer)

                For Each subword As String In vocabulary.Keys
                    Dim mergedSubword As String = subword.Replace(pairToMerge.Sub_word_1 + pairToMerge.Sub_Word_2, newSubword)
                    newVocabulary(mergedSubword) = vocabulary(subword)
                Next

                Return newVocabulary
            End Function

            Public Shared Function ReadFile(filePath As String) As List(Of String)
                Dim linesArray As String() = File.ReadAllLines(filePath)
                Dim linesList As List(Of String) = linesArray.ToList()
                Return linesList
            End Function

            Public Shared Function UpdateVocabulary(vocabulary As Dictionary(Of String, Integer), Term As String) As Dictionary(Of String, Integer)
                If vocabulary(Term) > 0 Then
                    Dim Freq As Integer = vocabulary(Term)
                    Freq += 1
                    vocabulary.Remove(Term)
                    vocabulary.Add(Term, Freq)
                Else
                    vocabulary.Add(Term, 1)
                End If
                Return vocabulary
            End Function
            Public Function getTokenlist(Vocab As Dictionary(Of String, Integer)) As List(Of String)
                Dim Tokens As New List(Of String)
                For Each item In Vocab
                    Tokens.Add(item.Key)
                Next
                Return Tokens
            End Function

            Public Function IdToToken(ByVal id As Integer) As String
                Return _vocabulary(id)
            End Function

            Public Sub UpdateVocabulary(ByRef Term As String)
                If _vocabularyDict.Keys.Contains(Term) = True Then
                    Dim value = _vocabularyDict(Term)
                    value += 1
                    _vocabularyDict.Remove(Term)
                    _vocabularyDict.Add(Term, value)
                Else
                    _vocabularyDict.Add(Term, 1)
                End If

            End Sub
        End Class
        ''' <summary>
        ''' Basic Tokenizer Model USed for Creating sub model Tokenizers
        ''' </summary>
        Public MustInherit Class Tokenizer
            Inherits TokenizerBase


            Public MustOverride Function Tokenize(text As String) As List(Of String)

            Public MustOverride Sub Train(ByRef Corpus As List(Of String))
        End Class
        ''' <summary>
        ''' a basic tokenizer ; with quick tokenizer functionality
        ''' </summary>
        Public MustInherit Class TokenizerBase
            Inherits iTokenizer

            Public Shared Function CharGram(Document As String, n As Integer) As List(Of String)
                CharGram = New List(Of String)
                Document = Document.ToLower()
                Document = Document.SpacePunctuation

                ' Generate character n-grams
                For i As Integer = 0 To Document.Length - n
                    Dim ngram As String = Document.Substring(i, n)
                    CharGram.Add(ngram)
                Next

            End Function

            Public Shared Function ParagraphGram(text As String, n As Integer) As List(Of String)
                ParagraphGram = New List(Of String)

                ' Split the text into paragraphs
                Dim paragraphs() As String = text.Split({Environment.NewLine & Environment.NewLine}, StringSplitOptions.RemoveEmptyEntries)

                ' Generate paragraph n-grams
                For i As Integer = 0 To paragraphs.Length - n
                    Dim ngram As String = String.Join(Environment.NewLine & Environment.NewLine, paragraphs.Skip(i).Take(n))
                    ParagraphGram.Add(ngram)
                Next

                Return ParagraphGram
            End Function

            Public Shared Function SentenceGram(text As String, n As Integer) As List(Of String)
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

            Public Shared Function TokenizeToCharacter(Document As String) As List(Of String)
                Document = Document.ToLower()
                Dim characters As Char() = Document.ToCharArray()
                TokenizeToCharacter = New List(Of String)
                For Each item In characters
                    TokenizeToCharacter.Add(item)
                Next
            End Function

            Public Shared Function TokenizeToParagraph(Document As String) As List(Of String)
                Document = Document.ToLower()

                Return Split(Document, vbNewLine).ToList
            End Function

            Public Shared Function TokenizeToSentence(Document As String) As List(Of String)
                Document = Document.ToLower()
                Document = Document.SpacePunctuation
                Return Split(Document, ".").ToList
            End Function

            Public Shared Function TokenizeToWord(Document As String) As List(Of String)
                Document = Document.ToLower()
                Document = Document.SpacePunctuation
                Return Document.Split({" ", ".", ",", ";", ":", "!", "?"}, StringSplitOptions.RemoveEmptyEntries).ToList
            End Function
            Public Shared Function WordGram(ByRef text As String, n As Integer) As List(Of String)
                WordGram = New List(Of String)
                text = text.ToLower()
                text = text.SpacePunctuation

                ' Split the clean text into individual words
                Dim words() As String = text.Split({" ", ".", ",", ";", ":", "!", "?"}, StringSplitOptions.RemoveEmptyEntries)

                ' Generate n-grams from the words
                For i As Integer = 0 To words.Length - n
                    Dim ngram As String = String.Join(" ", words.Skip(i).Take(n))
                    WordGram.Add(ngram)
                Next

            End Function
        End Class
    End Namespace
    Namespace BertModels
        Namespace Base
            ''' <summary>
            ''' Base Tokenizer for bert Styled tokenization ;
            ''' This is the base model for The bert tokenizers
            ''' </summary>
            Public MustInherit Class Bert_Base
                Inherits iTokenizer
                ''' <summary>
                ''' Initiate With a Pre-trained Text File
                ''' </summary>
                ''' <param name="vocabularyFilePath"></param>
                Public Sub New(ByVal vocabularyFilePath As String)
                    _vocabulary = ReadFile(vocabularyFilePath)

                    _vocabularyDict = New Dictionary(Of String, Integer)()
                    For i = 0 To _vocabulary.Count - 1
                        _vocabularyDict(_vocabulary(i)) = i
                    Next
                End Sub
                ''' <summary>
                ''' Initiate with a pretrained Vocabulary list
                ''' </summary>
                ''' <param name="vocabulary"></param>
                Public Sub New(Optional vocabulary As List(Of String) = Nothing)
                    If vocabulary Is Nothing Then
                        _vocabulary = New List(Of String)
                    End If
                    _vocabulary = vocabulary
                    _vocabularyDict = New Dictionary(Of String, Integer)()
                    For i = 0 To _vocabulary.Count - 1
                        _vocabularyDict(_vocabulary(i)) = i
                    Next
                End Sub

                ''' <summary>
                ''' Load a Pre-Trained model, (ie Vocabulary)
                ''' </summary>
                ''' <param name="Pretrained"></param>
                Protected Sub New(Pretrained As BertModel)
                    Select Case Pretrained
                        Case BertModel.Cased
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_cased, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                        Case BertModel.CasedCustom
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_cased_large, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_cased_large.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                        Case BertModel.German
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_cased_german, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_cased_german.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                        Case BertModel.CasedLarge
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_cased_large, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_cased_large.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                        Case BertModel.Multilingual
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_cased_multilingual, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_cased_multilingual.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                        Case BertModel.UnCased
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_uncased, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_uncased.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                        Case BertModel.UnCasedCustom
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_uncased_large, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_uncased_large.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                        Case BertModel.UncasedLarge
                            ResourceExtractor.ExtractResourceToFile(My.Resources.base_uncased_large, Application.StartupPath & "\Vocabularies\Tokenizer_base_cased.txt")
                            _vocabulary = ReadFile(Application.StartupPath & "\Vocabularies\Tokenizer_base_uncased_large.txt")

                            _vocabularyDict = New Dictionary(Of String, Integer)()
                            For i = 0 To _vocabulary.Count - 1
                                _vocabularyDict(_vocabulary(i)) = i
                            Next
                    End Select
                End Sub

                Public Enum BertModel
                    Cased
                    CasedCustom
                    CasedLarge
                    UnCased
                    UnCasedCustom
                    UncasedLarge
                    Multilingual
                    German
                End Enum
                Public Function Encode(ByVal sequenceLength As Integer, ParamArray texts As String()) As List(Of (Long, Long, Long))
                    Dim tokens = Tokenize(texts)

                    Dim paddingCount As Integer = Math.Max(0, sequenceLength - tokens.Count)
                    Dim paddingTokens As List(Of (Integer, Integer)) = Enumerable.Repeat((0, 0), paddingCount).ToList()

                    Dim tokenIndexes As Long() = tokens.[Select](Function(token) CLng(token.Item2)).Concat(paddingTokens.[Select](Function(t) CLng(t.Item1))).ToArray()
                    Dim segmentIndexes As Long() = tokens.[Select](Function(token) CLng(token.Item3)).Concat(paddingTokens.[Select](Function(t) CLng(t.Item2))).ToArray()
                    Dim inputMask As Long() = tokens.[Select](Function(o) 1L).Concat(paddingTokens.[Select](Function(t) 0L)).ToArray()

                    Dim output = tokenIndexes.Zip(segmentIndexes, New Func(Of Long, Long, (Long, Long))(AddressOf ValueTuple.Create)).Zip(inputMask, Function(t, mask) (t.Item1, t.Item2, mask))

                    Return output.ToList()
                End Function

                ''' <summary>
                ''' Provides tokenized embeddings based on bert model
                ''' </summary>
                ''' <param name="texts"></param>
                ''' <returns></returns>
                Public Function Tokenize(ParamArray texts As String()) As List(Of (String, Integer, Long))
                    Dim tokens As IEnumerable(Of String) = New String() {Bert.Tokens.Classification}

                    For Each iTEXT In texts
                        tokens = tokens.Concat(Me.TokenizeSentence(iTEXT))
                        tokens = tokens.Concat(New String() {Bert.Tokens.Separation})
                    Next

                    Dim tokenAndIndex = tokens.SelectMany(New Func(Of String, IEnumerable(Of (Token As String, VocabularyIndex As Integer)))(AddressOf TokenizeSubwords)).ToList()

                    Dim segmentIndexes = SegmentIndex(tokenAndIndex)

                    Return tokenAndIndex.Zip(segmentIndexes, Function(tokenindex, segmentindex) (tokenindex.Token, tokenindex.VocabularyIndex, segmentindex)).ToList()
                End Function

                ''' <summary>
                ''' Reconstructs input based on tokens provided
                ''' </summary>
                ''' <param name="tokens"></param>
                ''' <returns></returns>
                Public Function Untokenize(ByVal tokens As List(Of String)) As List(Of String)
                    Dim currentToken = String.Empty
                    Dim untokens = New List(Of String)()
                    tokens.Reverse()

                    tokens.ForEach(Sub(token)
                                       If token.StartsWith("##") Then
                                           currentToken = token.Replace("##", "") & currentToken
                                       Else
                                           currentToken = token & currentToken
                                           untokens.Add(currentToken)
                                           currentToken = String.Empty
                                       End If
                                   End Sub)

                    untokens.Reverse()

                    Return untokens
                End Function

                ''' <summary>
                ''' To be overidden by Main Submodel MEthods , Such as Lowercased or Uncased
                ''' </summary>
                ''' <param name="text"></param>
                ''' <returns></returns>
                Protected MustOverride Function TokenizeSentence(ByVal text As String) As IEnumerable(Of String)

                Private Function SegmentIndex(ByVal tokens As List(Of (String, Integer))) As IEnumerable(Of Long)
                    Dim lSegmentIndex = 0
                    Dim segmentIndexes = New List(Of Long)()

                    For Each tokenIndex In tokens
                        Dim token = tokenIndex.Item1
                        Dim index = tokenIndex.Item2
                        segmentIndexes.Add(lSegmentIndex)

                        If Equals(token, Bert.Tokens.Separation) Then
                            lSegmentIndex += 1
                        End If
                    Next

                    Return segmentIndexes
                End Function
                ''' <summary>
                ''' SubWord Tokenizer
                ''' </summary>
                ''' <param name="word"></param>
                ''' <returns></returns>
                Private Function TokenizeSubwords(ByVal word As String) As IEnumerable(Of (String, Integer))
                    If _vocabularyDict.ContainsKey(word) Then
                        Return New(String, Integer)() {(word, _vocabularyDict(word))}
                    End If

                    Dim tokens = New List(Of (String, Integer))()
                    Dim remaining = word

                    While Not String.IsNullOrEmpty(remaining) AndAlso remaining.Length > 2
                        Dim prefix As String = Nothing
                        Dim subwordLength = remaining.Length
                        While subwordLength >= 1 ' was initially 2, which prevents using "character encoding"
                            Dim subword = remaining.Substring(0, subwordLength)
                            If Not _vocabularyDict.ContainsKey(subword) Then
                                subwordLength -= 1
                                Continue While
                            End If

                            prefix = subword
                            Exit While
                        End While

                        If Equals(prefix, Nothing) Then
                            tokens.Add((Bert.Tokens.Unknown, _vocabularyDict(Bert.Tokens.Unknown)))

                            Return tokens
                        End If

                        Dim regex = New Regex(prefix)
                        remaining = regex.Replace(remaining, "##", 1)

                        tokens.Add((prefix, _vocabularyDict(prefix)))
                    End While

                    If Not String.IsNullOrWhiteSpace(word) AndAlso Not tokens.Any() Then
                        tokens.Add((Bert.Tokens.Unknown, _vocabularyDict(Bert.Tokens.Unknown)))
                    End If

                    Return tokens
                End Function
            End Class
            ''' <summary>
            ''' Lower Cased Vocabulary
            ''' </summary>
            Public MustInherit Class CasedTokenizer
                Inherits Bert_Base
                Public Sub New(ByRef Model As BertModel)
                    MyBase.New(Model)
                End Sub


                ''' <summary>
                ''' Implments Lower cased tokenization
                ''' </summary>
                ''' <param name="text"></param>
                ''' <returns></returns>
                Protected Overrides Function TokenizeSentence(ByVal text As String) As IEnumerable(Of String)
                    Return text.Split(New String() {" ", "   ", vbCrLf}, StringSplitOptions.None).SelectMany(Function(o) o.SplitAndKeep(".,;:\/?!#$%()=+-*""'–_`<>&^@{}[]|~'".ToArray())).[Select](Function(o) o.ToLower())
                End Function

            End Class
            ''' <summary>
            ''' as is tokenizer
            ''' </summary>
            Public MustInherit Class UncasedTokenizer
                Inherits Bert_Base
                ''' <summary>
                ''' Which Subtokenizer Pretrained . Will be uncased is case inn sensitive
                ''' </summary>
                ''' <param name="Model"></param>
                Public Sub New(ByRef Model As BertModel)
                    MyBase.New(Model)
                End Sub


                ''' <summary>
                ''' Implements uncased tokenizations dictionary
                ''' </summary>
                ''' <param name="text"></param>
                ''' <returns></returns>
                Protected Overrides Function TokenizeSentence(ByVal text As String) As IEnumerable(Of String)
                    Return text.Split(New String() {" ", "   ", vbCrLf}, StringSplitOptions.None).SelectMany(Function(o) o.SplitAndKeep(".,;:\/?!#$%()=+-*""'–_`<>&^@{}[]|~'".ToArray()))
                End Function
            End Class
        End Namespace

        Public Class Bert
            ''' <summary>
            ''' Used as input to bert llms
            ''' </summary>
            Public Class ModelInput

                Public AttentionMask As List(Of Integer)
                Public InputIds As List(Of Double)
                Public TypeIds As List(Of Integer)
            End Class
            Public Class Tokens
                Public Const Classification As String = "[CLS]"
                Public Const Mask As String = "[MASK]"
                Public Const Padding As String = ""
                Public Const Separation As String = "[SEP]"
                Public Const Unknown As String = "[UNK]"
            End Class
        End Class
        ''' <summary>
        ''' For customized Vocabulary files
        ''' </summary>
        Public Class BertCasedCustomVocabulary
            Inherits CasedTokenizer
            Public Sub New(ByVal vocabularyFilePath As String)
                MyBase.New(vocabularyFilePath)
            End Sub

        End Class

        ''' <summary>
        ''' Large Vocab
        ''' </summary>
        Public Class BertCasedLargeTokenizer
            Inherits CasedTokenizer
            Public Sub New()
                MyBase.New(BertModel.CasedLarge)
            End Sub
        End Class

        Public Class BertGermanTokenizer
            Inherits CasedTokenizer
            Public Sub New()
                MyBase.New(BertModel.German)
            End Sub
        End Class

        ''' <summary>
        ''' MultiLingual Vocabulary
        ''' </summary>
        Public Class BertMultilingualTokenizer
            Inherits CasedTokenizer
            Public Sub New()
                MyBase.New(BertModel.Multilingual)
            End Sub
        End Class

        ''' <summary>
        ''' For customized Vocabulary files
        ''' </summary>
        Public Class BertUnCasedCustomVocabulary
            Inherits UncasedTokenizer
            ''' <summary>
            ''' Custom Vocab
            ''' </summary>
            ''' <param name="vocabularyFilePath"></param>
            Public Sub New(ByVal vocabularyFilePath As String)
                MyBase.New(vocabularyFilePath)
            End Sub

        End Class
        ''' <summary>
        ''' Large Vocab
        ''' </summary>
        Public Class BertUncasedLargeTokenizer
            Inherits UncasedTokenizer

            Public Sub New()
                MyBase.New(BertModel.UncasedLarge)
            End Sub


        End Class
    End Namespace
    <Serializable>
    Public Class LegacyTokenizer

        Inherits HybridTokenizer



        Private Shared ReadOnly AlphaBet() As String = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
            "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
            "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}

        Private Shared ReadOnly Number() As String = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
        "30", "40", "50", "60", "70", "80", "90", "00", "000", "0000", "00000", "000000", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
        "nineteen", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "hundred", "thousand", "million", "Billion"}

        Public Shared Function GetTokenType(ByRef CharStr As String) As TokenType
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

        Public Shared Function GetValidTokens(ByRef InputStr As String) As String
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

        Public Function GetEncapsulated(ByRef Userinput As String) As List(Of String)
            GetEncapsulated = New List(Of String)
            Do Until ContainsEncapsulated(Userinput) = False
                GetEncapsulated.Add(ExtractEncapsulated(Userinput))
            Loop
        End Function

        ''' <summary>
        ''' Pure basic Tokenizer to Tokens
        ''' </summary>
        ''' <param name="Corpus"></param>
        ''' <param name="tokenizationOption">Type Of Tokenization</param>
        ''' <returns></returns>
        Public Function Tokenize(ByRef Corpus As List(Of String), tokenizationOption As TokenizerType) As List(Of Token)
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
        Private Shared Function AddSuffix(ByRef Str As String, ByVal Suffix As String) As String
            Return Str & Suffix
        End Function
    End Class
    Public Class BytePairEncoding
        Inherits Tokenizer
        Public MaxMergeOperations As Integer = 1000
        Public Overloads Function Tokenize(Corpus As List(Of String)) As List(Of String)
            Dim tokens As New List(Of String)
            Dim Subword As String = ""

            Dim UnknownDocs As New List(Of String)
            'SubDoc Vocabulary Tokenizer
            For Each doc In Corpus
                For i = 0 To doc.Count - 1
                    Subword &= doc(i)
                    If _vocabularyDict.ContainsKey(Subword.ToLower()) Then
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
                    Dim Para As List(Of String) = TokenizeToParagraph(doc)
                    For Each item In Para
                        Subword = ""

                        Subword += item
                        If _vocabularyDict.ContainsKey(Subword.ToLower) Then
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
                    Dim Sents As List(Of String) = TokenizeToSentence(sent)


                    For Each item In Sents
                        Subword = ""

                        Subword += item
                        If _vocabularyDict.ContainsKey(Subword.ToLower) Then
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
                    Dim Words As List(Of String) = TokenizeToWord(Word)
                    For Each item In Words
                        Subword = ""

                        Subword += item
                        If _vocabularyDict.ContainsKey(Subword.ToLower) Then
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
                    Dim Chars As List(Of String) = TokenizeToCharacter(iChar)
                    For Each item In Chars
                        Subword = ""

                        Subword += item
                        If _vocabularyDict.ContainsKey(Subword.ToLower) Then
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
                _vocabularyDict.Add(unkChar, 1)
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

        Public Overrides Function Tokenize(Document As String) As List(Of String)
            Dim characterLevelVocabulary As New Dictionary(Of String, Integer)


            For Each character As Char In Document
                Dim subword As String = character.ToString()

                If characterLevelVocabulary.ContainsKey(subword) Then
                    characterLevelVocabulary(subword) += 1
                Else
                    characterLevelVocabulary.Add(subword, 1)
                End If
            Next


            Return getTokenlist(characterLevelVocabulary)
        End Function

        Public Overrides Sub Train(ByRef Corpus As List(Of String))
            For Each item In Corpus
                ' Tokenize the corpus at the character level to get the initial vocabulary
                Dim characterLevelVocabulary As Dictionary(Of String, Integer) = Train(item)

                ' Merge the most frequent pairs of subwords iteratively
                For i As Integer = 0 To MaxMergeOperations - 1
                    Dim mostFrequentPair As SubWord_Pair = FindMostFrequentPair(characterLevelVocabulary)
                    If mostFrequentPair Is Nothing Then
                        Exit For
                    End If

                    Dim newSubword As String = mostFrequentPair.Sub_word_1 + mostFrequentPair.Sub_Word_2
                    characterLevelVocabulary = MergeSubwordPair(characterLevelVocabulary, mostFrequentPair, newSubword)
                Next
                For Each Entry In characterLevelVocabulary

                    UpdateVocabulary(_vocabularyDict, Entry.Key)
                Next


            Next




        End Sub
        Public Overloads Function Train(Document As String) As Dictionary(Of String, Integer)
            Dim characterLevelVocabulary As New Dictionary(Of String, Integer)


            For Each character As Char In Document
                Dim subword As String = character.ToString()

                If characterLevelVocabulary.ContainsKey(subword) Then
                    characterLevelVocabulary(subword) += 1
                Else
                    characterLevelVocabulary.Add(subword, 1)
                End If
            Next


            Return characterLevelVocabulary
        End Function
    End Class
    Public Class HybridTokenizer
        Inherits Tokenizer
        Public maxSubwordLength As Integer = 5
        Public MaxVocabSize As Integer = 1000000
        Private ReadOnly unkToken As String = "<Unk>"
        Public Property MaxMergeOperations As Integer = 1000
        Public ReadOnly Property PairFrequencies As Dictionary(Of String, Integer) = ComputePairFrequencies()
        Public Sub Initialize_Vocabulary(initialVocabulary As List(Of String), n As Integer)

            For Each word In initialVocabulary
                For i As Integer = 0 To word.Length - n
                    UpdateVocabulary(word.Substring(i, n))
                Next
            Next

        End Sub
        Public Sub Prune(pruningThreshold As Integer)
            Dim Pruner As New VocabularyPruner(MaxVocabSize, _vocabularyDict, pruningThreshold)

            If _vocabulary.Count > pruningThreshold Then
                _vocabularyDict = Pruner.Prune()
            End If

        End Sub
        ''' <summary>
        ''' a Sub/Word Tokenizer ; Unknowns are handled by BitWordEncoding if possible
        ''' a BytePairEncoding is used for fully unknown Tokens
        ''' </summary>
        ''' <param name="text"></param>
        ''' <returns></returns>
        Public Overrides Function Tokenize(text As String) As List(Of String)
            Dim Words = Tokenizer.TokenizeToWord(text)
            Dim Tokens As New List(Of String)
            For Each item In Words
                Tokens.AddRange(TokenizeWordPiece(item))
            Next
            Return Tokens
        End Function
        Public Overrides Sub Train(ByRef Corpus As List(Of String))
            For Each item In Corpus
                Train(item)
            Next
        End Sub
        Public Overloads Sub Train(text As String)
            ' Tokenize the text into individual characters

            Dim Bits As List(Of String) = TokenizeBitWord(text)
            For Each bit As String In Bits
                UpdateVocabulary(bit)
            Next


            ' Train BPE using merging strategy
            For mergeIndex As Integer = 0 To MaxMergeOperations - 1
                MergeMostFrequentBigram()
                MergeMostFrequentPair(FindMostFrequentPair.Key)
            Next

            Prune(1)
        End Sub
        Public Sub UpdateFrequencyDictionary(mergedSubword As String)
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
        Private Function ComputePairFrequencies() As Dictionary(Of String, Integer)
            Dim pairFrequencies As Dictionary(Of String, Integer) = New Dictionary(Of String, Integer)

            For Each token As String In _vocabularyDict.Keys
                Dim tokenChars As List(Of Char) = token.ToList()

                For i As Integer = 0 To tokenChars.Count - 2
                    Dim pair As String = tokenChars(i) & tokenChars(i + 1)

                    If Not pairFrequencies.ContainsKey(pair) Then
                        pairFrequencies.Add(pair, _vocabularyDict(token))
                    Else
                        Dim value = pairFrequencies(pair)
                        value += _vocabularyDict(token)
                        pairFrequencies.Remove(pair)
                        pairFrequencies.Add(pair, value)


                    End If
                Next
            Next

            Return pairFrequencies
        End Function
        Private Overloads Function FindMostFrequentPair() As KeyValuePair(Of String, Integer)
            ' Find the most frequent character pair from the frequency counts.
            Return PairFrequencies.Aggregate(Function(x, y) If(x.Value > y.Value, x, y))
        End Function
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
        Private Sub MergeMostFrequentBigram()
            Dim mostFrequentBigram As String = GetMostFrequentBigram()
            If mostFrequentBigram IsNot Nothing Then
                Dim mergedSubword As String = mostFrequentBigram.Replace("", " ")

                UpdateVocabulary(mergedSubword)

            End If
        End Sub
        Private Sub MergeMostFrequentPair(pair As String)
            ' Merge the most frequent character pair into a new subword unit.
            Dim mergedToken As String = pair.Replace(" ", "##")
            UpdateVocabulary(mergedToken)

        End Sub
        ''' <summary>
        ''' Used for Character level Tokenization
        ''' </summary>
        ''' <param name="subword"></param>
        ''' <returns></returns>
        Private Function TokenizeBitWord(subword As String) As List(Of String)
            Dim wordPieceTokens As New List(Of String)
            Dim startIdx As Integer = 0

            While startIdx < subword.Length
                Dim endIdx As Integer = subword.Length
                Dim foundSubword As Boolean = False

                While startIdx < endIdx
                    Dim candidate As String = subword.Substring(startIdx, endIdx - startIdx)
                    Dim isLast = endIdx = subword.Length

                    If _vocabularyDict.Keys.Contains(candidate) OrElse isLast Then
                        wordPieceTokens.Add(candidate)
                        startIdx = endIdx
                        foundSubword = True
                        Exit While
                    End If

                    endIdx -= 1
                End While

                ' If no subword from the vocabulary matches, break the subword into smaller parts
                If Not foundSubword Then
                    'We Replace this code so not to return unknown tokens
                    'wordPieceTokens.Add("<unk>")
                    wordPieceTokens.AddRange(TokenizeBPE(foundSubword))
                    startIdx += 1
                End If
            End While

            Return wordPieceTokens
        End Function
        ''' <summary>
        ''' Byte Pair Encoding
        ''' </summary>
        ''' <param name="text"></param>
        ''' <returns></returns>
        Private Function TokenizeBPE(ByVal text As String) As List(Of String)
            Dim tokens As New List(Of String)

            While text.Length > 0
                Dim foundToken As Boolean = False

                ' Find the longest token in the vocabulary that matches the start of the text
                For Each subword In _vocabularyDict.OrderByDescending(Function(x) x.Key.Length)
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
                        If _vocabularyDict.Keys(subword) Then
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
        Private Function TokenizeWordPiece(text As String) As List(Of String)
            Dim tokens As New List(Of String)
            Dim pos As Integer = 0

            While pos < text.Length
                Dim foundSubword As Boolean = False
                Dim subword As String = ""

                For subwordLen As Integer = Math.Min(maxSubwordLength, text.Length - pos) To 1 Step -1
                    subword = text.Substring(pos, subwordLen)

                    If _vocabularyDict.Keys.Contains(subword) Then
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
    End Class
    Public Class TokenEncoder
        Inherits Tokenizer
        Private nextId As Integer = 0
        Private TokenToID As New Dictionary(Of String, Integer)
        Private IDToTokens As New Dictionary(Of Integer, String)

        ''' <summary>
        ''' Given  a Set of Token ID Decode the Tokens 
        ''' </summary>
        ''' <param name="tokenIds"></param>
        ''' <returns></returns>
        Public Function Decode(tokenIds As List(Of Integer)) As String
            Dim tokens As New List(Of String)

            For Each tokenId As Integer In tokenIds
                tokens.Add(IDToTokens(tokenId))
            Next

            Return String.Join(" ", tokens)
        End Function

        ''' <summary>
        ''' Pure Tokenizer (will tokenize based on the Tokenizer model settings)
        ''' </summary>
        ''' <param name="Doc"></param>
        ''' <returns></returns>
        Public Shadows Function Encode(Doc As String) As List(Of Integer)
            Dim tokens = LegacyTokenizer.TokenizeByWord(Doc)
            Dim tokenIds As New List(Of Integer)

            For Each itoken In tokens
                Dim tokenId As Integer
                If TokenToID.ContainsKey(itoken.Value) Then
                    tokenId = TokenToID(itoken.Value)
                Else
                    'Not registered

                    tokenId = TokenToID(itoken.Value)

                End If
                tokenIds.Add(tokenId)

            Next

            Return tokenIds
        End Function
        Public Overrides Function Tokenize(text As String) As List(Of String)
            Dim lst As New List(Of String)
            For Each item In Encode(text)
                lst.Add(item)
            Next
            Return lst
        End Function

        Public Shadows Sub UpdateVocabulary(Token As String)

            If Not _vocabularyDict.ContainsKey(Token) Then
                _vocabularyDict(Token) = nextId
                nextId += 1
                TokenToID = _vocabularyDict.ToDictionary(Function(x) x.Key, Function(x) x.Value)
                IDToTokens = TokenToID.ToDictionary(Function(x) x.Value, Function(x) x.Key)
            End If


        End Sub
        ''' <summary>
        ''' unused
        ''' </summary>
        ''' <param name="Corpus"></param>
        Public Overrides Sub Train(ByRef Corpus As List(Of String))
            Throw New NotImplementedException()
        End Sub
    End Class
    Public Class SubWord_Pair
        Public Sub New(Sub_word_1 As String, Sub_Word_2 As String, frequency As Integer)
            Me.Sub_word_1 = Sub_word_1
            Me.Sub_Word_2 = Sub_Word_2
            Me.Frequency = frequency
        End Sub

        Public Property Frequency As Integer
        Public Property Sub_word_1 As String
        Public Property Sub_Word_2 As String
    End Class
End Namespace
Public Module TokenizerExtensions
    Public Function LoadVocabularyList() As List(Of String)
        Dim openFileDialog As New OpenFileDialog()
        Dim Vocabulary As New List(Of String)
        openFileDialog.Filter = "txt Files|*.txt"
        openFileDialog.Title = "Select a Vocabulary list"

        If openFileDialog.ShowDialog() = DialogResult.OK Then
            Dim selectedFilePath As String = openFileDialog.FileName


            Vocabulary = LoadListFromFile(selectedFilePath)

        End If
        Return Vocabulary
    End Function
    Public Function LoadCorpusFromDirectory(directoryPath As String) As List(Of String)
        Dim textList As New List(Of String)

        Try
            If Directory.Exists(directoryPath) Then
                Dim textFiles As String() = Directory.GetFiles(directoryPath, "*.txt")

                For Each filePath As String In textFiles
                    Dim text As String = File.ReadAllText(filePath)

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

    Public Function LoadTextFilesFromDirectory(directoryPath As String) As List(Of String)
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

    Public Function LoadTextFromFile(filePath As String) As String
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

    Public Sub SaveTextsToFiles(textStrings As List(Of String), fileNames As List(Of String), directoryPath As String)
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

    Public Sub SaveTextToFile(text As String, filePath As String)
        Try
            File.WriteAllText(filePath, text)
            MessageBox.Show("Text saved to file: " & filePath, "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Catch ex As Exception
            MessageBox.Show("An error occurred: " & ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub


    <Serializable>
    Public Class Token
        Private iStopWords As List(Of String)

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
        Public Property EndPosition As Integer
        Public Property StartPosition As Integer
        Public Property StopWordRemovalEnabled As Boolean
        Public Property StopWords As List(Of String)
            Get
                Return iStopWords
            End Get
            Set(value As List(Of String))
                iStopWords = value
            End Set
        End Property

        Public Property Type As TokenType
        Public Property Value As String
        Private Function RemoveStopWords(ByVal tokens As List(Of Token)) As List(Of Token)
            Return tokens.Where(Function(token) Not StopWords.Contains(token.Value)).ToList()
        End Function
    End Class
    Public Enum TokenizerType
        _Char
        _Word
        _Sentence
        _Paragraph
        _BytePairEncoding
        _Hybrid
        _TokenEncoder
        _Legacy
        _BertCased
        _BertUnCased
        _BertMultilingual
    End Enum
    Public Function CalculateWordPieceFrequency(ByVal subword As String, ByVal mergedWord As String) As Integer
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
    <Runtime.CompilerServices.Extension()>
    Public Function FindFrequentCharacterBigrams(Vocab As List(Of String), ByRef Freq_Threshold As Integer) As List(Of String)
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

    <Extension>
    Public Function GetHighFreqLst(ByRef Vocabulary As Dictionary(Of String, Integer), ByRef Threshold As Integer) As List(Of String)
        Dim HighFreq As New List(Of String)
        For Each item In Vocabulary
            If item.Value > Threshold Then
                HighFreq.Add(item.Key)
            End If
        Next
        Return HighFreq
    End Function

    <Extension>
    Public Function GetVocabularyLst(ByRef Vocabulary As Dictionary(Of String, Integer)) As List(Of String)
        Return Vocabulary.Keys.ToList()
    End Function
    Public Function InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
        target = value
        Return value
    End Function

    Public Function LoadListFromFile(filePath As String) As List(Of String)
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

    <Extension>
    Public Sub ModelExporter(ByRef Model As Object, Filename As String)
        Dim path As String = Application.StartupPath

        Dim FileStream As New System.IO.FileStream(Filename, System.IO.FileMode.CreateNew)
        Dim Formatter As New BinaryFormatter
        Formatter.Serialize(Model, FileStream)
        FileStream.Close()


    End Sub

    <Extension>
    Public Sub ModelExporter(ByRef Model As Dictionary(Of String, Integer), Filename As String)
        Try
            File.WriteAllLines(Filename, GetVocabularyLst(Model))
            MessageBox.Show("Text saved to file: " & Filename, "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Catch ex As Exception
            MessageBox.Show("An error occurred: " & ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    <Extension>
    Public Function ModelImporter(ByRef Filename As String, ByRef Model As Object) As Object
        Dim FileStream As New System.IO.FileStream(Filename, System.IO.FileMode.Open)
        Dim Formatter As New BinaryFormatter
        Model = Formatter.Deserialize(FileStream)
        FileStream.Close()

        Return Model
    End Function
    <Runtime.CompilerServices.Extension()>
    Public Sub SaveListToFile(strings As List(Of String), filePath As String)
        Try
            File.WriteAllLines(filePath, strings)
            Console.WriteLine("Strings saved to file: " & filePath)
        Catch ex As Exception
            Console.WriteLine("An error occurred: " & ex.Message)
        End Try
    End Sub

    <Runtime.CompilerServices.Extension()>
    Public Function SpaceItems(ByRef txt As String, Item As String) As String
        Return txt.Replace(Item, " " & Item & " ")
    End Function

    <Runtime.CompilerServices.Extension()>
    Public Function SpacePunctuation(ByRef Txt As String) As String
        For Each item In PunctuationMarkers.Punctuation
            Txt = SpaceItems(Txt, item)
        Next

        Return Txt
    End Function

    <Extension()>
    Public Iterator Function SplitAndKeep(
        ByVal inputString As String, ByVal ParamArray delimiters As Char()) As IEnumerable(Of String)
        Dim start As Integer = 0
        Dim index As Integer

        While (InlineAssignHelper(index, inputString.IndexOfAny(delimiters, start))) <> -1
            If index - start > 0 Then
                Yield inputString.Substring(start, index - start)
            End If

            Yield inputString.Substring(index, 1)

            start = index + 1
        End While

        If start < inputString.Length Then
            Yield inputString.Substring(start)
        End If
    End Function

    <Extension>
    Public Function UpdateCorpusWithMergedToken(ByRef corpus As List(Of String), pair As String) As List(Of String)
        ' Update the text corpus with the merged token for the next iteration.
        Return corpus.ConvertAll(Function(text) text.Replace(pair, pair.Replace(" ", "_")))
    End Function

    <Extension>
    Public Function UpdateVocabulary(vocabulary As Dictionary(Of String, Integer), Term As String) As Dictionary(Of String, Integer)
        If vocabulary(Term) > 0 Then
            Dim Freq As Integer = vocabulary(Term)
            Freq += 1
            vocabulary.Remove(Term)
            vocabulary.Add(Term, Freq)
        Else
            vocabulary.Add(Term, 1)
        End If
        Return vocabulary
    End Function

    <Serializable>
    Public Class PunctuationMarkers
        Public Shared ReadOnly CodePunctuation() As String = {"\", "#", "@", "^"}
        Public Shared ReadOnly Delimiters() As Char = {CType(" ", Char), CType(".", Char),
                    CType(",", Char), CType("?", Char),
                    CType("!", Char), CType(";", Char),
                    CType(":", Char), Chr(10), Chr(13), vbTab}

        Public Shared ReadOnly EncapuslationPunctuationEnd() As String = {"}", "]", ">", ")"}
        Public Shared ReadOnly EncapuslationPunctuationStart() As String = {"{", "[", "<", "("}
        Public Shared ReadOnly GramaticalPunctuation() As String = {".", "?", "!", ":", ";", ","}
        Public Shared ReadOnly MathPunctuation = New String() {"+", "-", "*", "/", "=", "<", ">", "≤", "≥", "±", "≈", "≠", "%", "‰", "‱", "^", "_", "√", "∛", "∜", "∫", "∬", "∭", "∮", "∯", "∰", "∇", "∂", "∆", "∏", "∑", "∐", "⨀", "⨁", "⨂", "⨃", "⨄", "∫", "∬", "∭", "∮", "∯", "∰", "∇", "∂", "∆", "∏", "∑", "∐", "⨀", "⨁", "⨂", "⨃", "⨄"}
        Public Shared ReadOnly MoneyPunctuation() As String = {"$", "€", "£", "¥", "₹", "₽", "₿"}
        Public Shared ReadOnly SeperatorPunctuation() As String = {" ", ",", "|"}
        Public Shared ReadOnly Symbols() As String = {"@", "#", "$", "%", "&", "*", "+", "=", "^", "_", "~", "§", "°", "¿", "¡"}
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

        Public ReadOnly Property SentenceEndPunctuation As List(Of String)
            Get
                Dim markers() As String = {".", ";", ":", "!", "?"}
                Return markers.ToList
            End Get
        End Property
    End Class

    Public Class ResourceExtractor
        Public Shared Sub ExtractResourceToFile(resourceName As String, outputPath As String)
            Using stream As Stream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName)
                If stream IsNot Nothing Then
                    Using fileStream As New FileStream(outputPath, FileMode.Create)
                        stream.CopyTo(fileStream)
                    End Using
                Else
                    Console.WriteLine("Resource not found: " & resourceName)
                End If
            End Using
        End Sub
    End Class
    Public Class VocabularyPruner
        Public Sub New(maxVocab As Integer, vocabulary As Dictionary(Of String, Integer), lowestVocabularyFreq As Integer)
            If vocabulary Is Nothing Then
                Throw New ArgumentNullException(NameOf(vocabulary))
            End If

            Me.MaxVocab = maxVocab
            Me.Vocabulary = vocabulary
            Me.LowestVocabularyFreq = lowestVocabularyFreq
        End Sub

        Public Property LowestVocabularyFreq As Integer = 1

        ''' <summary>
        ''' Defines max entries in vocabulary before Pruning Rare Words
        ''' </summary>
        ''' <returns></returns>
        Public Property MaxVocab As Integer = 100000
        Public Property Vocabulary As New Dictionary(Of String, Integer)
        Public Function Prune() As Dictionary(Of String, Integer)


            If Vocabulary.Count > MaxVocab Then
                PruneVocabulary()
            End If
            Return Vocabulary
        End Function

        Private Sub PruneVocabulary()
            ' Create a list to store tokens to be removed.
            Dim tokensToRemove As New List(Of String)

            ' Iterate through the vocabulary and identify tokens to prune.
            For Each token In Vocabulary
                Dim tokenId As Integer = token.Value
                Dim tokenFrequency As Integer = Vocabulary(token.Key)

                ' Prune the token if it has frequency below the threshold (1) and is not recent (has a lower ID).
                If tokenFrequency <= LowestVocabularyFreq AndAlso tokenId < Vocabulary.Count - 1 Then
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
    End Class
End Module
