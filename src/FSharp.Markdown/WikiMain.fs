namespace FSharp.Markdown

open System
open System.IO
open System.Collections.Generic

open FSharp.Patterns
open FSharp.Markdown.Wiki
open FSharp.Markdown.Html

/// Representation of a Markdown document - the representation of Paragraphs
/// uses an F# discriminated union type and so is best used from F#.
type WikiDocument(paragraphs, links) =
  /// Returns a list of paragraphs in the document
  member x.Paragraphs : MarkdownParagraphs = paragraphs
  /// Returns a dictionary containing explicitly defined links
  member x.DefinedLinks : IDictionary<string, string * option<string>> = links


type WikiMarkup =
  /// Parse the specified text into a MarkdownDocument. Line breaks in the
  /// inline HTML (etc.) will be stored using the specified string.
  static member Parse(text, newline) =
    use reader = new StringReader(text)
    let lines = 
      [ let line = ref ""
        while (line := reader.ReadLine(); line.Value <> null) do
          yield line.Value
        if text.EndsWith(newline) then
          yield "" ]
      //|> Utils.replaceTabs 4
    let links = Dictionary<_, _>()
    //let (Lines.TrimBlank lines) = lines
    let ctx : ParsingContext = { Newline = newline; Links = links }
    let paragraphs =
      lines
      |> FSharp.Collections.List.skipWhile String.IsNullOrWhiteSpace
      |> parseParagraphsWiki ctx
      |> List.ofSeq
    WikiDocument(paragraphs, links)

  /// Parse the specified text into a MarkdownDocument.
  static member Parse(text) =
    WikiMarkup.Parse(text, Environment.NewLine)

  /// Transform Markdown document into HTML format. The result
  /// will be written to the provided TextWriter.
  static member TransformHtml(text, writer:TextWriter, newline) = 
    let doc = WikiMarkup.Parse(text, newline)
    formatMarkdown writer false newline false doc.DefinedLinks doc.Paragraphs 

  /// Transform Markdown document into HTML format. The result
  /// will be written to the provided TextWriter.
  static member TransformHtml(text, writer:TextWriter) = 
    WikiMarkup.TransformHtml(text, writer, Environment.NewLine)

  /// Transform Markdown document into HTML format. 
  /// The result will be returned as a string.
  static member TransformHtml(text, newline) =
    let sb = new System.Text.StringBuilder()
    use wr = new StringWriter(sb)
    WikiMarkup.TransformHtml(text, wr, newline)
    sb.ToString()

  /// Transform Markdown document into HTML format. 
  /// The result will be returned as a string.
  static member TransformHtml(text) =
    WikiMarkup.TransformHtml(text, Environment.NewLine)