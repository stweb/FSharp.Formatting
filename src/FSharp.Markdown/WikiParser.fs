// --------------------------------------------------------------------------------------
// F# Wiki Markup Parser (Confluence Style) 
// by Stefan Weber, 2016, Available under Apache 2.0 license.
// https://confluence.atlassian.com/doc/confluence-wiki-markup-251003035.html
// --------------------------------------------------------------------------------------
module internal FSharp.Markdown.Wiki

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open FSharp.Patterns
open FSharp.Collections
open FSharp.Markdown


let inline toString chars = System.String(chars |> Array.ofList)

/// Succeeds when the specified character list starts with an escaped
/// character - in that case, returns the character and the tail of the list
let inline (|EscapedChar|_|) input =
  match input with
  | '\\'::( ( '*' | '\\' | '`' | '_' | '{' | '}' | '[' | ']'
            | '(' | ')' | '>' | '#' | '.' | '!' | '+' | '-' | '$') as c) ::rest -> Some(c, rest)
  | _ -> None

/// Escape dollar inside a LaTex inline math span.
let inline (|EscapedLatexInlineMathChar|_|) input =
  match input with
  | '\\'::( ('$') as c) :: rest -> Some(c, rest)
  | _ -> None

/// Matches a list if it starts with a sub-list that is delimited
/// using the specified delimiters. Returns a wrapped list and the rest.
///
/// This is similar to `List.Delimited`, but it skips over escaped characters.
let (|DelimitedMarkdown|_|) bracket input =
  let startl, endl = bracket, bracket
  // Like List.partitionUntilEquals, but skip over escaped characters
  let rec loop acc = function
    | EscapedChar(x, xs) -> loop (x::'\\'::acc) xs
    | input when List.startsWith endl input -> Some(List.rev acc, input)
    | x::xs -> loop (x::acc) xs
    | [] -> None
  // If it starts with 'startl', let's search for 'endl'
  if List.startsWith bracket input then
    match loop [] (List.skip bracket.Length input) with
    | Some(pre, post) -> Some(pre, List.skip bracket.Length post)
    | None -> None
  else None

/// Recognizes some form of emphasis using `**bold**` or `*italic*`
/// (both can be also marked using underscore).
/// TODO: This does not handle nested emphasis well.
let (|Emphasised|_|) = function
  | (('_' | '*') :: tail) as input ->
    match input with
    | DelimitedMarkdown ['*'] (body, rest) ->
        Some(body, Strong, rest)
    | DelimitedMarkdown ['_'] (body, rest) ->
        Some(body, Emphasis, rest)
    | _ -> None
  | _ -> None

/// Parses a body of a paragraph and recognizes all inline tags.
let rec parseChars acc input = seq {
  // Zero or one literals, depending whether there is some accumulated input
  let accLiterals = Lazy.Create(fun () ->
    if List.isEmpty acc then []
    else [Literal(String(List.rev acc |> Array.ofList))] )

  match input with
  // Recognizes explicit line-break at the end of line
  | ' '::' '::'\r'::'\n'::rest
  | ' '::' '::('\n' | '\r')::rest ->
      yield! accLiterals.Value
      yield HardLineBreak
      yield! parseChars [] rest

  // Encode & as an HTML entity
  | '&'::'a'::'m'::'p'::';'::rest
  | '&'::rest ->
      yield! parseChars (';'::'p'::'m'::'a'::'&'::acc) rest
(*

  // Ignore escaped characters that might mean something else
  | EscapedChar(c, rest) ->
      yield! parseChars (c::acc) rest

  // Inline code delimited either using double `` or single `
  // (if there are spaces around, then body can contain more backticks)
  | List.DelimitedWith ['`'; ' '] [' '; '`'] (body, rest)
  | List.DelimitedNTimes '`' (body, rest) ->
      yield! accLiterals.Value
      yield InlineCode(String(Array.ofList body).Trim())
      yield! parseChars [] rest

  // Recognize direct link [foo](http://bar) or indirect link [foo][bar] or auto link http://bar
  | DirectLink (body, link, rest) ->
      yield! accLiterals.Value
      let info = getLinkAndTitle (String(Array.ofList link))
      yield DirectLink(parseChars [] body |> List.ofSeq, info)
      yield! parseChars [] rest
  | IndirectLink(body, link, original, rest) ->
      yield! accLiterals.Value
      let key = if String.IsNullOrEmpty(link) then String(body |> Array.ofSeq) else link
      yield IndirectLink(parseChars [] body |> List.ofSeq, original, key)
      yield! parseChars [] rest
  | AutoLink (link, rest) ->
      yield! accLiterals.Value
      yield DirectLink([Literal link], (link, None))
      yield! parseChars [] rest

  // Recognize image - this is a link prefixed with the '!' symbol
  | '!'::DirectLink (body, link, rest) ->
      yield! accLiterals.Value
      yield DirectImage(String(Array.ofList body), getLinkAndTitle (String(Array.ofList link)))
      yield! parseChars [] rest
  | '!'::IndirectLink(body, link, original, rest) ->
      yield! accLiterals.Value
      let key = if String.IsNullOrEmpty(link) then String(body |> Array.ofSeq) else link
      yield IndirectImage(String(Array.ofList body), original, key)
      yield! parseChars [] rest
*)
  // Handle emphasised text
  | Emphasised (body, f, rest) ->
      yield! accLiterals.Value
      let body = parseChars [] body |> List.ofSeq
      yield f(body)
      yield! parseChars [] rest
  // Encode '<' char if it is not link or inline HTML
  | '<'::rest ->
      yield! parseChars (';'::'t'::'l'::'&'::acc) rest
  | '>'::rest ->
      yield! parseChars (';'::'t'::'g'::'&'::acc) rest
  | x::xs ->
      yield! parseChars (x::acc) xs
  | [] ->
      yield! accLiterals.Value }



/// Parse body of a paragraph into a list of Markdown inline spans
let parseSpans (String.TrimBoth s) =
  parseChars [] (s.ToCharArray() |> List.ofArray) |> List.ofSeq

/// Recognizes heading h1. through h6.
let (|Heading|_|) (str: string) = 
  match str |> List.ofSeq with    
  | 'h' :: n :: '.' :: rest when Char.IsDigit n -> Some(int n - 48, toString rest)
  | _ -> None

/// Recognizes a start of a blockquote
let (|BlockquoteStart|_|) (line:string) =
  let regex =
    "^ {0,3}" // Up to three leading spaces
    + ">" // Blockquote character
    + "\s?" // Maybe one whitespace character
    + "(.*)" // Capture everything else
  let match' = Regex.Match(line, regex)
  if match'.Success then Some (match'.Groups.Item(1)).Value
  else None

let rec trimSpaces numSpaces (s:string) =
  if numSpaces <= 0 then s
  elif s.StartsWith(" ") then trimSpaces (numSpaces - 1) (s.Substring(1))
  elif s.StartsWith("\t") then trimSpaces (numSpaces - 4) (s.Substring(1))
  else s


/// Takes lines that belong to a continuing paragraph until
/// a white line or start of other paragraph-item is found
let (|TakeParagraphLines|_|) input =
  match List.partitionWhileLookahead (function
//    | Heading _ -> false
//    | FencedCodeBlock _ -> false
//    | BlockquoteStart _::_ -> false
    | String.WhiteSpace::_ -> false
    | _ -> true) input with
  | matching, rest when matching <> [] -> Some(matching, rest)
  | _ -> None

/// Matches when the input starts with a number. Returns the
/// rest of the input, following the last number.
let (|SkipSomeNumbers|_|) (input:string) =
  match List.ofSeq input with
  | x::xs when Char.IsDigit x ->
      let _, rest = List.partitionUntil (Char.IsDigit >> not) xs
      Some(input.Length - rest.Length, rest)
  | _ -> None

/// Recognizes a staring of a list (either 1. or +, *, -).
/// Returns the rest of the line, together with the indent.
let (|ListStart|_|) = function
  | String.TrimStartAndCount
      (startIndent, spaces,
        // NOTE: a tab character after +, * or - isn't supported by the reference implementation
        // (it will be parsed as paragraph for 0.22)
        (String.StartsWithAny ["+ "; "* "; "- " (*; "+\t"; "*\t"; "-\t"*)] as item)) ->
      let li = item.Substring(2)
      let (String.TrimStartAndCount (startIndent2, spaces2, _)) = li
      let endIndent =
        startIndent + 2 +
        // Handle case of code block
        if startIndent2 >= 5 then 1 else startIndent2
      Some(Unordered, startIndent, endIndent, li)
  | String.TrimStartAndCount // Remove leading spaces
      (startIndent, spaces,
       (SkipSomeNumbers // read a number
          (skipNumCount, '.' :: ' ' :: List.AsString item))) ->
      let (String.TrimStartAndCount (startIndent2, spaces2, _)) = item
      let endIndent =
        startIndent + 2 + skipNumCount +
        // Handle case of code block
        if startIndent2 >= 5 then 1 else startIndent2
      Some(Ordered, startIndent, endIndent, item)
  | _ -> None


/// Splits input into lines until whitespace or starting of a list and the rest.
let (|LinesUntilListOrWhite|) =
  List.partitionUntil (function
    | ListStart _ | String.WhiteSpace -> true | _ -> false)

/// Splits input into lines until not-indented line or starting of a list and the rest.
let (|LinesUntilListOrUnindented|) =
  List.partitionUntilLookahead (function
    | (ListStart _ | String.Unindented)::_
    | String.WhiteSpace::String.WhiteSpace::_ -> true | _ -> false)

/// Recognizes a list item until the next list item (possibly nested) or end of a list.
/// The parameter specifies whether the previous line was simple (single-line not
/// separated by a white line - simple items are not wrapped in <p>)
let (|ListItem|_|) prevSimple = function
  | ListStart(kind, startIndent, endIndent, item)::
      // Take remaining lines that belong to the same item
      // (everything until an empty line or start of another list item)
      LinesUntilListOrWhite
        (continued,
            // Take more things that belong to the item -
            // the value 'more' will contain indented paragraphs
            (LinesUntilListOrUnindented (more, rest) as next)) ->
      let simple =
        match item with
        | String.TrimStartAndCount (_, spaces, _) when spaces >= 4->
          // Code Block
          false
        | _ ->
          match next, rest with
          | String.WhiteSpace::_, (ListStart _)::_ -> false
          | (ListStart _)::_, _ -> true
          | [], _ -> true
          | [ String.WhiteSpace ], _ -> true
          | String.WhiteSpace::String.WhiteSpace::_, _ -> true
          | _, String.Unindented::_ -> prevSimple
          | _, _ -> false

      let lines =
        [ yield item
          for line in continued do
            yield line.Trim()
          for line in more do
            let trimmed = trimSpaces endIndent line
            yield trimmed ]
            //let trimmed = line.TrimStart()
            //if trimmed.Length >= line.Length - endIndent then yield trimmed
            //else yield line.Substring(endIndent) ]
      Some(startIndent, (simple, kind, lines), rest)
  | _ -> None

/// Recognizes a horizontal rule 
/// Wiki TODO: To create a horizontal line across the width of your page or content block, type four dashes (like this: ----) at the beginning of a line, then press Enter or space.
let (|HorizontalRule|_|) (line:string) =
  let rec loop ((h, a, u) as arg) i =
    if (h >= 3 || a >= 3 || u >= 3) && i = line.Length then Some(line.[0]) 
    elif i = line.Length then None
    elif Char.IsWhiteSpace line.[i] then loop arg (i + 1)
    elif line.[i] = '-' && a = 0 && u = 0 then loop (h + 1, a, u) (i + 1)
    //elif line.[i] = '*' && h = 0 && u = 0 then loop (h, a + 1, u) (i + 1)
    //elif line.[i] = '_' && a = 0 && h = 0 then loop (h, a, u + 1) (i + 1)
    else None
  loop (0, 0, 0) 0

/// Recognizes a list - returns list items with information about
/// their indents - these need to be turned into a tree structure later.
let rec (|ListItems|_|) prevSimple = function
  | ListItem prevSimple (indent, ((nextSimple, _, _) as info), rest) ->
      match rest with
      | (HorizontalRule _)::_ ->
          Some([indent, info], rest)
      | ListItems nextSimple (items, rest) ->
          Some((indent, info)::items, rest)
      | _ -> Some([indent, info], rest)
  | _ -> None

/// Defines a context for the main `parseParagraphs` function
type ParsingContext =
  { Links : Dictionary<string, string * option<string>>
    Newline : string }

/// Parse a list of lines into a sequence of markdown paragraphs
let rec parseParagraphsWiki (ctx:ParsingContext) (lines: string list) = seq {
  match lines with    
  | HorizontalRule(c) :: (Lines.TrimBlankStart lines) ->
      yield HorizontalRule(c)
      yield! parseParagraphsWiki ctx lines

  // Recognize list of list items and turn it into nested lists
  | ListItems true (items, Lines.TrimBlankStart rest) ->
      let tree = Tree.ofIndentedList items

      // Nest all items that have another kind (i.e. UL vs. OL)
      let rec nestUnmatchingItems items =
        match items with
        | Node((_, baseKind, _), _)::_ ->
            items
            |> List.nestUnderLastMatching (fun (Node((_, kind, _), _)) -> kind = baseKind)
            |> List.map (fun (Node(info, children), nested) ->
                let children = nestUnmatchingItems children
                Node(info, children @ nested))
        | [] -> []

      // Turn tree into nested list definitions
      let rec formatTree (nodes:Tree<bool * MarkdownListKind * string list> list) =
        let kind = match nodes with Node((_, kind, _), _)::_ -> kind | _ -> Unordered
        let items =
          [ for (Node((simple, _, body), nested)) in nodes ->
              [ if not simple then yield! parseParagraphsWiki ctx body
                else yield Span(parseSpans(String.concat ctx.Newline body))
                if nested <> [] then
                  yield formatTree nested ] ]
        ListBlock(kind, items)

      // Make sure all items of the list have are either simple or not.
      let rec unifySimpleProperty (nodes:Tree<bool * MarkdownListKind * string list> list) =
        let containsNonSimple =
          tree |> Seq.exists (function
            | Node ((false, _, _), _) -> true
            | _ -> false)
        if containsNonSimple then
          nodes |> List.map (function
            | Node ((_, kind, content), nested) -> Node((false, kind, content), unifySimpleProperty nested))
        else nodes

      yield  tree |> unifySimpleProperty |> formatTree
      yield! parseParagraphsWiki ctx rest
  | Heading(n, body) :: Lines.TrimBlankStart lines->
      yield Heading(n, parseSpans body)
      yield! parseParagraphsWiki ctx lines
  | TakeParagraphLines(Lines.TrimParagraphLines lines, Lines.TrimBlankStart rest) ->
        yield Paragraph (parseSpans (String.concat ctx.Newline lines))
        yield! parseParagraphsWiki ctx rest

  | Lines.TrimBlankStart [] -> ()
  | _ -> failwithf "Unexpectedly stopped!\n%A" lines }

