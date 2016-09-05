module FSharp.WikiMarkup.Tests.Parsing

open FsUnit
open NUnit.Framework
open FSharp.Markdown

let properNewLines (text: string) = text.Replace("\r\n", System.Environment.NewLine)

let shouldEqualNoWhiteSpace (x:string) (y:string) =
    shouldEqual (x.Split()) (y.Split())

[<Test>]
let ``Transform header 1 correctly``() =
    let doc = "h1. Header 1\r\n";
    let expected = "<h1>Header 1</h1>\r\n" |> properNewLines
    WikiMarkup.TransformHtml doc
    |> shouldEqual expected

[<Test>]
let ``Transform sample correctly``() =
    let doc = """h1. User Story:

As a business owner I would like to track _signup conversions_ in GA, but the tracking doesn't include the CP side of signup, so that I cannot setup the funnel.

h2. Acceptance Criteria:
* ACC1) The *Google Analytics* tracking share-it landing page (UA-37872710-3) includes the Control Panel pages of the publisher signup: Initial CP signup page, Create Product, Configure payout, Accept contract, CP Overview
* ACC2) The conversion goal for the tracking is a completed signup (CP Overview page, publisher status REV)

----

||Column 1||Column 2||
| Data     | Text    |
| Second   | Line    |

End.
"""
    let expected = "<h1>Header 1</h1>\r\n" |> properNewLines
    let html = WikiMarkup.TransformHtml doc
    Assert.IsNotEmpty html
