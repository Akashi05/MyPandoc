# MyPandoc

An Epitech project implementing a document format converter, inspired by [Pandoc](https://pandoc.org/). Written in Haskell, **mypandoc** parses structured documents and converts them between **JSON**, **XML**, and **Markdown** formats.

## Features

- Parse documents from **JSON** or **XML** input
- Convert to **JSON**, **XML**, or **Markdown** output
- Optional output to a file
- Supports rich document elements:
  - Headers (title, author, date)
  - Paragraphs, Sections (nested)
  - **Bold**, *Italic*, `Code` inline formatting
  - Code blocks
  - Lists
  - Links and Images

## Requirements

- [GHC](https://www.haskell.org/ghc/) (Haskell compiler)
- [Stack](https://docs.haskellstack.org/) (build tool)

## Build

```sh
make
```

This runs `stack build` and copies the `mypandoc` binary to the project root.

Other Makefile targets:

| Target   | Description                          |
|----------|--------------------------------------|
| `all`    | Build the project                    |
| `clean`  | Remove build artifacts               |
| `fclean` | Full clean (includes the binary)     |
| `re`     | Full clean then rebuild              |

## Usage

```
./mypandoc -i <input_file> -f <output_format> [-o <output_file>] [-e <input_format>]
```

| Flag | Description                                           |
|------|-------------------------------------------------------|
| `-i` | Path to the input file (**required**)                 |
| `-f` | Output format: `json`, `xml`, or `markdown` (**required**) |
| `-o` | Path to the output file (optional, defaults to stdout) |
| `-e` | Force input format (optional, auto-detected otherwise) |

The input format is automatically detected from the file extension (`.json` or `.xml`).

### Examples

```sh
# Convert a JSON document to Markdown
./mypandoc -i examples/syntaxe.json -f markdown

# Convert an XML document to JSON and save to a file
./mypandoc -i examples/syntaxe.xml -f json -o output.json

# Convert a JSON document to XML
./mypandoc -i examples/exemple1.json -f xml
```

## Document Format

### JSON

```json
{
  "header": {
    "title": "My Document",
    "author": "John Doe",
    "date": "2024-01-01"
  },
  "body": [
    ["A simple paragraph."],
    {
      "section": {
        "title": "Section Title",
        "content": [
          ["Paragraph with ", {"bold": "bold"}, " and ", {"italic": "italic"}, " text."],
          {"codeblock": ["Some code here."]},
          {"list": [["item 1"], ["item 2"]]},
          [{"link": {"url": "https://example.com", "content": ["click here"]}}],
          [{"image": {"url": "https://example.com/img.png", "alt": ["alt text"]}}]
        ]
      }
    }
  ]
}
```

### XML

```xml
<document>
  <header title="My Document">
    <author>John Doe</author>
    <date>2024-01-01</date>
  </header>
  <body>
    <paragraph>A simple paragraph.</paragraph>
    <section title="Section Title">
      <paragraph>Paragraph with <bold>bold</bold> and <italic>italic</italic> text.</paragraph>
      <codeblock><paragraph>Some code here.</paragraph></codeblock>
      <list>
        <paragraph>item 1</paragraph>
        <paragraph>item 2</paragraph>
      </list>
      <paragraph><link url="https://example.com">click here</link></paragraph>
      <paragraph><image url="https://example.com/img.png">alt text</image></paragraph>
    </section>
  </body>
</document>
```

### Markdown (output only)

```markdown
---
title: My Document
author: John Doe
date: 2024-01-01
---

A simple paragraph.

# Section Title

Paragraph with **bold** and *italic* text.

```
Some code here.
```

- item 1
- item 2

[click here](https://example.com)

![alt text](https://example.com/img.png)
```

## Project Structure

```
MyPandoc/
├── Makefile
├── stack.yaml
└── MYPANDOC/
    ├── app/
    │   └── Main.hs             # Entry point
    ├── src/
    │   ├── Document.hs         # Document data model
    │   ├── Errorcase.hs        # CLI parsing and orchestration
    │   ├── JsonToDoc.hs        # JSON parser & converter
    │   ├── XmlParser.hs        # XML parser
    │   ├── Xml_converter.hs    # XML converter
    │   ├── Markdown_converter.hs # Markdown converter
    │   └── Lib.hs
    ├── test/
    └── examples/               # Sample input/output files
```

## Author

Epitech Project — 2025
