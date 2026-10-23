#!/bin/sh
#
# The multidoc command line.
#
# Converts guide.sxml to several formats, and builds a small site of
# two pages, guide.sxml and Stacks.sxml, whose links to each other are
# checked and resolved.  The results go to the directory out/.
#
#   sh cli.sh
#
set -e

mkdir -p out
cp guide.sxml Stacks.sxml out/
cd out

# One page, several formats.  -t chooses the writer, -w the width, -o
# the output file.  A long option takes its value after =.
multidoc -t latex -o guide.tex guide.sxml
multidoc --to=texinfo --output=guide.texi guide.sxml
multidoc -t markdown -w 60 -o guide.md guide.sxml

# Writer options are given with -V KEY=VALUE.  Here: numbered
# sections, references by number, and a stylesheet for the HTML page.
multidoc -t html --number-sections -V ref-style=number -V style=style.css \
         -o numbered.html guide.sxml

# The page as the chapter of a book: top-level sections become
# chapters, and LaTeX uses the book class.
multidoc -t latex --top-division=chapter -V document-class=book -o book.tex guide.sxml

# A fragment, without the document header, read from standard input.
echo '(body (p "Hello, " (em "world") "."))' | multidoc -t html --fragment -

# The plain text version at 40 columns, to standard output.
multidoc -t plain -w 40 guide.sxml | head -20

# A site.  In one run, multidoc collects the targets of every page
# before writing any, so links between the pages resolve.  -V
# xref-suffix=.html makes the links point to the HTML files, and
# --xref-out saves the targets for later runs.  The link to a missing
# section is reported on standard error.
multidoc -t html --auto-ext -V xref-suffix=.html --xref-out=site.xref guide.sxml Stacks.sxml

# A later run can convert one page alone, reading the targets of the
# others with --xref-in.
multidoc -t plain -w 72 --xref-in=site.xref -o Stacks.txt Stacks.sxml

# --check reports how a writer renders the custom nodes of a page, and
# --list-formats the formats available.
multidoc -t html --check guide.sxml
multidoc --list-formats

ls
