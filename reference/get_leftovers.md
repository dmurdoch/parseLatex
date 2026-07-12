# Retrieve source from beyond the end of the document.

Retrieve source from beyond the end of the document.

## Usage

``` r
get_leftovers(text, items = parseLatex(text))
```

## Arguments

- text:

  Character vector holding source.

- items:

  Parsed version of `text`.

## Value

The part of `text` that follows `\end{document}` other than a single
newline, named according to the original line numbers.

## Note

The line numbering in the output matches what a text editor would see;
embedded newlines in `text` will result in separate lines in the output.

## Examples

``` r
# line:  1                  2                3
text <- "\\begin{document}\n\\end{document}\nnotes"
get_leftovers(text)
#>       3 
#> "notes" 
```
