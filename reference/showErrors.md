# Show errors in parsed Latex object

Show errors in parsed Latex object

## Usage

``` r
showErrors(
  x,
  repeatSrcline = FALSE,
  errorMsgTwice = FALSE,
  lineNumbers = TRUE,
  showAllLines = FALSE
)
```

## Arguments

- x:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object.

- repeatSrcline:

  Repeat the source line when it has multiple errors?

- errorMsgTwice:

  Show the error message at both the start and end of a multiline error?

- lineNumbers:

  Show line numbers on output?

- showAllLines:

  Show all lines whether they have errors or not?

## Value

A list of paths to errors, invisibly.

## Examples

``` r
parsed <- parseLatex("\\end{baz} \\begin{foo} \n \\begin{bar}  $1+1\n4",
                     recover = TRUE, showErrors = FALSE)
showErrors(parsed)
#> 1: \end{baz} \begin{foo} 
#>    <-->
#>    unexpected END '\end'
#>              <-----------
#> 2:  \begin{bar}  $1+1
#>     <----------------
#>                  <---
#> 3: 4
#>    ->
#>    unexpected END_OF_INPUT
#>   '$' at 2:15 is still open
#>    ->
#>    unexpected END_OF_INPUT
#>   '\begin{bar}' at 2:2 is still open
#>    ->
#>    unexpected END_OF_INPUT
#>   '\begin{foo}' at 1:11 is still open
#> 
```
