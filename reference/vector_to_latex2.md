# Convert vector to items

Convert vector to items

## Usage

``` r
vector_to_latex2(x)
```

## Arguments

- x:

  A list or vector to convert.

## Value

A
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing the entries of `x` concatenated.

## Examples

``` r
print(vector_to_latex2(1:3), tags = TRUE)
#>  tag= SPECIAL 
#> SPECIAL(OTHER): "1"
#>  tag= SPECIAL 
#> SPECIAL(OTHER): "2"
#>  tag= SPECIAL 
#> SPECIAL(OTHER): "3"
```
