# The default "catcodes" used by [parseLatex](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md).

The default "catcodes" used by
[parseLatex](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md).

## Usage

``` r
defaultCatcodes
```

## Details

`defaultCatcodes` is a dataframe containing the default catcode
definitions. The numeric values of each code are exported, e.g. `LETTER`
is 11.

## Examples

``` r
# \makeatletter has no effect by default...
unclass(parseLatex("\\makeatletter\\internal@macro"))
#> [[1]]
#> MACRO: \makeatletter
#> 
#> [[2]]
#> MACRO: \internal
#> 
#> [[3]]
#> SPECIAL: @
#> 
#> [[4]]
#> TEXT: macro
#> 
# ... but the effect can be simulated
atletter <- rbind(defaultCatcodes,
                  data.frame(char="@", catcode=11))
unclass(parseLatex("\\makeatletter\\internal@macro",
                   catcodes = atletter))
#> [[1]]
#> MACRO: \makeatletter
#> 
#> [[2]]
#> MACRO: \internal@macro
#> 
# These are the default codes:
cbind(defaultCatcodes, name = c("ESCAPE", "LBRACE", "RBRACE", "MATH",
     "ALIGN",  "NEWLINE","NEWLINE", "PARAM",  "SUPER",
     "SUB",    "SPACE",  "SPACE", "COMMENT"))
#>    char catcode    name
#> 1    \\       0  ESCAPE
#> 2     {       1  LBRACE
#> 3     }       2  RBRACE
#> 4     $       3    MATH
#> 5     &       4   ALIGN
#> 6    \n       5 NEWLINE
#> 7    \r       5 NEWLINE
#> 8     #       6   PARAM
#> 9     ^       7   SUPER
#> 10    _       8     SUB
#> 11           10   SPACE
#> 12   \t      10   SPACE
#> 13    %      14 COMMENT
# The missing ones are
#  9 - IGNORE
# 11 - LETTER
# 12 - OTHER
# 13 - ACTIVE
# 15 - INVALID
```
