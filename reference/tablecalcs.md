# Calculations on tables

Calculations on tables

## Usage

``` r
tableNrow(table)

tableNcol(table)

tableDim(table)
```

## Arguments

- table:

  A known tabular-like environment object.

## Value

`tableNrow()` returns the number of rows in the table.

`tableNcol()` returns the number of columns in the table.

`tableDim()` returns the number of rows and columns in the table.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:3], format = "latex")
parsed <- parseLatex(latex)
table <- parsed[[find_tabular(parsed)]]
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r|r}
#> \hline
#>   & mpg & cyl & disp\\
#> \hline
#> Mazda RX4 & 21 & 6 & 160\\
#> \hline
#> Mazda RX4 Wag & 21 & 6 & 160\\
#> \hline
#> \end{tabular}
tableNrow(table)
#> [1] 3
tableNcol(table)
#> [1] 4
tableDim(table)
#> [1] 3 4
```
