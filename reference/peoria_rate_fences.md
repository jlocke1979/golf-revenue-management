# Rate fences on the Peoria posted card

Each row is one column on the Peoria weekday and weekend price sheet,
version 2. A fence is one posted price for one kind of round. Blank
columns stay in this list because the sheet has the column and leaves
every cell empty.

## Usage

``` r
peoria_rate_fences()
```

## Value

A data frame of fence names, what is priced, and the rule. `posted` is
FALSE when every cell in that column is empty.

## Examples

``` r
peoria_rate_fences()
#>                 segment                                   prices
#> 1                 Adult                  Green fee for one round
#> 2        Adult twilight                  Green fee for one round
#> 3                Senior                  Green fee for one round
#> 4                Junior                  Green fee for one round
#> 5       Junior twilight                  Green fee for one round
#> 6              Military                  Green fee for one round
#> 7       Adult with cart   Green fee and cart for one adult round
#> 8  Holiday 18 with cart Green fee and cart for one 18-hole round
#> 9                  Cart                       Cart for one round
#> 10            Pull cart                  Pull cart for one round
#> 11        Club per club                                 One club
#> 12          Club rental                              Club rental
#>                                                                                                                                                            rule
#> 1                                                                                            Weekday and weekend each have an adult green fee on every product.
#> 2                            Posted on Kellogg Executive, Kellogg Back 9, and Madison Back 9, for weekday and weekend. The other products leave the cell blank.
#> 3                                                                                            Weekday and weekend each have a senior green fee on every product.
#> 4            Posted on Kellogg Executive and on each 18-hole course. The weekday fee and the weekend fee are the same. Front 9 and Back 9 leave the cell blank.
#> 5                                                                                                                The sheet has the column. Every cell is blank.
#> 6                                                                                     The sheet has a weekday column and a weekend column. Every cell is blank.
#> 7  A posted adult price that includes a cart. On a weekday it equals that product's adult green fee plus its cart fee. On a weekend it is its own posted price.
#> 8                   The column header names a Saturday and Sunday holiday rate for 18 holes with a cart, from May 11 through September 29. Every cell is blank.
#> 9                                       Eighteen-hole courses are $17. Nine-hole products are $12. The sheet does not split the cart fee by weekday or weekend.
#> 10                                                                                                                               Five dollars on every product.
#> 11                                                                                                                        One dollar per club on every product.
#> 12                                                                         Kellogg 18 and Newman 18 are $20. Every other product, including Madison 18, is $10.
#>    posted
#> 1    TRUE
#> 2    TRUE
#> 3    TRUE
#> 4    TRUE
#> 5   FALSE
#> 6   FALSE
#> 7    TRUE
#> 8   FALSE
#> 9    TRUE
#> 10   TRUE
#> 11   TRUE
#> 12   TRUE
```
