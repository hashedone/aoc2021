My solutions for 2021 Advent of Code solutions. ~No special assumptions this year
just make solutions as simple as possible.~ Changed my mind. No mutations
besides parsing. Zero mutable variables. Pure functional Rust. Everything would
be done in rust. It would prob kill me, but why not ;)

Mutable borrows are allowed unless I actually need to create mutable variables
(so implementing own iterator, or functions like `Iterator::scan` are ok).

Current code stats would be generated with tokei.

```
  ~/git/aoc2021 main ?2
 tokei
===============================================================================
 Language            Files        Lines         Code     Comments       Blanks
===============================================================================
 Cabal                   1           55           46            3            6
 Haskell                 2           59           34            7           18
 Markdown                1           29            0           26            3
 TOML                   14          118           79           13           26
 YAML                    2          111           36           64           11
-------------------------------------------------------------------------------
 Rust                   13         1256         1048           16          192
 |- Markdown             2           12            0            9            3
 (Total)                           1268         1048           25          195
===============================================================================
 Total                  33         1628         1243          129          256
===============================================================================
```
