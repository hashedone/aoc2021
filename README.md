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
 Markdown                1           26            0           23            3
 TOML                   12          102           69           11           22
-------------------------------------------------------------------------------
 Rust                   11         1001          848            4          149
 |- Markdown             1            9            0            6            3
 (Total)                           1010          848           10          152
===============================================================================
 Total                  24         1129          917           38          174
===============================================================================
```
