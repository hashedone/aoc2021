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
 TOML                   13          110           74           12           24
-------------------------------------------------------------------------------
 Rust                   12         1161          973           14          174
 |- Markdown             2           12            0            9            3
 (Total)                           1173          973           23          177
===============================================================================
 Total                  26         1297         1047           49          201
===============================================================================
```
