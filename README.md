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
 TOML                   11           94           64           10           20
-------------------------------------------------------------------------------
 Rust                   10          904          764            4          136
 |- Markdown             1            9            0            6            3
 (Total)                            913          764           10          139
===============================================================================
 Total                  22         1024          828           37          159
===============================================================================
```
