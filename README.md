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
 Markdown                1           23            0           20            3
 TOML                    9           78           54            8           16
-------------------------------------------------------------------------------
 Rust                    8          724          610            1          113
 |- Markdown             1            9            0            6            3
 (Total)                            733          610            7          116
===============================================================================
 Total                  18          825          664           29          132
===============================================================================
```
