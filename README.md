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
 Rust                    3          236          195            0           41
 TOML                    4           32           23            3            6
===============================================================================
 Total                   8          291          218           23           50
===============================================================================
```
