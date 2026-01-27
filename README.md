# :christmas_tree::santa: Advent of Code 2025 :santa::christmas_tree:
My OCaml :camel: solutions for some [Advent of Code 2025](https://adventofcode.com/2025) puzzles.
## How to run
1. Clone the repository
```sh
git clone https://github.com/danielrouco/Advent-of-Code-2025.git
```
In each folder there are these files:
```
dayN
├── example.txt
├── input.txt
├── part1.ml
└── part2.ml
```
2. Compile part *X* of day *N*:
```sh
cd dayN
```
```sh
ocamlopt partX.ml -o partX
```
3. Run the executable
```sh
./partX
```
> [!NOTE]  
> By default the program gives the solution for the `input.txt` file. If you want to change that, only change the first line of the `.ml` file:
>
> By default:
> ```ml
> let file = open_in "input.txt"
> ```
> If you want to execute it with `example.txt` instead:
> ```ml
> let file = open_in "example.txt"
> ```
