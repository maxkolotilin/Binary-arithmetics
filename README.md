My custom realization of binary aritmetics (both integer and float) in x86-64 assembly (NASM). With sample.

## Integer aritmetics
[Booth's multiplication algorithm](https://en.wikipedia.org/wiki/Booth%27s_multiplication_algorithm)

Non-restoring division (as I remember)

## Float aritmetics
According to [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point)
Internally uses my custom integer arithmetics.

## How-to assembly
[Docs](http://www.nasm.us/xdoc/2.12.02/html/nasmdoc2.html)

## Run sample
For Linux x64 example:
        nasm -f elf64 float_aritmetics.s
        gcc -o app float_aritmetics.o
        ./app
