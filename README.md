# Huffman

My implementation of the Huffman encoder in Haskell.


## Run

    cabal run tt-huffman encode file.txt
    cabal run tt-huffman decode file.txt


## Inspect Binary File

    xxd test/resources/file.bin


## Questions

- What should `Bit` be: `Char` or a number?
