# Huffman

My implementation of the Huffman encoder in Haskell.


## Run

    cabal run tt-huffman encode file.txt
    cabal run tt-huffman decode file.txt

## TO-DO

- DRY: `code c = fromMaybe "" (Map.lookup c cm)`
- Check places wher instead of using `let ... in` you could use `... where`.


## Questions

- Why using cons (:) for buffering and reversing the buffer when printing is not working for the last segment?