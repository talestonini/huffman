cabal clean
cabal build

cabal run tt-huffman encode test/resources/tales.txt
cabal run tt-huffman decode test/resources/tales.txt

cabal run tt-huffman encode test/resources/pnp.txt
cabal run tt-huffman decode test/resources/pnp.txt

cabal run tt-huffman encode test/resources/sample_unicode.txt
cabal run tt-huffman decode test/resources/sample_unicode.txt

diff test/resources/tales.txt test/resources/tales.txt-inflated
diff test/resources/pnp.txt test/resources/pnp.txt-inflated
diff test/resources/sample_unicode.txt test/resources/sample_unicode.txt-inflated