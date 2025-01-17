# Day 19

Today's problem has to do with drawing elements from a set of strings (allowing repetition) to make a larger string. Part 1 asks for which of the larger strings this is possible, and part 2 asks the number of ways to make a larger string.

I tried a few approaches. First I tried the Prologyist thing I could think of, to write a grammar for the larger string using DCGs, and to use `phrase(my_grammar(S), larger_string)` to test each value. This was too slow.

Next I hoped to more quickly brute force by building a Trie. This was also too slow. Part of this is probably because the libraries for key-value maps in Prolog (e.g. assoc) are slower than equivalents in imperative languages.

The final approach I tried that worked was memoized recursion. The code for this was much simpler than for the Trie.