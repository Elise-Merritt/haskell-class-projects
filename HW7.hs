-- Elise Merritt
-- CS 314, Fall 2020, Homework 7
-- Due 10:00 PM, November 23
-- Read the directions below, add your answers to this file, and submit it via Sakai.
-- Updated 2020-11-16: clarification of "contains"
-- Updated 2020-11-17: in language 3, added that A cannot be followed by C
--                     added some examples
-- Updated 2020-11-18: BCA is part of language 3

module HW7 where

-- You will need to download these from Sakai, under Resources -> Lecture Slides.
-- You should not need to import any additional files.

import Lec18
import Lec20

-- Each of the questions below describe a regular language over the alphabet symbols,
-- given below. Note that we use a custom Symbol type, rather than Char.

data Symbol = A | B | C | D deriving (Show, Eq)
symbols = [A,B,C,D]

-- For each question:
--
-- 1. Write a comment giving a regular expression for the language, written using the
--    notation from class. (You may write e for epsilon.)
--
-- 2. Write the same regular expression as a value of type RE Symbol. You must write the
--    value using only the constructors for RE and Symbol, or using functions you write
--    that are included in this file. Do not import Parser.hs.
--
-- 3. Define a DFA for the language. You may find it helpful to draw graphs on paper,
--    but you do not need to hand them in.
--
-- 4. Define your DFA as a value of type DFA state Symbol, for some convenient type state.
--    (Int is probably the most convenient type, but you are free to use any instance of
--    Eq.) You must give symbols as the alphabet.
--
-- You will be graded on the correctness of your RE and DFA values, which will be
-- determined by testing on randomly generated inputs. Your file should be runnable
-- Haskell code; you will lose points if your file cannot be loaded in GHCi.
--
-- You are encouraged, but not required, to justify why your RE and DFA describe the
-- specified language. In the event that your RE or DFA is incorrect, you may receive
-- partial credit based on the quality of your explanation.
--
-- Please make use of the provided dfaCheck function to make sure your DFA defines a
-- transition for every combination of symbol and state, and does not make a transition
-- to an unlisted state.

-- dfaCheck tries every combination of symbol and state for a DFA, and checks whether the
-- resulting state is listed.
dfaCheck dfa = all (`elem` states dfa) [ trans dfa s x | s <- states dfa, x <- alphabet dfa ]

-- 0. (Example) Strings containing the sequence ABC repeated zero or more times.

-- (ABC)*

regex0 = RStar (RSeq (RSym A) (RSeq (RSym B) (RSym C)))

dfa0 = DFA
    { alphabet = symbols
    , states = [1,2,3,4]
    , start = 1
    , trans = transit
    , accept = (== 1)
    }
    where
    transit 1 A = 2
    transit 2 B = 3
    transit 3 C = 1
    transit _ _ = 4

-- 1. Strings containing exactly two A's and zero or more other symbols.
--
--    Langauge includes: AA, ABCBADB, DDDDDDDDAA, BCDBCDACBDBBDCABBCBDBC
--    Language excludes: AAA, A, CBACBABCCCCABCBBC

-- (B|C|D)*(A)(B|C|D)*(A)(B|C|D)*
-- 
regex1 = (RSeq (RStar (RAlt (RSym B) (RAlt (RSym C)(RSym D)))) (RSeq (RSym A) (RSeq (RStar (RAlt (RSym B) (RAlt (RSym C) (RSym D)))) (RSeq (RSym A) (RStar (RAlt (RSym B) (RAlt (RSym C) (RSym D)))) ))) )

dfa1 =  DFA 
      { alphabet = symbols
      , states = [1,2,3,4]
      , start = 1
      , trans = transit
      , accept = (== 3)}
      where
      transit 1 A = 2
      transit 1 B = 1
      transit 1 C = 1
      transit 1 D = 1
      transit 2 A = 3
      transit 2 B = 2
      transit 2 C = 2
      transit 2 D = 2
      transit 3 A = 4
      transit 3 B = 3
      transit 3 C = 3
      transit 3 D = 3
      transit 4 A = 4
      transit 4 B = 4
      transit 4 C = 4
      transit 4 D = 4


-- 2. Strings containing only zero or more A's and B's, such that each symbol is different
--    from its neighbors. (Strings do not contain C or D.)

-- ((AB)*|(BA)*)
regex2 = (RAlt (RStar (RSeq (RSym A) (RSym B))) (RStar (RSeq (RSym B) (RSym A))) )

dfa2 = DFA
    { alphabet = symbols
    , states = [1,2,3,4,5]
    , start = 1
    , trans = transit
    , accept = (== 4)
    }
    where
    transit 1 A = 2
    transit 1 B = 3
    transit 1 C = 5
    transit 1 D = 5
    transit 2 A = 5
    transit 2 B = 4
    transit 2 C = 5
    transit 2 D = 5
    transit 3 A = 4
    transit 3 B = 5
    transit 3 C = 5
    transit 3 D = 5
    transit 4 A = 2
    transit 4 B = 3
    transit 4 C = 5
    transit 4 D = 5
    transit 5 A = 5
    transit 5 B = 5
    transit 5 C = 5
    transit 5 D = 5


-- 3. Strings containing only zero or more A's, B's, and C's, such that no A is followed
--    by A or C, no B is followed by A or B, and no C is followed by B or C. (Strings do
--    not contain D.)
--
--    Language includes: AB, ABCA, CABCAB, C, CABCABCABCABCAB, BCA
--    Language excludes: AC, AABC, CBA, ABCD

-- ((AB|BC|CA)* | (A|B|C))
regex3 = (RAlt (RStar (RAlt (RSeq (RSym A) (RSym B)) (RAlt (RSeq (RSym B) (RSym C)) (RSeq (RSym C) (RSym A))) )) (RAlt (RSym A) (RAlt (RSym B) (RSym C))) )

dfa3 = DFA
    { alphabet = symbols
    , states = [1,2,3,4,5,6]
    , start = 1
    , trans = transit
    , accept = \s -> s == 2 || s == 3 || s == 4 || s == 5
    }
    where
    transit 1 A = 2
    transit 1 B = 3
    transit 1 C = 4
    transit 1 D = 6
    transit 2 A = 6
    transit 2 B = 5
    transit 2 C = 6
    transit 2 D = 6
    transit 3 A = 6
    transit 3 B = 6
    transit 3 C = 5
    transit 3 D = 6
    transit 4 A = 5
    transit 4 B = 6
    transit 4 C = 6
    transit 4 D = 6
    transit 5 A = 2
    transit 5 B = 3
    transit 5 C = 4
    transit 5 D = 6
    transit 6 _ = 6

-- 4. Strings containing an A, a B, a C, and a D, in order, with any number of other
--    characters between them. This includes ABCD and ACDBABDCBAD and DCBADCBADCBADCBA,
--    but not DCBA or ABDAC or ACABD.

-- (A|B|C|D)*(A)(A|B|C|D)*(B)(A|B|C|D)*(C)(A|B|C|D)*(D)(A|B|C|D)*
regex4 = ( RSeq (RStar (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D))))) (RSeq (RSym A) (RSeq (RStar (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D)))))  (RSeq (RSym B) (RSeq (RStar (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D))))) (RSeq (RSym C) (RSeq (RStar (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D))))) (RSeq (RSym D) (RStar (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D))))) ))))))))

dfa4 = DFA
    { alphabet = symbols
    , states = [1,2,3,4,5]
    , start = 1
    , trans = transit
    , accept = (== 5)
    }
    where
    transit 1 A = 2
    transit 1 B = 1
    transit 1 C = 1
    transit 1 D = 1
    transit 2 A = 2
    transit 2 B = 3
    transit 2 C = 2
    transit 2 D = 2
    transit 3 A = 3
    transit 3 B = 3
    transit 3 C = 4
    transit 3 D = 3
    transit 4 A = 4
    transit 4 B = 4
    transit 4 C = 4
    transit 4 D = 5
    transit 5 A = 5
    transit 5 B = 5
    transit 5 C = 5
    transit 5 D = 5


-- 5. Strings containing at least 3 and not more than 5 symbols.
-- (A|B|C|D)(A|B|C|D)(A|B|C|D)(A|B|C|D|e)(A|B|C|D|e)
regex5 = (RSeq (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D)))) (RSeq (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D)))) (RSeq (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RSym D)))) (RSeq (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RAlt (RSym D) (REps))))) (RAlt (RSym A) (RAlt (RSym B) (RAlt (RSym C) (RAlt (RSym D) (REps)))))))))

dfa5 = DFA
    { alphabet = symbols
    , states = [1,2,3,4,5,6,7]
    , start = 1
    , trans = transit
    , accept = \s -> s == 4 || s == 5 || s == 6 
    }
    where
    transit 1 _ = 2
    transit 2 _ = 3
    transit 3 _ = 4
    transit 4 _ = 5
    transit 5 _ = 6
    transit 6 _ = 7
    transit 7 _ = 7
    


-- Extra Credit
-- ------------
--
-- Write a function reps, such that reps m n r is a regular expression that matches
-- strings that can be broken into at least m and not more than n substrings, each of
-- which matches r. That is, reps represents repetition with specified minimum and
-- maximum bounds.
--
-- Your function should handle negative integers as though they were 0. It should return
-- RZero if n < m.
--

reps :: Int -> Int -> RE symbol -> RE symbol
reps = undefined
