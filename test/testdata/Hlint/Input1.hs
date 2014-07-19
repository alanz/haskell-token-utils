module Hlint.Input where

--Layout rule applies after 'where','let','do' and 'of'

--In this Example: rename 'sq' to 'square'.

sumSquares x y= sq x + sq y where sq x= x^pow
  --There is a comment.
                                  pow=2

thisIsATest  = maybe (a * b) id 100

thisIsATest' = fromMaybe (a * b) 100



