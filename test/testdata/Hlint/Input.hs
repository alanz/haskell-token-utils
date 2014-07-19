module Hlint.Input where

--Layout rule applies after 'where','let','do' and 'of'

--In this Example: rename 'sq' to 'square'.

sumSquares x y= sq x + sq y where sq x= x^pow
  --There is a comment.
                                  pow=2

thisIsATest = maybe (a * b) id 100


advanced = maybe (case x of
    Nothing -> a
    Just _ -> b) id 100


advanced = foo
 where
  foo = maybe
            x
            id $
                100
