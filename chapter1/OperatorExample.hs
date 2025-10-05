module OperatorExample where

infixl 6 +++
(+++) a b = a + b

infixl 7 ***
a *** b = a * b

-- same associativity and fixity as (/)
divide = (/)
