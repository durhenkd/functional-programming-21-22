
module Sugar where

infixl 3 ??

(Just x) ?? _ = x
_ ?? def = def

