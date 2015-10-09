module Utils where

subscripts :: [(Char, Char)]
subscripts =
  [ ('0', '₀')
  , ('1', '₁')
  , ('2', '₂')
  , ('3', '₃')
  , ('4', '₄')
  , ('5', '₅')
  , ('6', '₆')
  , ('7', '₇')
  , ('8', '₈')
  , ('9', '₉')
  ]

subscriptify :: Char -> Char
subscriptify n | Just c <- lookup n subscripts = c
subscriptify n = error $ "subscriptify: non-numeral argument" ++ show n
