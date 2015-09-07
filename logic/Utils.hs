module Utils where


subscriptify :: Char -> Char
subscriptify '0' = '₀'
subscriptify '1' = '₁'
subscriptify '2' = '₂'
subscriptify '3' = '₃'
subscriptify '4' = '₄'
subscriptify '5' = '₅'
subscriptify '6' = '₆'
subscriptify '7' = '₇'
subscriptify '8' = '₈'
subscriptify '9' = '₉'
subscriptify _ = error "subscriptify: non-numeral argument"

