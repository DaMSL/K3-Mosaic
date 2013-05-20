if exists("b:current_syntax")
    finish
endif

let b:current_syntax = "k3"

syntax keyword k3Keyword declare expected foreign trigger role default
syntax keyword k3Keyword source sink file socket random stream bind patten consume
syntax keyword k3Keyword do if then else map 
syntax keyword k3Keyword map filtermap flatten aggregate groupby sort
syntax keyword k3Keyword peek insert delete update send
syntax match k3Keyword "\\"
syntax match k3Operator "<"
syntax match k3Operator ">"
syntax match k3Operator "-"
syntax match k3Operator "+"
syntax match k3Operator "*"
syntax match k3Operator "<="
syntax match k3Operator ">="
syntax match k3Operator ":="
syntax match k3Operator "=="
syntax match k3Operator "++"
syntax match k3Keyword "->"

syntax keyword k3Constant nothing () _
syntax keyword k3Boolean true false
syntax match   k3Number "\v\d+"

syntax keyword k3Type unit int float bool string address
syntax match   k3Comment "\v//.*$"
syntax region  k3Comment start="/\*" end="\*/"

highlight link k3Keyword Keyword
highlight link k3Type Type
highlight link k3Constant Constant
highlight link k3Comment Comment
highlight link k3Boolean Boolean
highlight link k3Number Number
highlight link k3Operator Operator

