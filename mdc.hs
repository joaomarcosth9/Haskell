mdc a b | a == b = a
        | a > b = mdc (a-b) b
        | a < b = mdc (b-a) a  
 
mdc' a b = if a == b then a else if a > b then mdc' (a-b) b else mdc' (b-a) a
