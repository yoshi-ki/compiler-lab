let rec h f x = f x in
let rec f x y = x + y in
let g = h f 2 in
print_int (g 3)
