let rec fact x = if x = 1.0 then 1.0 else x *. fact (x -. 1.0) in print_int (fact 1500.0)
