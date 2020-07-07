let c = ref 1

let line_increment () = c := (!c) + 1

let line_get () = (!c)
