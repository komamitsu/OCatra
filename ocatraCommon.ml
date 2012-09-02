let (@@) f x = f x

let ($) f g x = f (g x)

let p s = print_endline s; flush stdout

let log s = 
  (*
    print_endline s
  *)
  ()

