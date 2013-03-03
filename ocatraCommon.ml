let (@@) f x = f x

let ($) f g x = f (g x)

let p s = print_endline s; flush stdout

let log s = 
  try
    let _ = Sys.getenv "OCATRA_DEBUG" in
    Lwt_io.print s
  with Not_found -> Lwt.return_unit
