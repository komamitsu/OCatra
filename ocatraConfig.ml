type t = {
  port: int;
  keepalive: float option;
  processes: int;
}

let create 
  ?port:(port=8080)
  ?keepalive:(keepalive=Some(15.0))
  ?processes:(processes=4) () =

  {
    port=port;
    keepalive=keepalive;
    processes=processes;
  }

let port t = t.port

let keepalive t = t.keepalive

let processes t = t.processes
