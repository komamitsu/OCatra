open HttpCommon
open HttpRequest
open HttpResponse

module Proc = struct
  type t = HttpRequest.t -> HttpResponse.t
end

module Routes = struct
  type t = (string, Proc.t) Hashtbl.t
  let init () : t = Hashtbl.create 10
  let bind = Hashtbl.replace
  let find = Hashtbl.find
end

let get_routes = Routes.init ()
let post_routes = Routes.init ()
let put_routes = Routes.init ()
let delete_routes = Routes.init ()

let get path f = Routes.bind get_routes path f
let post path f = Routes.bind post_routes path f
let put path f = Routes.bind put_routes path f
let delete path f = Routes.bind delete_routes path f

let run ?(port=59876) () =
  HttpServer.start port
    (fun req ->
      let route =
        match req.methd with
        | HttpMethod.Get -> get_routes
        | HttpMethod.Post -> post_routes
        | HttpMethod.Put -> put_routes
        | HttpMethod.Delete -> delete_routes
      in
      try
        let proc = Routes.find route req.path in proc req
      with 
      | HttpError st -> create_response ~status:st
          (HttpContent.TextPlain (HttpStatus.string_of_status st)) ()
      | Not_found -> create_response ~status:HttpStatus.NotFound
          (HttpContent.TextPlain "Not found") ()
    )

let say = HttpResponse.create_response

let (@-) req = HttpParam.find req.param

