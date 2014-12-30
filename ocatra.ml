open OcatraCommon
open OcatraHttpCommon
open OcatraHttpRequest
open OcatraHttpResponse

let doc_root = ref None
let get_routes = ref (OcatraRoutes.init ())
let post_routes = ref (OcatraRoutes.init ())
let put_routes = ref (OcatraRoutes.init ())
let delete_routes = ref (OcatraRoutes.init ())

let docroot path =
  doc_root := Some path

let get path handler =
  get_routes := OcatraRoutes.bind !get_routes path handler 

let post path handler =
  post_routes := OcatraRoutes.bind !post_routes path handler 

let put path handler =
  put_routes := OcatraRoutes.bind !put_routes path handler 

let delete path handler =
  delete_routes := OcatraRoutes.bind !delete_routes path handler 

let run ?conf:(conf=OcatraConfig.create ()) () =
  OcatraHttpServer.start conf
    (fun req ->
      let routes =
        match req.methd with
        | Method.Get -> !get_routes
        | Method.Post -> !post_routes
        | Method.Put -> !put_routes
        | Method.Delete -> !delete_routes
      in
      try
        let handler = OcatraRoutes.find routes req.path in
        handler req
      with 
      | HttpError st -> create_response ~status:st
          (Content.TextPlain (Status.string_of_status st)) ()
      | Not_found -> 
          let create_not_found () = 
            create_response ~status:Status.NotFound
              (Content.TextPlain "Not found") ()
          in
          if req.methd = Method.Get then
            match !doc_root with
            | Some document_root ->
              begin
                try
                  let fullpath = Filename.concat document_root req.path in
                  let content = OcatraStaticFile.get_content fullpath in
                  create_response ~status:Status.OK content ()
                with Not_found -> create_not_found ()
              end
            | None -> create_not_found ()
          else
            create_not_found ()
    )

let say = OcatraHttpResponse.create_response

let (++>) req = Param.find req.param

