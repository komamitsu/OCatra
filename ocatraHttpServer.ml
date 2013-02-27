open Lwt
open Lwt_unix
open OcatraCommon
open OcatraHttpCommon

let set_keepalive socket = function
  | Some ka -> setsockopt_float socket SO_RCVTIMEO ka
  | None -> ()

let start port proc ?(keepalive=Some(15.0)) () =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_sock SO_REUSEADDR true;
  let address = Unix.inet_addr_any in
  bind server_sock (ADDR_INET (address, port));
  listen server_sock 10;

  let rec accept_loop () =
    accept server_sock >>= 
      (fun (client_sock, client_addr) ->
        set_keepalive client_sock keepalive;

        let in_ch = Lwt_io.of_fd Lwt_io.input client_sock in
        let out_ch = Lwt_io.of_fd Lwt_io.output client_sock in

        let rec worker_loop () =
          OcatraHttpRequest.parse_request in_ch >>=
            fun req ->
              let res = proc req in
              OcatraHttpResponse.response out_ch res;
              Lwt_io.flush out_ch >>=
                fun _ ->
                  match keepalive with
                    | Some _ when OcatraHttpRequest.keepalive req.OcatraHttpRequest.version req.OcatraHttpRequest.header -> worker_loop ()
                    | _ -> return_unit
        in
        ignore (finalize worker_loop (fun _ -> Lwt_unix.close client_sock));
        accept_loop ()
      )
  in

  Lwt_main.run @@ accept_loop ()

