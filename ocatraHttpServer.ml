open Lwt
open Lwt_unix
open OcatraCommon
open OcatraHttpCommon

let set_keepalive socket = function
  | Some ka -> setsockopt_float socket SO_RCVTIMEO ka
  | None -> ()

let start conf proc =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_sock SO_REUSEADDR true;
  let address = Unix.inet_addr_any in
  bind server_sock (ADDR_INET (address, (OcatraConfig.port conf)));
  listen server_sock 256;

  let rec accept_loop () =
    accept server_sock >>= 
      (fun (client_sock, client_addr) ->
        let keepalive = OcatraConfig.keepalive conf in
        set_keepalive client_sock keepalive;

        let in_ch = Lwt_io.of_fd Lwt_io.input client_sock in
        let out_ch = Lwt_io.of_fd Lwt_io.output client_sock in

        let rec worker_loop () =
          try_lwt
            OcatraHttpRequest.parse_request in_ch >>=
              fun req ->
                let res = proc req in
                OcatraHttpResponse.response out_ch res >>
                Lwt_io.flush out_ch >>
                  match keepalive with
                  | Some _ when OcatraHttpRequest.keepalive req.OcatraHttpRequest.version req.OcatraHttpRequest.header -> worker_loop ()
                  | _ -> Lwt_unix.close client_sock
          with
          | End_of_file -> Lwt_unix.close client_sock
          | e -> (
              Lwt_io.print (Printexc.to_string e) >>
              Lwt_io.flush Lwt_io.stdout >>
              Lwt_unix.close client_sock
          )
        in
        ignore (worker_loop ());
        accept_loop ()
      )
  in

  let num_of_children = (OcatraConfig.processes conf) in

  let children = Hashtbl.create num_of_children in

  let rec create_process n =
    if n <= 0 then
      ()
    else begin
      let pid = fork () in
      if pid = 0 then
        begin
          Sys.set_signal Sys.sigterm Sys.Signal_default;
          Sys.set_signal Sys.sigint Sys.Signal_default;
          Lwt_main.run @@ accept_loop ()
        end
      else
        Hashtbl.add children pid true;
        create_process (n - 1)
    end
  in

  let kill_children () =
    Hashtbl.iter (fun pid _ -> Unix.kill pid Sys.sigterm) children
  in

  let sighandler signal =
    begin
      print_endline "sighandler is called";
      kill_children ();
      Unix.sleep 1
    end
  in

  Sys.set_signal Sys.sigterm (Sys.Signal_handle sighandler);
  Sys.set_signal Sys.sigint (Sys.Signal_handle sighandler);

  ignore (create_process num_of_children);

  let rec wait_children () =
    if Hashtbl.length children = 0 then ()
    else begin
      let (pid, _) = Unix.waitpid [Unix.WNOHANG] (-1) in
      if pid > 0 then
        if Hashtbl.mem children pid then
          begin
            Hashtbl.remove children pid;
            wait_children ()
          end
        else
          begin
            print_endline (Printf.sprintf "Unexpected process id: %d\n" pid);
            kill_children ()
          end
      else
        begin
          Unix.sleep 1;
          wait_children ()
        end
    end
  in
  wait_children ()
