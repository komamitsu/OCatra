open Unix
open HttpCommon
open HttpRequest
open HttpResponse

let start port proc =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_sock SO_REUSEADDR true;
  let address = inet_addr_any in
  bind server_sock (ADDR_INET (address, port));
  listen server_sock 10;
  while true do
    let (client_sock, client_addr) = accept server_sock in
    ignore (
      Thread.create (fun socket ->
        let in_ch = in_channel_of_descr socket in
        let out_ch = out_channel_of_descr socket in
        while true do
          let req = parse_request in_ch in
          let res = proc req in
          response out_ch res;
          flush out_ch
        done
      ) client_sock
    )
  done

