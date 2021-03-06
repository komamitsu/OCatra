open Ocatra
open OcatraHttpCommon
open OcatraHttpCommon.Content

let _ =
  docroot "./example/docroot";

  get "/" (fun r -> say (TextPlain "Hello, World (GET)") ());

  post "/" (fun r -> say (TextPlain "Hello, World (POST)") ());

  get "/givemeyournameandage" (fun r ->
    say (
      TextHtml (
        "<html><head><title>hello " ^ r ++> "name" ^ "</title></head>" ^
        "<body><h3>you are " ^ r ++> "age" ^ " years old.</h3></body></html>")
    ) ()
  );

  run ()

