open OcatraHttpCommon

let get_content filepath =
  (* TODO: Change OcatraCommon.Content to pass in_channel instead of string *)
  let in_ch = open_in filepath in
  let len = in_channel_length in_ch in
  let data = really_input_string in_ch len in
  if Filename.check_suffix filepath ".html" then
    Content.TextHtml data
  else if Filename.check_suffix filepath ".xml" then
    Content.ApplicationXml data
  else if Filename.check_suffix filepath ".json" then
    Content.ApplicationJson data
  else
    Content.TextPlain data
