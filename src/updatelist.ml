open Lwt.Syntax

let url = "https://unicode.org/Public/emoji/14.0/emoji-test.txt"

let program =
  let* _, body = Cohttp_lwt_unix.Client.get (url |> Uri.of_string) in
  let* html = Cohttp_lwt__Body.to_string body in
  let* file = Lwt_io.open_file ~mode:Lwt_io.Output "emoji-test.txt" in
  Lwt_io.write_line file html

let () = Lwt_main.run program
