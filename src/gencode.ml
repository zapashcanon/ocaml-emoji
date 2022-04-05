type emoji =
  { code_point : string
  ; emoji : string
  ; description : string
  ; name : string
  ; category : string
  ; sub_category : string
  }

let string_escape_hex =
  let fst_code = Char.code '0' in
  let snd_code = Char.code 'a' - 10 in
  let nibble_to_hex_char n =
    if n < 0 || n >= 16 then
      invalid_arg "nibbles must be within the range 0x0 and 0xf"
    else if n < 10 then Char.chr (fst_code + n)
    else Char.chr (snd_code + n)
  in
  fun s ->
    let buf = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        let c = Char.code c in
        Buffer.add_string buf "\\x";
        let n = (c land 0xf0) lsr 4 in
        Buffer.add_char buf (nibble_to_hex_char n);
        let n = c land 0x0f in
        Buffer.add_char buf (nibble_to_hex_char n) )
      s;
    Buffer.contents buf

(* leading ints are illegal in
 * OCaml identifiers so we prepend
 * them with a '_' *)
let wrap_leading_ints s = match s.[0] with '0' .. '9' -> "_" ^ s | _ -> s

let to_legal_ident_char c =
  if not (Uchar.is_char c) then
    (* not a latin1 character, ex: quotation mark (U+2019) in names *)
    "_"
  else
    match Uchar.to_char c with
    | '&' -> "and"
    | '#' -> "hash"
    | '*' -> "star"
    | '-' | ' ' | ':' | '.' | ',' | '(' | ')' -> "_"
    | '!' ->
      (*TODO: use "_" and merge with previous case ? *)
      (* only used for ON! arrow and UP! button *)
      ""
    | '\197' -> "a" (* Å *)
    | '\227' -> "a" (* ã *)
    | '\231' -> "c" (* ç *)
    | '\233' -> "e" (* é *)
    | '\237' -> "i" (* í *)
    | '\241' -> "n" (* piñata !*)
    | '\244' -> "o" (* ô *)
    | ('_' | '\'' | '0' .. '9' | 'a' .. 'z') as c -> String.make 1 c
    | 'A' .. 'Z' as c -> String.make 1 (Char.lowercase_ascii c)
    | c -> failwith (Format.sprintf "unhandled character: '%c'" c)

let deduplicate_underscores s =
  let buf = Buffer.create (String.length s) in
  let _was_underscore : bool =
    String.fold_left
      (fun was_underscore c ->
        let is_underscore = c = '_' in
        if not (was_underscore && is_underscore) then Buffer.add_char buf c;
        is_underscore )
      false s
  in
  Buffer.contents buf

let identifier_of_description s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s) in
  begin
    try
      while true do
        match Uutf.decode decoder with
        | `Uchar u -> Buffer.add_string buf (to_legal_ident_char u)
        | `End -> raise Exit
        | `Await -> ()
        | `Malformed e -> failwith e
      done
    with Exit -> ()
  end;
  Buffer.contents buf |> wrap_leading_ints |> deduplicate_underscores

let parse_line (l, category, sub_category) line =
  let group_prefix = "# group: " in
  let group_prefix_len = String.length group_prefix in
  let subgroup_prefix = "# subgroup: " in
  let subgroup_prefix_len = String.length subgroup_prefix in
  let line_len = String.length line in
  let fully_qualified_string = "fully-qualified" in
  if String.starts_with ~prefix:group_prefix line then
    (* change of category line *)
    let group =
      String.sub line group_prefix_len (line_len - group_prefix_len)
    in
    let category = identifier_of_description @@ String.trim group in
    (l, category, sub_category)
  else if String.starts_with ~prefix:subgroup_prefix line then
    (* change of sub category line *)
    let subgroup =
      String.sub line subgroup_prefix_len (line_len - subgroup_prefix_len)
    in
    let sub_category = identifier_of_description @@ String.trim subgroup in
    (l, category, sub_category)
  else if String.starts_with ~prefix:"#" line then (l, category, sub_category)
  else if String.trim line = "" then (* empty line *)
    (l, category, sub_category)
  else
    (* its an emoji line!*)
    match String.split_on_char ';' line with
    | [ code_point; rest ] -> (
      let code_point =
        "U+" ^ String.concat " U+" @@ String.split_on_char ' '
        @@ String.trim code_point
      in
      let rest = String.trim rest in
      let handle_infos infos =
        match String.split_on_char ' ' infos with
        | "" :: emoji :: _magic_number :: description_list ->
          let description = String.concat " " description_list in
          let name = identifier_of_description description in

          ( { code_point
            ; emoji
            ; description
            ; name
            ; category = Format.sprintf "category_%s" category
            ; sub_category = Format.sprintf "sub_category_%s" sub_category
            }
            :: l
          , category
          , sub_category )
        | _invalid_line -> failwith (Format.sprintf "invalid line ' ': %s" line)
      in
      match String.split_on_char '#' rest with
      | [ qualified; infos ] ->
        if String.trim qualified = fully_qualified_string then
          handle_infos infos
        else
          (* we ignore unqualified/minimally-qualified emojis *)
          (l, category, sub_category)
      | [ qualified; " "; infos; "" ] ->
        (* this is for the keycaps emoji *)
        if String.trim qualified = fully_qualified_string then
          handle_infos (Format.sprintf " #%s#" infos)
        else (l, category, sub_category)
      | _invalid_line -> failwith (Format.sprintf "invalid line '#': %s" line) )
    | _invalid_line -> failwith (Format.sprintf "invalid line for ';': %s" line)

let read file =
  let chan = open_in file in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let try_read () = try Some (input_line chan) with End_of_file -> None in
      let rec loop acc =
        match try_read () with
        | Some s -> loop (s :: acc)
        | None -> List.rev acc
      in
      loop [] )

let lines = read "emoji-test.txt"

let init = ([], "", "")

let emojis, _last_category, _last_sub_category =
  List.fold_left parse_line init lines

let emojis = List.sort (fun e1 e2 -> compare e1.name e2.name) emojis

(* category_name -> (emoji_name -> unit) *)
let cats_table = Hashtbl.create 512

(* sub_category_name -> (emoji_name -> unit) *)
let subcats_table = Hashtbl.create 512

let () =
  List.iter
    (fun { category; sub_category; name; _ } ->
      let cat_table =
        match Hashtbl.find_opt cats_table category with
        | None ->
          let cat_table = Hashtbl.create 512 in
          Hashtbl.add cats_table category cat_table;
          cat_table
        | Some cat_table -> cat_table
      in
      Hashtbl.add cat_table name ();
      let subcat_table =
        match Hashtbl.find_opt subcats_table sub_category with
        | None ->
          let subcat_table = Hashtbl.create 512 in
          Hashtbl.add subcats_table sub_category subcat_table;
          subcat_table
        | Some subcat_table -> subcat_table
      in
      Hashtbl.add subcat_table name () )
    emojis;

  Format.printf
    "(** All Emojis defined by the Unicode standard, encoded using UTF-8 *)@\n";
  List.iter
    (fun e ->
      Format.printf "@\n(** %s (%s): %s *)@\nlet %s = \"%s\"@\n" e.emoji
        e.code_point e.description
        (identifier_of_description e.description)
        (string_escape_hex e.emoji) )
    emojis

let pp_print_list_to_ocaml_array fmt a =
  Format.fprintf fmt "[|%a|]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
       Format.pp_print_string )
    a

let subcats =
  Hashtbl.fold
    (fun name emojis acc ->
      (name, List.sort compare @@ List.of_seq @@ Hashtbl.to_seq_keys emojis)
      :: acc )
    subcats_table []

let subcats = List.sort (fun (n1, _) (n2, _) -> compare n1 n2) subcats

let () =
  Format.printf "@\n(** All sub categories *)@\n";
  List.iter
    (fun (name, emojis) ->
      Format.printf "@\nlet %s = %a@\n" name pp_print_list_to_ocaml_array emojis
      )
    subcats

let cats =
  Hashtbl.fold
    (fun name emojis acc ->
      (name, List.sort compare @@ List.of_seq @@ Hashtbl.to_seq_keys emojis)
      :: acc )
    cats_table []

let cats = List.sort (fun (n1, _) (n2, _) -> compare n1 n2) cats

let () =
  Format.printf "@\n(** All categories *)@\n";
  List.iter
    (fun (cat, emojis) ->
      Format.printf "@\nlet %s = %a@\n" cat pp_print_list_to_ocaml_array emojis
      )
    cats

let all_names = List.map (fun emoji -> emoji.name) emojis

let () =
  Format.printf "@\n(** All included emojis in an array *)@\n";
  Format.printf "let all_emojis = %a@\n" pp_print_list_to_ocaml_array all_names
