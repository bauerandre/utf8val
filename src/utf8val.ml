(*
  Patterns available for representing a single code point:

  0_______
  110_____ 10______
  1110____ 10______ 10______
  11110___ 10______ 10______ 10______
  111110__ 10______ 10______ 10______ 10______
  1111110_ 10______ 10______ 10______ 10______ 10______

  The shortest pattern possible must be used.
*)

let is_utf8 s =

  let trailing s i =
    Char.code (String.unsafe_get s i) land 0b11000000 = 0b10000000
  in

  let next s i = Char.code (String.unsafe_get s (i+1)) in

  let rec loop s len i =
    if i = len then true
    else
      let x = Char.code (String.unsafe_get s i) in
      if x lsr 7 = 0 then
        loop s len (i+1)

      else if x lsr 5 = 0b110 then
        i+1 < len
        && (x land 0b00011110 <> 0)
        && trailing s (i+1)
        && loop s len (i+2)

      else if x lsr 4 = 0b1110 then
        i+2 < len
        && (x <> 0b11100000 || next s i land 0b00100000 <> 0)
        && trailing s (i+1)
        && trailing s (i+2)
        && loop s len (i+3)

      else if x lsr 3 = 0b11110 then
        i+3 < len
        && (x <> 0b11110000 || next s i land 0b00110000 <> 0)
        && trailing s (i+1)
        && trailing s (i+2)
        && trailing s (i+3)
        && loop s len (i+4)

      else if x lsr 2 = 0b111110 then
        i+4 < len
        && (x <> 0b11111000 || next s i land 0b00111000 <> 0)
        && trailing s (i+1)
        && trailing s (i+2)
        && trailing s (i+3)
        && trailing s (i+4)
        && loop s len (i+5)

      else if x lsr 1 = 0b1111110 then
        i+5 < len
        && (x <> 0b11111100 || next s i land 0b00111100 <> 0)
        && trailing s (i+1)
        && trailing s (i+2)
        && trailing s (i+3)
        && trailing s (i+4)
        && trailing s (i+5)
        && loop s len (i+6)

      else false
  in
  loop s (String.length s) 0

exception Malformed of int option * string

type pos = int
type code = int

type esc = {
  esc_char : char;
  esc_reader : string -> pos ref -> code list;
  escape : code -> string option;
}

exception Internal_error

let hex c =
  match c with
      '0'..'9' -> Char.code c - Char.code '0'
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | _ -> raise Internal_error

let to_hexcode x =
  if x < 0 || x > 15 then raise Internal_error
  else if x < 10 then
    Char.code '0' + x
  else
    Char.code 'A' + x - 10

let code_of_surrogate_pair i j =
  let high10 = i - 0xD800 in
  let low10 = j - 0xDC00 in
  0x10000 + ((high10 lsl 10) lor low10)

let unsafe_hex4 s p =
  (hex s.[p] lsl 12)
  lor (hex s.[p+1] lsl 8)
  lor (hex s.[p+2] lsl 4)
  lor hex s.[p+3]

let finish_surrogate_pair x s p =
  if p + 5 >= String.length s || s.[p] <> '\\' || s.[p+1] <> 'u' then
    raise (Malformed (Some p, s))
  else
    let p = p + 2 in
    let y = unsafe_hex4 s p in
    if y >= 0xDC00 && y <= 0xDFFF then
      code_of_surrogate_pair x y
    else
      raise (Malformed (Some p, s))

let json_esc_reader s pos =
  let len = String.length s in
  let p = !pos in
  if p = len then
    raise (Malformed (Some p, s))
  else
    match s.[p] with
      | 'b' | 't' | 'n' | 'f' | 'r' | '\"' -> [0x5C]
      | '/' -> [] (* remove the escape since it's allowed but not needed *)
      | '\\' -> incr pos; [0x5C; 0x5C]
      | 'u' ->
          (let p = p + 1 in
           try
             if p + 3 >= len then
               raise Internal_error
             else
               let x = unsafe_hex4 s p in
               let code =
                 if x >= 0xD800 && x <= 0xDBFF then (
                   let code = finish_surrogate_pair x s (p+4) in
                   pos := p + 10;
                   code
                 )
                 else (
                   pos := p + 4;
                   x
                 )
               in
               match code with
                 | 0x08 -> [0x5C; 0x62] (* \b *)
                 | 0x09 -> [0x5C; 0x74] (* \t *)
                 | 0x0A -> [0x5C; 0x6E] (* \n *)
                 | 0x0C -> [0x5C; 0x66] (* \f *)
                 | 0x0D -> [0x5C; 0x72] (* \r *)
                 | 0x22 -> [0x5C; 0x22] (* \ double quote *)
               (*| 0x2F -> [0x5C; 0x2F] (* / *) (*not required*) *)
                 | 0x5C -> [0x5C; 0x5C] (* \\ *)
                 | _ ->
                     if code <= 0x1F then [
                       0x5C; 0x75; (* \u *) 0x30; 0x30; (* 00 *)
                       to_hexcode (code lsr 4); to_hexcode (code land 0x0F)
                     ]
                     else
                       [code]

           with Internal_error -> raise (Malformed (Some p, s))
          )
      | _ ->
          raise (Malformed (Some p, s))

(* Escape raw control characters *)
let escape_json x =
  match x with
    | 0x09 | 0x0A | 0x0D ->
        (* control characters that are whitespace;
           can't fix if they're within string literals *)
        None
    | _ ->
        if x <= 0x1F then
          (* control characters that must be escaped (in string literals) *)
          Some (Printf.sprintf "\\u00%02X" x)
        else
          (* other characters will be encoded in UTF-8 *)
          None

let json_esc = {
  esc_char = '\\';
  esc_reader = json_esc_reader;
  escape = escape_json;
}

let no_esc = {
  esc_char = '\xFF';
  esc_reader = (fun s pos -> assert false);
  escape = (fun _ -> None);
}

let fold_left ?(esc = no_esc) f acc s =

  let { esc_char } = esc in

  let check b =
    if not b then
      raise (Malformed (None, s))
  in

  let check_at i b =
    if not b then
      raise (Malformed (Some i, s))
  in

  let ( ++ ) x i =
    let p = Char.code (String.unsafe_get s i) in
    check_at i (p land 0b11000000 = 0b10000000);
    (x lsl 6) lor (p land 0b00111111)
  in

  let next s i = Char.code (String.unsafe_get s (i+1)) in

  (* pos is updated only for esc_reader *)
  let pos = ref 0 in

  let rec loop f acc s len i =
    if i = len then acc
    else
      let x = Char.code (String.unsafe_get s i) in
      if x lsr 7 = 0 then (
        if x = Char.code esc_char then (
          pos := i+1;
          let acc = List.fold_left f acc (esc.esc_reader s pos) in
          loop f acc s len !pos
        )
        else
          let acc = f acc x in
          loop f acc s len (i+1)
      )
      else if x lsr 5 = 0b110 then (
        check (i+1 < len && x land 0b00011110 <> 0);
        let acc = f acc ((x land 0b00011111) ++ (i+1)) in
        loop f acc s len (i+2)
      )
      else if x lsr 4 = 0b1110 then (
        check (i+2 < len
               && (x <> 0b11100000 || next s i land 0b00100000 <> 0));
        let acc =
          f acc ((x land 0b00001111) ++ (i+1) ++ (i+2))
        in
        loop f acc s len (i+3)
      )
      else if x lsr 3 = 0b11110 then (
        check (i+3 < len
               && (x <> 0b11110000 || next s i land 0b00110000 <> 0));
        let acc =
          f acc ((x land 0b00000111) ++ (i+1) ++ (i+2) ++ (i+3))
        in
        loop f acc s len (i+4)
      )
      else if x lsr 2 = 0b111110 then (
        check (i+4 < len
               && (x <> 0b11111000 || next s i land 0b00111000 <> 0));
        let acc =
          f acc ((x land 0b00000011) ++ (i+1) ++ (i+2) ++ (i+3) ++ (i+4))
        in
        loop f acc s len (i+5)
      )
      else if x lsr 1 = 0b1111110 then (
        check (i+5 < len
               && (x <> 0b11111100 || next s i land 0b00111100 <> 0));
        let acc =
          f acc ((x land 0b11111110)
                 ++ (i+1) ++ (i+2) ++ (i+3) ++ (i+4) ++ (i+5))
        in
        loop f acc s len (i+6)
      )
      else
        raise (Malformed (Some i, s))
  in
  loop f acc s (String.length s) 0

let iter ?esc f s = fold_left ?esc (fun () x -> f x) () s

let is_supported_unicode ?esc is_valid s =
  let check x =
    if not (is_valid x) then
      raise Exit
  in
  try
    iter ?esc check s;
    true
  with
      Exit
    | Malformed _ -> false

let is_allowed_unicode ?esc s =
  is_supported_unicode ?esc Utf8uni.is_allowed s

let is_allowed_and_assigned_unicode ?esc s =
  is_supported_unicode ?esc Utf8uni.is_allowed_and_assigned s

let is_json_compatible s =
  is_supported_unicode ~esc:json_esc Utf8uni.is_json_compatible s

let add = Buffer.add_char
let encode_char buf x =
  (* Straight <= doesn't work with signed 31-bit ints
     (which are outside of Unicode but supported by UTF-8) *)
  let maxbits n x = x lsr n = 0 in

  if maxbits 7 x then
    (* 7 *)
    add buf (Char.chr x)
  else if maxbits 11 x then (
    (* 5 + 6 *)
    add buf (Char.chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
    add buf (Char.chr (0b10000000 lor (x         land 0b00111111)))
  )
  else if maxbits 16 x then (
    (* 4 + 6 + 6 *)
    add buf (Char.chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)))
  )
  else if maxbits 21 x then (
    (* 3 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )
  else if maxbits 26 x then (
    (* 2 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )
  else (
    assert (maxbits 31 x);
    (* 1 + 6 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
    add buf (Char.chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )

let default_replace _ = 0xFFFD

let fix_unicode ?esc ?(replace = default_replace) is_valid s =
  let buf = Buffer.create (String.length s) in
  let escape =
    match esc with
        None -> (fun _ -> None)
      | Some { escape } -> escape
  in
  try
    iter ?esc (
      fun x ->
        match escape x with
            None ->
              encode_char buf (if is_valid x then x else replace x)
          | Some escaped ->
              Buffer.add_string buf escaped
    ) s;
    Some (Buffer.contents buf)
  with Malformed _ ->
    None

let fix_allowed_unicode ?esc ?replace s =
  fix_unicode ?esc ?replace Utf8uni.is_allowed s

let fix_allowed_and_assigned_unicode ?esc ?replace s =
  fix_unicode ?esc ?replace Utf8uni.is_allowed_and_assigned s

let fix_json_compatible ?replace s =
  fix_unicode ~esc:json_esc ?replace Utf8uni.is_json_compatible s
