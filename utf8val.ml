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

let fold_left f acc s =

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

  let rec loop f acc s len i =
    if i = len then acc
    else
      let x = Char.code (String.unsafe_get s i) in
      if x lsr 7 = 0 then (
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

let iter f s = fold_left (fun () x -> f x) () s

let unicode_private_use_area x =
  x >= 0xE000 && x <= 0xF8FF
  || x >= 0xF0000 && x <= 0xFFFFD
  || x >= 0x100000 && x <= 0x10FFFD

let unicode_assigned x =
  (x >= 0x00000 && x <= 0x13FFF
   || x >= 0x16000 && x <= 0x16FFF
   || x >= 0x1B000 && x <= 0x1BFFF
   || x >= 0x1D000 && x <= 0x2BFFF
   || x >= 0x2F000 && x <= 0x2FFFF
   || x >= 0xE0000 && x <= 0xE0FFF)
  && not (unicode_private_use_area x)

let unicode_public x =
  x >= 0x00000 && x <= 0xEFFFF
  && not (unicode_private_use_area x)

let is_unicode f s =
  let check x =
    if not (f x) then
      raise Exit
  in
  try
    iter check s;
    true
  with
      Exit
    | Malformed _ -> false

let is_assigned_unicode s =
  is_unicode unicode_assigned s

let is_public_unicode s =
  is_unicode unicode_public s