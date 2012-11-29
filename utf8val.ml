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
