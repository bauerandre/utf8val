open Printf

let ascii =
  let s = String.create 128 in
  for i = 0 to 127 do
    s.[i] <- Char.chr i
  done;
  s

let sample_strings = [
  "hello", "Hello, world", true;
  "ascii", ascii, true;
]

let special_tests = [
  "a", [| 0b00000000 |], true;
  "b", [| 0b10000000 |], false;
  "c", [| 0b00000000; 0b10000000 |], false;
  "d", [| 0b11000010; 0b10000000 |], true;
  "e", [| 0b11000001; 0b10000000 |], false;
  "f", [| 0b11100000; 0b10100000; 0b10000000 |], true;
  "g", [| 0b11100000; 0b10010000; 0b10000000 |], false;
  "h", [| 0b11110000; 0b10010000; 0b10000000; 0b10000000 |], true;
  "i", [| 0b11110000; 0b10001000; 0b10000000; 0b10000000 |], false;
  "j", [| 0b11111000; 0b10001000; 0b10000000; 0b10000000; 0b10000000 |], true;
  "k", [| 0b11111000; 0b10000100; 0b10000000; 0b10000000; 0b10000000 |], false;
  "l", [| 0b11111100; 0b10000100; 0b10000000; 0b10000000; 0b10000000; 0b10000000 |], true;
  "m", [| 0b11111100; 0b10000010; 0b10000000; 0b10000000; 0b10000000; 0b10000000 |], false;
  "n", [| 0b11111111 |], false;

  (* truncated *)
  "1", [| 0b11000011; |], false;
  "2", [| 0b11100011; 0b10000000 |], false;
  "3", [| 0b11110011; 0b10000000; 0b10000000 |], false;
  "4", [| 0b11111011; 0b10000000; 0b10000000; 0b10000000 |], false;
  "5", [| 0b11111101; 0b10000000; 0b10000000; 0b10000000; 0b10000000 |], false;
]

let special_strings =
  let string_of_array a =
    let s = String.create (Array.length a) in
    for i = 0 to String.length s - 1 do
      s.[i] <- Char.chr a.(i)
    done;
    s
  in
  List.map (
    fun (name, a, valid) ->
      (sprintf "special case (%s)" name, string_of_array a, valid))
    special_tests

let test_strings () =
  List.iter (
    fun (name, s, valid) ->
      if Utf8val.is_utf8 s = valid then
        printf "OK %s\n%!" name
      else
        failwith ("Utf8val test " ^ name ^ " failed")
  ) (sample_strings @ special_strings)

let test_file fname =
  let load fname =
    let ic = open_in fname in
    let buf = Buffer.create 1000 in
    try
      while true do
        bprintf buf "%s\n" (input_line ic)
      done;
      assert false
    with End_of_file ->
      Buffer.contents buf
  in
  if Utf8val.is_utf8 (load fname) then
    printf "OK %s\n%!" fname
  else
    failwith (sprintf "File %s does not contain valid UTF-8" fname)


let main () =
  test_strings ();
  test_file "wikipedia-languages.utf8"

let () = main ()
