open Printf

let ascii =
  let s = String.create 128 in
  for i = 0 to 127 do
    s.[i] <- Char.chr i
  done;
  s

let sample_strings = [
  "hello", "Hello, world", true, Some true;
  "ascii", ascii, true, Some true;
  "private-use-area", "bad \238\128\131 ugly \237\160\189\237\184\136",
  true, Some false;
]

let json_validate = [
  "json-escaped-control", "\\u0000", true;
  "json-unescaped-control", "\000", false;
  "json-escaped-newline", "\\n", true;
  "json-unescaped-newline", "\n", true;
  "json-escaped-backslash", "\\\\", true;
  "json-escaped-quote", "\\\"", true;
]

let json_fix = [
  "json-hello", "hello", Some "hello";

  "json-unescaped-utf-8",
  "e, é, ệ, \xF0\x9F\x92\xA9",
  Some "e, \xC3\xA9, \xE1\xBB\x87, \xF0\x9F\x92\xA9";

  "json-escaped-utf-8",
  "\\u0065, \\u00E9, \\u1ec7, \\uD83D\\uDCA9",
  Some "e, \xC3\xA9, \xE1\xBB\x87, \xF0\x9F\x92\xA9";

  "json-misc",
  "\\\\, \\\", \\/, \\n, \\t, \\u000A, \\u0000, \x1F",
  Some "\\\\, \\\", /, \\n, \\t, \\n, \\u0000, \\u001F";

  "json-bad-escape", "\\zzzzzzz", None;

  "json-fix-private-use-area", "[\xEE\x80\x80]", Some "[\xEF\xBF\xBD]";
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

  (* truncated + ascii *)
  "1a", [| 0b11000011; 0b00000000 |], false;
  "2a", [| 0b11100011; 0b10000000; 0b00000000 |], false;
  "3a", [| 0b11110011; 0b10000000; 0b10000000; 0b00000000 |], false;
  "4a", [| 0b11111011; 0b10000000; 0b10000000; 0b10000000; 0b00000000 |], false;
  "5a", [| 0b11111101; 0b10000000; 0b10000000; 0b10000000; 0b10000000; 0b00000000 |], false;
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
    fun (name, a, valid_utf8) ->
      (sprintf "special case (%s)" name, string_of_array a, valid_utf8, None))
    special_tests

let test_strings () =
  List.iter (
    fun (name, s, valid_utf8, opt_valid_unicode) ->
      if Utf8val.is_utf8 s = valid_utf8 then
        printf "OK UTF-8 %s\n%!" name
      else
        failwith ("Utf8val test " ^ name ^ " failed UTF-8 validation");
      match opt_valid_unicode with
          Some valid_unicode ->
            if Utf8val.is_allowed_and_assigned_unicode s = valid_unicode then
              printf "OK Unicode %s\n%!" name
            else
              failwith ("Utf8val test " ^ name ^ " failed Unicode validation")
        | None ->
            ()
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
  if Utf8val.is_allowed_and_assigned_unicode (load fname) then
    printf "OK %s\n%!" fname
  else
    failwith (sprintf "File %s does not contain valid UTF-8" fname)

let test f l =
  List.iter (
    fun (name, arg, expected) ->
      if f arg = expected then
        printf "OK %s\n%!" name
      else
        failwith (sprintf "Test %s failed" name)
  ) l

let main () =
  test_strings ();
  test_file "wikipedia-languages.utf8";
  test Utf8val.is_json_compatible json_validate;
  test Utf8val.fix_json_compatible json_fix

let () = main ()
