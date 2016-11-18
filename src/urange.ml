open Printf

let make = Array.make

let copy = Array.copy

let allow a ranges =
  List.iter (
    fun (mini, maxi) ->
      for i = mini to maxi do
        a.(i) <- true
      done
  ) ranges

let disallow a ranges =
  List.iter (
    fun (mini, maxi) ->
      for i = mini to maxi do
        a.(i) <- false
      done
  ) ranges

let extract_valid_ranges a =
  let acc = ref [] in
  let current = ref None in
  for i = 0 to Array.length a - 1 do
    match !current, a.(i) with
        None, false -> ()
      | Some first, true -> ()
      | None, true -> current := Some i
      | Some first, false -> acc := (first, (i-1)) :: !acc; current := None
  done;
  (match !current with
      Some first -> acc := (first, Array.length a - 1) :: !acc
    | _ -> ());
  List.rev !acc

let test_range (first, last) =
  sprintf "if x < 0x%X then false else if x <= 0x%X then true" first last

let print_filter oc name ranges =
  fprintf oc "\
let %s x =
  " name;
  fprintf oc "%s\n\n"
    (String.concat "\n  else " ((List.map test_range ranges) @ ["false"]))

let noncharacters = [
  0xFDD0, 0xFDEF;
  0xFFFE, 0xFFFF;
  0x1FFFE, 0x1FFFF;
  0x2FFFE, 0x2FFFF;
  0x3FFFE, 0x3FFFF;
  0x4FFFE, 0x4FFFF;
  0x5FFFE, 0x5FFFF;
  0x6FFFE, 0x6FFFF;
  0x7FFFE, 0x7FFFF;
  0x8FFFE, 0x8FFFF;
  0x9FFFE, 0x9FFFF;
  0xAFFFE, 0xAFFFF;
  0xBFFFE, 0xBFFFF;
  0xCFFFE, 0xCFFFF;
  0xDFFFE, 0xDFFFF;
  0xEFFFE, 0xEFFFF;
  0xFFFFE, 0xFFFFF;
  0x10FFFE, 0x10FFFF;
]

let utf16_surrogate_pairs = [
  0xD800, 0xDFFF
]

let private_use_areas = [
  0xE000, 0xF8FF;
  0xF0000, 0xFFFFD;
  0x100000, 0x10FFFD
]

let assigned = [
  0x0000, 0xFFFF;
  0x10000, 0x13FFF;
  0x16000, 0x16FFF;
  0x1B000, 0x1BFFF;
  0x1D000, 0x1FFFF;
  0x20000, 0x2BFFF;
  0x2F000, 0x2FFFF;
  0xE0000, 0xE0FFF;
]

let json_control_characters = [
  0x00, 0x1F;
]

(* TAB, LF, CR, SP *)
let json_whitespace = [
  0x09, 0x0A;
  0x0D, 0x0D;
  0x20, 0x20;
]


let print_allowed oc =
  let a = make 0x110000 true in
  disallow a noncharacters;
  disallow a utf16_surrogate_pairs;
  disallow a private_use_areas;
  let ranges = extract_valid_ranges a in
  print_filter oc "is_allowed" ranges

let print_allowed_and_assigned oc =
  let a = make 0x110000 false in
  allow a assigned;
  disallow a noncharacters;
  disallow a utf16_surrogate_pairs;
  disallow a private_use_areas;
  let ranges = extract_valid_ranges a in
  print_filter oc "is_allowed_and_assigned" ranges

let print_json_compatible oc =
  let a = make 0x110000 true in
  disallow a noncharacters;
  disallow a utf16_surrogate_pairs;
  disallow a private_use_areas;
  disallow a json_control_characters;
  let ranges = extract_valid_ranges a in
  print_filter oc "is_json_string_compatible" ranges;
  allow a json_whitespace;
  let ranges = extract_valid_ranges a in
  print_filter oc "is_json_compatible" ranges

let main () =
  let oc = stdout in
  fprintf oc "(* Auto-generated from urange.ml *)\n\n";
  print_allowed oc;
  print_allowed_and_assigned oc;
  print_json_compatible oc

let () = main ()
