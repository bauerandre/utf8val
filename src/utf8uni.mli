(** Unicode code point filters *)

(**
   Functions in this module check whether code points are within valid ranges
   without using large lookup tables.
*)

val is_allowed : int -> bool
  (** Whether the given code point is allowed in publicly-exchanged documents.
      This excludes non-characters and private use areas. *)

val is_allowed_and_assigned : int -> bool
  (** Whether the given code point is allowed and was assigned
      by the Unicode consortium.
      More code points may be assigned in the future. *)

val is_json_compatible : int -> bool
  (** Whether the given code point is allowed in publicly-exchanged documents
      and is allowed unescaped in JSON.
      Characters TAB, LF, and CR which are allowed as whitespace between
      JSON tokens are tolerated, even in string literals where they should
      be escaped. See also {!is_json_string_compatible}. *)

val is_json_string_compatible : int -> bool
  (** Same as {!is_json_compatible} but returns false if any
      TAB, LF, or CR character is found. *)
