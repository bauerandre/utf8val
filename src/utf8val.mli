(** UTF-8 Validator *)

type code = int
type pos = int

val is_utf8 : string -> bool
  (** Return true if and only if the given string is follows a
      valid UTF-8 encoding.
      This function does not check whether the code points are valid
      or representable Unicode characters. *)

exception Malformed of pos option * string
(** Exception raised by {!fold_left} and {!iter}.
    The first argument is the position where the error occurred,
    position in the string which comes as the second argument. *)

type esc = {
  esc_char : char;
    (** ASCII character used to introduce an escaped Unicode character,
        typically a backslash. *)
  esc_reader : string -> pos ref -> code list;
    (** Function that reads an escaped Unicode character in the input
        string starting from the given position right after the
        escape character. The position must be updated by this function.
        For instance, when parsing the JSON string ["\u0009"],
        [esc_reader] would be called at position 1 (on 'u'). It would
        return 9 and set the position argument to 6 upon returning.
        @raise Malformed to indicate an invalid input
    *)
  escape : code -> string option;
    (** Function used to escape a Unicode character, if desired.
        In JSON for example, ASCII control characters may not be encoded
        directly in UTF-8 and must be escaped. *)
}

val json_esc : esc
  (** Escape reader suitable for JSON. It reads escaped characters
      in the basic multilingual plane (U+0000-U+FFFF) as well
      as those beyond U+FFFF expressed as a pair of UTF-16 surrogates
      such as ["\uD83D\uDCA9"] for U+1F4A9. *)

val fold_left : ?esc:esc -> ('a -> code -> 'a) -> 'a -> string -> 'a
  (** Like [List.fold_left], but iterate over the Unicode code points.
      @raise Malformed if the input is malformed UTF-8 *)

val iter : ?esc:esc -> (code -> unit) -> string -> unit
  (** Like [List.iter], but iterate over the Unicode code points.
      @raise Malformed if the input is malformed UTF-8 *)

val is_supported_unicode : ?esc:esc -> (code -> bool) -> string -> bool
  (** In addition to UTF-8 validation, check the validity of
      each code point using the given predicate. *)

val is_allowed_unicode : ?esc:esc -> string -> bool
  (** Check whether the string is valid UTF-8 and contains Unicode code points
      that are not forbidden and not in private use areas
      (all code points except private use areas).

      This filter allows any string that might represent valid Unicode
      now or in the future.
  *)

val is_allowed_and_assigned_unicode : ?esc:esc -> string -> bool
  (** Check whether the string is valid UTF-8 and contains Unicode code points
      that are not forbidden, that are not in private use areas,
      and that were assigned by the Unicode consortium
      (assigned ranges within planes 0, 1, 2 and 14).

      This filter allows any string that represents valid Unicode "today", but
      exclude ranges that are not yet assigned.
  *)

val is_json_compatible : string -> bool

val default_replace : code -> code
  (** The default [replace] function. It always returns 0xFFFD
      which is the code point for the so-called replacement character. *)

val fix_unicode :
  ?esc:esc ->
  ?replace:(code -> code) ->
  (code -> bool) -> string -> string option
  (** [fix_unicode is_valid s] rewrites the given UTF-8 string [s],
      replacing invalid code points using the [replace] function.
      The return value is [None] if the input string is not
      even properly UTF-8-encoded.
      @param esc optional unescaping and escaping functions
      @param replace defaults to {!default_replace}
  *)

val fix_allowed_unicode :
  ?esc:esc -> ?replace:(code -> code) -> string -> string option
  (** Specialized version of [fix_unicode], using {!Utf8uni.is_allowed}.
      @param esc optional unescaping and escaping functions
      @param replace defaults to {!default_replace} *)

val fix_allowed_and_assigned_unicode :
  ?esc:esc -> ?replace:(code -> code) -> string -> string option
  (** Specialized version of [fix_unicode],
      using {!Utf8uni.is_allowed_and_assigned}.
      @param esc optional unescaping and escaping functions
      @param replace defaults to {!default_replace} *)

val fix_json_compatible : ?replace:(code -> code) -> string -> string option
  (** Rewrite the input string as if it were UTF-8-encoded JSON.
      Disallowed Unicode code points are replaced using the [replace]
      function.
      Escaped Unicode characters are checked for their validity.
      JSON characters that must be escaped are escaped (control characters,
      backslash and double quotes).
      @param replace defaults to {!default_replace}
  *)
