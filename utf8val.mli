(** UTF-8 Validator *)

val is_utf8 : string -> bool
  (** Return true if and only if the given string is follows a
      valid UTF-8 encoding.
      This function does not check whether the code points are valid
      or representable Unicode characters. *)

exception Malformed of int option * string
(** Exception raised by {!fold_left} and {!iter}.
    The first argument is the position where the error occurred,
    position in the string which comes as the second argument. *)

val fold_left : ('a -> int -> 'a) -> 'a -> string -> 'a
  (** Like [List.fold_left], but iterate over the Unicode code points.
      @raise Malformed if the input is malformed UTF-8 *)

val iter : (int -> unit) -> string -> unit
  (** Like [List.iter], but iterate over the Unicode code points.
      @raise Malformed if the input is malformed UTF-8 *)

val is_supported_unicode : (int -> bool) -> string -> bool
  (** In addition to UTF-8 validation, check the validity of
      each code point using the given predicate. *)

val is_allowed_unicode : string -> bool
  (** Check whether the string is valid UTF-8 and contains Unicode code points
      that are not forbidden and not in private use areas
      (all code points except private use areas).

      This filter allows any string that might represent valid Unicode
      now or in the future.
  *)

val is_allowed_and_assigned_unicode : string -> bool
  (** Check whether the string is valid UTF-8 and contains Unicode code points
      that are not forbidden, that are not in private use areas,
      and that were assigned by the Unicode consortium
      (assigned ranges within planes 0, 1, 2 and 14).

      This filter allows any string that represents valid Unicode "today", but
      exclude ranges that are not yet assigned.
  *)

val fix_unicode : (int -> bool) -> (int -> int) -> string -> string option
  (** [fix_unicode is_valid replace s] rewrites the given UTF-8 string [s],
      replacing invalid code points using the [replace] function.
      The return value is [None] if the input string is not
      even properly UTF-8-encoded. *)

val default_replace : int -> int
  (** The default [replace] function. It always returns 0xfffd
      which is the code point for the so-called replacement character. *)

val fix_allowed_unicode :
  ?replace:(int -> int) -> string -> string option
  (** Specialized version of [fix_unicode], using {!Utf8uni.is_allowed}.
      @param replace defaults to {!default_replace} *)

val fix_allowed_and_assigned_unicode :
  ?replace:(int -> int) -> string -> string option
  (** Specialized version of [fix_unicode],
      using {!Utf8uni.is_allowed_and_assigned}.
      @param replace default to {!default_replace} *)
