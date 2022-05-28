type t =
  | String of string
  | Number of float
  | Array of t list
  | Object of (string * t) list
  | True
  | False
  | Null

module Parser = struct
  let split_by is_delim str =
    let len = String.length str in
    let rec loop acc last_pos pos =
      if pos = -1 then
        if last_pos = 0 then acc else String.sub str 0 last_pos :: acc
      else if is_delim str.[pos] then
        let new_len = last_pos - pos - 1 in
        if new_len <> 0 then
          let v = String.sub str (pos + 1) new_len in
          loop (v :: acc) pos (pos - 1)
        else loop acc pos (pos - 1)
      else loop acc last_pos (pos - 1)
      [@@raises Invalid_argument]
    in
    loop [] len (len - 1)
    [@@raises Invalid_argument]

  let fail text pos message =
    let pre = String.sub text 0 pos in
    let lines = split_by (fun c -> c = '\n') pre in
    let count = List.length lines in
    let last =
      match count > 0 with true -> List.nth lines (count - 1) | false -> ""
    in
    let col = String.length last + 1 in
    let line = List.length lines in
    let string =
      Printf.sprintf "Error \"%s\" at %d:%d -> %s\n" message line col last
    in
    failwith string
    [@@raises [(Failure, Invalid_argument)]]

  let rec skipToNewline text pos =
    if pos >= String.length text then pos
    else if text.[pos] = '\n' then pos + 1
    else skipToNewline text (pos + 1)
    [@@raises Invalid_argument]

  let rec skipToCloseMultilineComment text pos =
    if pos + 1 >= String.length text then failwith "Unterminated comment"
    else if text.[pos] = '*' && text.[pos + 1] = '/' then pos + 2
    else skipToCloseMultilineComment text (pos + 1)
    [@@raises Failure, Invalid_argument]

  let rec skipWhite text pos =
    if
      pos < String.length text
      && (text.[pos] = ' '
         || text.[pos] = '\t'
         || text.[pos] = '\n'
         || text.[pos] = '\r')
    then skipWhite text (pos + 1)
    else pos
    [@@raises Invalid_argument]

  (* from https://stackoverflow.com/a/42431362 *)
  let utf8encode s =
    let prefs = [|0; 192; 224|] in
    let s1 n = String.make 1 (Char.chr n) [@@raises Invalid_argument] in
    let rec ienc k sofar resid =
      let bct = if k = 0 then 7 else 6 - k in
      if resid < 1 lsl bct then s1 (prefs.(k) + resid) ^ sofar
      else
        ienc (k + 1)
          (s1 (128 + (resid mod 64)) ^ sofar)
          (resid / 64) [@doesNotRaise]
      [@@raises Invalid_argument]
    in
    ienc 0 "" (int_of_string ("0x" ^ s))
    [@@raises Failure, Invalid_argument]

  let parseString text pos =
    (* let i = ref(pos); *)
    let buffer = Buffer.create (String.length text) in
    let ln = String.length text in
    let rec loop i =
      match i >= ln with
      | true -> fail text i "Unterminated string"
      | false -> (
        match text.[i] with
        | '"' -> i + 1
        | '\\' -> (
          match i + 1 >= ln with
          | true -> fail text i "Unterminated string"
          | false -> (
            match text.[i + 1] with
            | '/' ->
              Buffer.add_char buffer '/';
              loop (i + 2)
            | 'f' ->
              Buffer.add_char buffer '\012';
              loop (i + 2)
            | 'u' when i + 6 < ln ->
              Buffer.add_string buffer (utf8encode (String.sub text (i + 2) 4));
              loop (i + 7)
            | _ ->
              Buffer.add_string buffer (Scanf.unescaped (String.sub text i 2));
              loop (i + 2)))
        | c ->
          Buffer.add_char buffer c;
          loop (i + 1))
      [@@raises Failure, Invalid_argument]
    in
    let final = loop pos in
    (Buffer.contents buffer, final)
    [@@raises Invalid_argument]

  let parseDigits text pos =
    let len = String.length text in
    let rec loop i =
      if i >= len then i
      else match text.[i] with '0' .. '9' -> loop (i + 1) | _ -> i
      [@@raises Invalid_argument]
    in
    loop (pos + 1)
    [@@raises Invalid_argument]

  let parseWithDecimal text pos =
    let pos = parseDigits text pos in
    if pos < String.length text && text.[pos] = '.' then
      let pos = parseDigits text (pos + 1) in
      pos
    else pos
    [@@raises Invalid_argument]

  let parseNumber text pos =
    let pos = parseWithDecimal text pos in
    let ln = String.length text in
    if pos < ln - 1 && (text.[pos] = 'E' || text.[pos] = 'e') then
      let pos =
        match text.[pos + 1] with '-' | '+' -> pos + 2 | _ -> pos + 1
      in
      parseDigits text pos
    else pos
    [@@raises Invalid_argument]

  let parseNegativeNumber text pos =
    let final =
      if text.[pos] = '-' then parseNumber text (pos + 1)
      else parseNumber text pos
    in
    (Number (float_of_string (String.sub text pos (final - pos))), final)
    [@@raises Failure, Invalid_argument]

  let expect char text pos message =
    if text.[pos] <> char then fail text pos ("Expected: " ^ message)
    else pos + 1
    [@@raises Failure, Invalid_argument]

  let parseComment : 'a. string -> int -> (string -> int -> 'a) -> 'a =
   fun text pos next ->
    if text.[pos] <> '/' then
      if text.[pos] = '*' then
        next text (skipToCloseMultilineComment text (pos + 1))
      else failwith "Invalid syntax"
    else next text (skipToNewline text (pos + 1))
   [@@raises Failure, Invalid_argument]

  let maybeSkipComment text pos =
    if pos < String.length text && text.[pos] = '/' then
      if pos + 1 < String.length text && text.[pos + 1] = '/' then
        skipToNewline text (pos + 1)
      else if pos + 1 < String.length text && text.[pos + 1] = '*' then
        skipToCloseMultilineComment text (pos + 1)
      else fail text pos "Invalid synatx"
    else pos
    [@@raises Failure, Invalid_argument]

  let rec skip text pos =
    if pos = String.length text then pos
    else
      let n = skipWhite text pos |> maybeSkipComment text in
      if n > pos then skip text n else n
    [@@raises Failure, Invalid_argument]

  let rec parse text pos =
    if pos >= String.length text then
      fail text pos "Reached end of file without being done parsing"
    else
      match text.[pos] with
      | '/' -> parseComment text (pos + 1) parse
      | '[' -> parseArray text (pos + 1)
      | '{' -> parseObject text (pos + 1)
      | 'n' ->
        if String.sub text pos 4 = "null" then (Null, pos + 4)
        else fail text pos "unexpected character"
      | 't' ->
        if String.sub text pos 4 = "true" then (True, pos + 4)
        else fail text pos "unexpected character"
      | 'f' ->
        if String.sub text pos 5 = "false" then (False, pos + 5)
        else fail text pos "unexpected character"
      | '\n' | '\t' | ' ' | '\r' -> parse text (skipWhite text pos)
      | '"' ->
        let s, pos = parseString text (pos + 1) in
        (String s, pos)
      | '-' | '0' .. '9' -> parseNegativeNumber text pos
      | _ -> fail text pos "unexpected character"
    [@@raises Failure, Invalid_argument]

  and parseArrayValue text pos =
    let pos = skip text pos in
    let value, pos = parse text pos in
    let pos = skip text pos in
    match text.[pos] with
    | ',' ->
      let pos = skip text (pos + 1) in
      if text.[pos] = ']' then ([value], pos + 1)
      else
        let rest, pos = parseArrayValue text pos in
        (value :: rest, pos)
    | ']' -> ([value], pos + 1)
    | _ -> fail text pos "unexpected character"
    [@@raises Failure, Invalid_argument]

  and parseArray text pos =
    let pos = skip text pos in
    match text.[pos] with
    | ']' -> (Array [], pos + 1)
    | _ ->
      let items, pos = parseArrayValue text pos in
      (Array items, pos)
    [@@raises Failure, Invalid_argument]

  and parseObjectValue text pos =
    let pos = skip text pos in
    if text.[pos] <> '"' then fail text pos "Expected string"
    else
      let key, pos = parseString text (pos + 1) in
      let pos = skip text pos in
      let pos = expect ':' text pos "Colon" in
      let value, pos = parse text pos in
      let pos = skip text pos in
      match text.[pos] with
      | ',' ->
        let pos = skip text (pos + 1) in
        if text.[pos] = '}' then ([(key, value)], pos + 1)
        else
          let rest, pos = parseObjectValue text pos in
          ((key, value) :: rest, pos)
      | '}' -> ([(key, value)], pos + 1)
      | _ ->
        let rest, pos = parseObjectValue text pos in
        ((key, value) :: rest, pos)
    [@@raises Failure, Invalid_argument]

  and parseObject text pos =
    let pos = skip text pos in
    if text.[pos] = '}' then (Object [], pos + 1)
    else
      let pairs, pos = parseObjectValue text pos in
      (Object pairs, pos)
    [@@raises Failure, Invalid_argument]
end
[@@nodoc]

(** Turns some text into a json object. throws on failure *)
let parse text =
  try
    let item, pos = Parser.parse text 0 in
    let pos = Parser.skip text pos in
    if pos < String.length text then
      (* failwith
         ("Extra data after parse finished: "
         ^ String.sub text pos (String.length text - pos)) *)
      None
    else Some item
  with Invalid_argument _ | Failure _ -> None

(** If `t` is an object, get the value associated with the given string key *)
let get key t =
  match t with
  | Object items -> ( try Some (List.assoc key items) with Not_found -> None)
  | _ -> None

let escape text =
  let ln = String.length text in
  let buf = Buffer.create ln in
  let rec loop i =
    if i < ln then (
      (match text.[i] [@doesNotRaise "bound checked above"] with
      | '\012' -> Buffer.add_string buf "\\f"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf
