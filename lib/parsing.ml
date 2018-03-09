(** Posix parser, borrowed from Re *)

exception Parse_error
exception Not_supported

let parse s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let get () = let r = s.[!i] in incr i; r in
  let unget () = decr i in

  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept '|' then regexp' (Regex.alt left (branch ()))
    else if accept '&' then regexp' (Regex.inter left (branch ()))
    else left
  and branch () = branch' []
  and branch' left =
    if eos () || test '|' || test '&' || test ')' then Regex.concat (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*' then Regex.star r else
    if accept '+' then Regex.plus r else
    if accept '?' then Regex.opt r else
    if accept '{' then
      match integer () with
      | Some i ->
        let j = if accept ',' then integer () else Some i in
        if not (accept '}') then raise Parse_error;
        begin match j with
            Some j when j < i -> raise Parse_error | _ -> ()
        end;
        raise Not_supported
        (* Regex.rep i j r *)
      | None ->
        unget (); r
    else
      r
  and atom () =
    if accept '.' then begin
      raise Not_supported
      (* if newline then Re.notnl else Re.any *)
    end else if accept '(' then begin
      let r = regexp () in
      if not (accept ')') then raise Parse_error;
      r
    end else
    if accept '^' then begin
      raise Not_supported
      (* if newline then Re.bol else Re.bos *)
    end else if accept '$' then begin
      raise Not_supported
      (* if newline then Re.eol else Re.eos *)
    end else if accept '[' then begin
      if accept '^' then
        Regex.compl (Regex.char '\n')
      else
        raise Not_supported
        (* Regex.charset (bracket []) *)
    end else
    if accept '\\' then begin
      if eos () then raise Parse_error;
      match get () with
        '|' | '&' | '(' | ')' | '*' | '+' | '?'
      | '[' | '.' | '^' | '$' | '{' | '\\' as c -> Regex.char c
      |                 _                       -> raise Parse_error
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      |                 c            -> Regex.char c
    end
  and integer () =
    if eos () then None else
      match get () with
      '0'..'9' as d -> integer' (Char.code d - Char.code '0')
      |     _        -> unget (); None
  and integer' i =
    if eos () then Some i else
      match get () with
      '0'..'9' as d ->
        let i' = 10 * i + (Char.code d - Char.code '0') in
        if i' < i then raise Parse_error;
        integer' i'
      | _ ->
        unget (); Some i
  (* and bracket s =
   *   if s <> [] && accept ']' then s else begin
   *     let c = char () in
   *     if accept '-' then begin
   *       if accept ']' then c :: '-' :: s else begin
   *         let c' = char () in
   *         match Regex.enumerate c c' with
   *         | None -> raise Parse_error
   *         | Some l -> bracket (l @ s)
   *       end
   *     end else
   *       bracket (c :: s)
   *   end
   * and char () =
   *   if eos () then raise Parse_error;
   *   let c = get () in
   *   if c = '[' then begin
   *     if accept '=' then raise Not_supported
   *     else if accept ':' then begin
   *       raise Not_supported (\*XXX*\)
   *     end else if accept '.' then begin
   *       if eos () then raise Parse_error;
   *       let c = get () in
   *       if not (accept '.') then raise Not_supported;
   *       if not (accept ']') then raise Parse_error;
   *       c
   *     end else
   *       c
   *   end else
   *     c *)
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

let parse s =
  try Ok (parse s) with
  | Parse_error -> Error `Parse_error
  | Not_supported -> Error `Not_supported
