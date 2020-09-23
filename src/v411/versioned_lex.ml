open Parser

type result = {
  lexeme : string;
  start: int;
  length: int
}

let strings analyse text = 
  let strings = ref [] in
  analyse text begin fun ~token ~lexeme:_ ~start ~length ~lexbuf:_ ->
    match [@warning "-4"] token with
      | STRING (s, _, _) ->
        strings := {lexeme = s; start = start; length = length} :: !strings;
        None
      | _ -> None
  end;
  (*List.rev*) !strings