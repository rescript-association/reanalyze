type const =
  | I32(int32);

type offset = int;

type instr =
  | Call(string)
  | Const(const)
  | LocalGet(offset)
  | Param(offset)
  | I32Add;

type id = string;

module Def = {
  type t = {
    loc: Location.t,
    id,
    mutable body: list(instr),
    mutable params: list((Ident.t, Types.type_expr)),
  };

  let create = (~loc, ~id) => {loc, id, body: [], params: []};
  let emit = (~instr, def) => def.body = [instr, ...def.body];
};

let defs: Hashtbl.t(string, Def.t) = Hashtbl.create(1);

let createDef = (~loc, ~id) => {
  let id = Ident.name(id);
  let def = Def.create(~loc, ~id);
  Hashtbl.replace(defs, id, def);
  def;
};

let findDef = (~id) => Hashtbl.find_opt(defs, id);

let constToString = const =>
  switch (const) {
  | I32(i) => "i32:" ++ Int32.to_string(i)
  };

let instrToString = instr =>
  switch (instr) {
  | Call(s) => "call " ++ s
  | Const(const) => "const " ++ constToString(const)
  | I32Add => "i32.add"
  | LocalGet(n) => "local.get " ++ string_of_int(n)
  | Param(n) => "param " ++ string_of_int(n)
  };

let dumpDefs = (~ppf) => {
  Format.fprintf(ppf, "Noalloc definitions@.");
  let sortedDefs =
    Hashtbl.fold((_id, def, definitions) => [def, ...definitions], defs, [])
    |> List.sort(({Def.loc: loc1}, {loc: loc2}) =>
         compare(loc1.loc_start.pos_lnum, loc2.loc_start.pos_lnum)
       );

  sortedDefs
  |> List.iter((def: Def.t) =>
       Format.fprintf(
         ppf,
         "%s: %s@.",
         def.id,
         def.body |> List.rev_map(instrToString) |> String.concat("; "),
       )
     );
};
