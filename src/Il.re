type const =
  | I32(int32);

type instr =
  | Const(const)
  | I32Add;

type id = string;

module Def = {
  type t = {
    id,
    mutable body: list(instr),
  };

  let create = id => {id, body: []};
  let emit = (~instr, def) => def.body = [instr, ...def.body];
};

let defs: Hashtbl.t(string, Def.t) = Hashtbl.create(1);

let newDef = (~id) => {
  let id = Ident.name(id);
  let def = Def.create(id);
  Hashtbl.replace(defs, id, def);
  def;
};

let constToString = const =>
  switch (const) {
  | I32(i) => "i32:" ++ Int32.to_string(i)
  };

let instrToString = instr =>
  switch (instr) {
  | Const(const) => "const " ++ constToString(const)
  | I32Add => "i32.add"
  };

let dumpDefs = (~ppf) => {
  Format.fprintf(ppf, "Noalloc definitions@.");
  Hashtbl.iter(
    (id, def: Def.t) =>
      Format.fprintf(
        ppf,
        "%s: %s@.",
        id,
        def.body |> List.rev_map(instrToString) |> String.concat("; "),
      ),
    defs,
  );
};
