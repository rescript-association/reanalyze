type const =
  | I32(int32);

type instr =
  | Const(const);

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

let dumpConst = (ppf, const) =>
  switch (const) {
  | I32(i) => Format.fprintf(ppf, "i32:%s", Int32.to_string(i))
  };

let dumpInstr = (ppf, instr) =>
  switch (instr) {
  | Const(const) => Format.fprintf(ppf, "const %a", dumpConst, const)
  };

let dumpDefs = (~ppf) => {
  Format.fprintf(ppf, "Noalloc definitions@.");
  Hashtbl.iter(
    (id, def: Def.t) =>
      def.body
      |> List.iter(instr =>
           Format.fprintf(ppf, "%s: %a@.", id, dumpInstr, instr)
         ),
    defs,
  );
};
