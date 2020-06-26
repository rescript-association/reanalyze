module StringMap = Map.Make(String);

module Kind = {
  type t =
    | Arrow(array(t), t)
    | Star
    | Tuple(list(t));

  let rec toString = k =>
    switch (k) {
    | Star => "*"
    | Tuple(ks) =>
      "(" ++ (ks |> List.map(toString) |> String.concat(", ")) ++ ")"
    | Arrow(arr, t) =>
      (Tuple(arr |> Array.to_list) |> toString) ++ " => " ++ (t |> toString)
    };

  let extractDeclTypes = typ => {
    let rec extract = (acc, typ: Types.type_expr) =>
      switch (typ.desc) {
      | Tlink(t)
      | Tsubst(t) => t |> extract(acc)
      | Tarrow(lbl, t1, t2, _) => t2 |> extract([(lbl, t1), ...acc])
      | _ => (List.rev(acc), typ)
      };
    typ |> extract([]);
  };

  let rec fromType = (typ: Types.type_expr) =>
    switch (typ.desc) {
    | Tlink(t)
    | Tsubst(t) => t |> fromType
    | Tarrow(_) =>
      let (declTypes, retType) = typ |> extractDeclTypes;
      Arrow(
        declTypes |> List.map(((_lbl, t)) => t |> fromType) |> Array.of_list,
        retType |> fromType,
      );
    | Ttuple(ts) => Tuple(ts |> List.map(fromType))
    | _ => Star
    };
};

type const =
  | I32(int32)
  | F64(string);

type offset = int;

type instr =
  | Call(string)
  | Const(const)
  | LocalGet(offset)
  | LocalDecl(offset)
  | LocalSet(offset)
  | Param(offset)
  | F64Add
  | F64Mul
  | I32Add;

type id = string;

type scope =
  | Local(offset)
  | Tuple(list(scope));

module Def = {
  type t = {
    id,
    kind: Kind.t,
    loc: Location.t,
    mutable body: list(instr),
    mutable params: list((Ident.t, scope)),
    mutable nextOffset: int,
  };

  let create = (~id, ~kind, ~loc) => {
    id,
    kind,
    loc,
    body: [],
    params: [],
    nextOffset: 0,
  };
  let emit = (~instr, def) => def.body = [instr, ...def.body];
};

module Env = {
  type id = string;
  type t = StringMap.t(scope);

  let addFunctionParameter = (~id, ~scope, env: t) => {
    env |> StringMap.add(id, scope);
  };

  let find = (~id, env: t) => env |> StringMap.find_opt(id);

  let create = (): t => StringMap.empty;
};

let constToString = const =>
  switch (const) {
  | I32(i) => "i32.const " ++ Int32.to_string(i)
  | F64(s) => "f64.const " ++ s
  };

let instrToString = instr => {
  "("
  ++ (
    switch (instr) {
    | Call(s) => "call " ++ s
    | Const(const) => constToString(const)
    | F64Add => "f64.add"
    | F64Mul => "f64.mul"
    | I32Add => "i32.add"
    | LocalDecl(n) => "local " ++ string_of_int(n)
    | LocalGet(n) => "local.get " ++ string_of_int(n)
    | LocalSet(n) => "local.set " ++ string_of_int(n)
    | Param(n) => "param " ++ string_of_int(n)
    }
  )
  ++ ")";
};

let defs: Hashtbl.t(string, Def.t) = Hashtbl.create(1);

let createDef = (~id, ~loc, ~kind) => {
  let id = Ident.name(id);
  let def = Def.create(~id, ~loc, ~kind);
  Hashtbl.replace(defs, id, def);
  def;
};

let findDef = (~id) => Hashtbl.find_opt(defs, id);

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
         "%s %s %s@.",
         switch (def.kind) {
         | Arrow(_) => "func"
         | _ => "global"
         },
         def.id,
         def.body |> List.rev_map(instrToString) |> String.concat(" "),
       )
     );
};
