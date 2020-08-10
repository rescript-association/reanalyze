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
  | LocalSet(offset)
  | F64Add
  | F64Mul
  | I32Add
  | I32Load(offset)
  | I32Store(offset);

type id = string;

type scope =
  | Local(offset)
  | Tuple(list(scope));

type funDef = {
  id,
  kind: Kind.t,
  loc: Location.t,
  mutable body: list(instr),
  mutable params: list((Ident.t, scope)),
  mutable nextOffset: int,
  mutable numParams: int,
};

let constToString = const =>
  switch (const) {
  | I32(i) => "i32.const " ++ Int32.to_string(i)
  | F64(s) => "f64.const " ++ s
  };

module Init = {
  type t =
    | Const(const)
    | Tuple(list(t));

  let rec toString = i =>
    switch (i) {
    | Const(const) => const |> constToString
    | Tuple(is) =>
      "(" ++ (is |> List.map(toString) |> String.concat(", ")) ++ ")"
    };
};

type globalDef = {
  id,
  init: Init.t,
};

type def =
  | FunDef(funDef)
  | GlobalDef(globalDef)
  | LocalScope(scope);

module FunDef = {
  let create = (~id, ~kind, ~loc) => {
    id,
    kind,
    loc,
    body: [],
    params: [],
    nextOffset: 0,
    numParams: 0,
  };
  let emit = (~instr, def) => def.body = [instr, ...def.body];

  let dumpParams = (ppf, def) => {
    for (i in 0 to def.numParams - 1) {
      Format.fprintf(ppf, "(param %d) ", i);
    };
  };

  let dumpLocalDecls = (ppf, def) => {
    for (i in def.numParams to def.nextOffset - 1) {
      Format.fprintf(ppf, "(local %d) ", i);
    };
  };
};

module Mem = {
  type index = int;
  type data =
    | String({
        index,
        string,
      });
  type t = {
    mutable nextIndex: index,
    mutable dataSegments: list(data),
    strings: Hashtbl.t(string, index),
  };

  let stringAlignment = 4;

  let align = (~alignment, size) =>
    size mod alignment == 0 ? size : 4 + size / 4 * 4;

  let create = () => {
    nextIndex: 0,
    dataSegments: [],
    strings: Hashtbl.create(1),
  };

  let alloc = (mem, ~size): index => {
    let index = mem.nextIndex;
    mem.nextIndex = mem.nextIndex + size;
    index;
  };

  let allocString = (mem, ~string): index => {
    switch (Hashtbl.find_opt(mem.strings, string)) {
    | None =>
      let size =
        1 + String.length(string) |> align(~alignment=stringAlignment);
      let index = mem |> alloc(~size);
      let data = String({index, string});
      mem.dataSegments = [data, ...mem.dataSegments];
      Hashtbl.replace(mem.strings, string, index);
      index;
    | Some(index) => index
    };
  };

  let dump = (~ppf, mem) => {
    Format.fprintf(ppf, "@.Dump Memory@.");
    Format.fprintf(ppf, "(memory $0 %d)@.", mem.nextIndex);
    mem.dataSegments
    |> List.rev
    |> List.iter(data =>
         switch (data) {
         | String({index, string}) =>
           Format.fprintf(
             ppf,
             "(data (i32.const %d) \"%s\\00\")@.",
             index,
             string,
           )
         }
       );
  };
};

module Env = {
  type id = string;
  type t = StringMap.t(def);

  let add = (~id, ~def, env: t) => {
    env |> StringMap.add(id, def);
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
      | I32Load(n) => "i32.load " ++ "offset=" ++ string_of_int(n)
      | I32Store(n) => "i32.store " ++ "offset=" ++ string_of_int(n)
      | LocalGet(n) => "local.get " ++ string_of_int(n)
      | LocalSet(n) => "local.set " ++ string_of_int(n)
      }
    )
    ++ ")";
  };

  let dump = (~ppf, env) => {
    Format.fprintf(ppf, "@.Dump Environment@.");
    env
    |> StringMap.iter((_id, scope) => {
         switch (scope) {
         | FunDef(funDef) =>
           Format.fprintf(
             ppf,
             "@.%s %s %a%a%s@.",
             switch (funDef.kind) {
             | Arrow(_) => "func"
             | _ => "global"
             },
             funDef.id,
             FunDef.dumpParams,
             funDef,
             FunDef.dumpLocalDecls,
             funDef,
             funDef.body |> List.rev_map(instrToString) |> String.concat(" "),
           )
         | GlobalDef({id, init}) =>
           Format.fprintf(ppf, "@.global %s %s@.", id, init |> Init.toString)
         | LocalScope(_) => assert(false)
         }
       });
  };

  let find = (~id, env: t) => env |> StringMap.find_opt(id);

  let create = (): t => StringMap.empty;
};
