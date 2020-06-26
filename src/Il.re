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

type funDef = {
  id,
  kind: Kind.t,
  loc: Location.t,
  mutable body: list(instr),
  mutable params: list((Ident.t, scope)),
  mutable nextOffset: int,
};

type globalDef = {
  id,
  const,
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
  };
  let emit = (~instr, def) => def.body = [instr, ...def.body];
};

module Env = {
  type id = string;
  type t = StringMap.t(def);

  let add = (~id, ~def, env: t) => {
    env |> StringMap.add(id, def);
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

  let dump = (~ppf, env) => {
    Format.fprintf(ppf, "@.Dump Environment@.");
    env
    |> StringMap.iter((_id, scope) => {
         switch (scope) {
         | FunDef(funDef) =>
           Format.fprintf(
             ppf,
             "@.%s %s %s@.",
             switch (funDef.kind) {
             | Arrow(_) => "func"
             | _ => "global"
             },
             funDef.id,
             funDef.body |> List.rev_map(instrToString) |> String.concat(" "),
           )
         | GlobalDef({id, const}) =>
           Format.fprintf(ppf, "@.global %s %s@.", id, const |> constToString)
         | LocalScope(_) => assert(false)
         }
       });
  };

  let find = (~id, env: t) => env |> StringMap.find_opt(id);

  let create = (): t => StringMap.empty;
};
