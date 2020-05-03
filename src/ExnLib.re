module ExnSet = Set.Make(Exn);

let raisesLibTable = {
  let table = Hashtbl.create(15);
  open Exn;
  [
    (
      "List",
      [
        ("hd", [failure]),
        ("tl", [failure]),
        ("nth", [failure, invalidArgument]),
        ("nth_opt", [invalidArgument]),
        ("init", [invalidArgument]),
        ("iter2", [invalidArgument]),
        ("map2", [invalidArgument]),
        ("fold_left2", [invalidArgument]),
        ("fold_right2", [invalidArgument]),
        ("for_all2", [invalidArgument]),
        ("exists2", [invalidArgument]),
        ("find", [notFound]),
        ("assoc", [notFound]),
        ("combine", [invalidArgument]),
      ],
    ),
    (
      "Array",
      [
        ("get", [invalidArgument]),
        ("set", [invalidArgument]),
        ("make", [invalidArgument]),
        ("init", [invalidArgument]),
        ("make_matrix", [invalidArgument]),
        ("fill", [invalidArgument]),
        ("blit", [invalidArgument]),
        ("iter2", [invalidArgument]),
        ("map2", [invalidArgument]),
      ],
    ),
    (
      "Buffer",
      [
        ("sub", [invalidArgument]),
        ("blit", [invalidArgument]),
        ("nth", [invalidArgument]),
        ("add_substitute", [notFound]),
        ("add_channel", [endOfFile]),
        ("truncate", [invalidArgument]),
      ],
    ),
  ]
  |> List.iter(((name, group)) =>
       group
       |> List.iter(((s, e)) =>
            Hashtbl.add(table, name ++ "." ++ s, e |> ExnSet.of_list)
          )
     );

  table;
};

let find = path => Hashtbl.find_opt(raisesLibTable, path |> Path.name);