module ExnSet = Set.Make(Exn);

let raisesLibTable = {
  let table = Hashtbl.create(15);
  open Exn;
  [
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
    ("Char", [("chr", [invalidArgument])]),
    (
      "Filename",
      [
        ("chop_extension", [invalidArgument]),
        ("temp_file", [sysError]),
        ("open_temp_file", [sysError]),
      ],
    ),
    ("Hashtbl", [("find", [notFound])]),
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
      "Pervasives",
      [
        ("invalid_arg", [invalidArgument]),
        ("failwith", [failure]),
        ("/", [divisionByZero]),
        ("mod", [divisionByZero]),
        ("char_of_int", [invalidArgument]),
        ("bool_of_string", [invalidArgument]),
        ("int_of_string", [failure]),
        ("float_of_string", [failure]),
        ("read_int", [failure]),
        ("output", [invalidArgument]),
        ("close_out", [sysError]),
        ("input_char", [endOfFile]),
        ("input_line", [endOfFile]),
        ("input", [invalidArgument]),
        ("really_input", [endOfFile, invalidArgument]),
        ("really_input_string", [endOfFile]),
        ("input_byte", [endOfFile]),
        ("input_binary_int", [endOfFile]),
        ("close_in", [sysError]),
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

let q = Pervasives.fst;