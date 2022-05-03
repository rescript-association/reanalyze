open Compilerlibs406

(* Remove code annotated with @dead *)

let hasDeadAnnotation attributes =
  attributes
  |> List.exists (fun (({txt}, _) : Parsetree.attribute) -> txt = "dead")

let rec filter_map l ~f =
  match l with
  | [] -> []
  | x :: rest -> (
    match f x with
    | None -> filter_map rest ~f
    | Some y -> y :: filter_map rest ~f)

let value_binding_list mapper value_bindings =
  value_bindings
  |> filter_map ~f:(fun (value_binding : Parsetree.value_binding) ->
         if value_binding.pvb_attributes |> hasDeadAnnotation then None
         else
           Some (Ast_mapper.default_mapper.value_binding mapper value_binding))

let structure mapper structure =
  structure
  |> filter_map ~f:(fun (structure_item : Parsetree.structure_item) ->
         match structure_item.pstr_desc with
         | Pstr_value (rec_value, value_bindings) ->
           let value_bindings = value_binding_list mapper value_bindings in
           if value_bindings = [] then None
           else
             Some
               {
                 structure_item with
                 pstr_desc = Pstr_value (rec_value, value_bindings);
               }
         | Pstr_primitive {pval_attributes} ->
           if pval_attributes |> hasDeadAnnotation then None
           else
             Some
               (Ast_mapper.default_mapper.structure_item mapper structure_item)
         | Pstr_exception x ->
           if x.pext_attributes |> hasDeadAnnotation then None
           else
             Some
               (Ast_mapper.default_mapper.structure_item mapper structure_item)
         | _ ->
           Some (Ast_mapper.default_mapper.structure_item mapper structure_item))

let signature mapper signature =
  signature
  |> filter_map ~f:(fun (signature_item : Parsetree.signature_item) ->
         let test =
           match signature_item.psig_desc with
           | Psig_value {pval_attributes} ->
             not (pval_attributes |> hasDeadAnnotation)
           | _ -> true
         in
         if test then
           Some (Ast_mapper.default_mapper.signature_item mapper signature_item)
         else None)

let () =
  Ast_mapper.register "DeadPPX" (fun _argv ->
      {Ast_mapper.default_mapper with signature; structure})
