



let try_function (t1: Ast.typ) (t2 : Ast.typ) = 
  let try_t2 = 
    match t2 with
      | Ast.Function (t2_in, t2_out) ->
        if t2_in = t1 then Some t2_out else None
      | _ -> None
      in 
  match t1 with
  | Ast.Function (t1_in, t1_out) ->
    if t1_in = t2 then Some t1_out else try_t2
  | _ -> try_t2
      

let guess_type (types : Ast.typ option list) = 
  let types = List.filter (fun t -> t <> None) types in
  match types with
  | [] -> None
  | [t] -> t
  | [t1; t2] -> 
    (match t1, t2 with
    | Some t1, Some t2 -> try_function t1 t2
    | _ -> None)
  | _ -> None

let get_type (tree : Ast.tree_node) : Ast.typ option = 
  match tree with
  | Leaf (_, t) -> t
  | Node (_, t) -> t

let rec synthesize_types (lexicon: Ast.lexicon) (tree : Ast.tree) : Ast.tree_node = 
  match tree with
  | Leaf (s, _) -> Leaf (s, (Ast.LexiconMap.find_opt s lexicon))
  | Node ({ symbol ; children }, _) ->
    let typed_children = List.map (synthesize_types lexicon) children in

    Node ({ symbol ; children = typed_children }, (guess_type (List.map get_type typed_children)))


let () = synthesize_types Ast.LexiconMap.empty (Ast.Node ({ symbol = "S" ; children = [Ast.Leaf ("a", None) ; Ast.Leaf ("b", None)] }, None)) |> ignore