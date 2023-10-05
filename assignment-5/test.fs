let merge (lst1, lst2) =
    let rec aux l1 l2 acc =
        match (l1, l2) with
        | e1 :: tail1, e2 :: _ when e1 < e2 -> aux tail1 l2 (e1 :: acc)
        | e1 :: _, e2 :: tail2 when e1 >= e2 -> aux l1 tail2 (e2 :: acc)
        | l, [] | [], l -> (acc |> List.rev) @ l 
        | _ -> acc |> List.rev
    aux lst1 lst2 []
    