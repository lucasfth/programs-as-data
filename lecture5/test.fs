let merge (lst1, lst2) =
    let rec aux l1 l2 acc =
        match (l1, l2) with
        | e1 :: tail1, e2 :: tail2 when e1 < e2 -> aux tail1 l2 (acc @ [e1])
        | e1 :: tail1, e2 :: tail2 when e1 > e2 -> aux l1 tail2 (acc @ [e2])
        | l, [] | [], l -> acc @ l
        | _ -> acc
    aux lst1 lst2 [];;