let merge l1 l2 =
    let rec loop l1 l2 acc =
        match l1, l2 with
        | [], _ -> acc @ l2
        | _, [] -> acc @ l1
        | h1 :: t1, h2 :: t2 ->
            let remainingList1, remainingList2, takenHead = if h1 <= h2 then t1, l2, h1 else l1, t2, h2
            loop remainingList1 remainingList2 (acc @ [ takenHead ])
    loop l1 l2 []

merge [ 1; 2; 3; 4 ] [ -1; 0; 1; 1 ] = [ -1; 0; 1; 1; 1; 2; 3; 4 ]
