let rec merge l1 l2 =
    match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | h1 :: t1, h2 :: t2 -> if h1 > h2 then h2 :: (merge l1 t2) else h1 :: (merge l2 t1)

merge [ 1; 2; 3; 4 ] [ -1; 0; 1; 1 ] = [ -1; 0; 1; 1; 1; 2; 3; 4 ]
