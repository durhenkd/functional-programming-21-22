
(* fisierul meu*)

(*  E 8.14.8 Write a function called countChar which counts how many times a given string contains
            a character*)

fun countChar (c: char) (s: string) : int = 
    foldl (fn (x, s) => x + s ) 0 (map (fn x => if x = c then 1 else 0) (explode s));

(*  E 8.14.9 Write a function called addBigs which adds two big numbers given as lists of digits. E.g.: *)

fun addBigs (a: int list) (b: int list) : int list = 
    let 
    fun u (nr : int) = nr mod 10

    fun helper (ta: int list) (tb: int list) (c: int) (res: int list) = 
        if (ta = [] andalso tb = []) then res 
        else if (ta <> [] andalso tb = []) then if c = 1 then
                            if (hd ta) + 1 > 9 then 
                                helper (tl ta) [] 1 ((u ((hd ta) + 1))::res)
                            else
                                helper (tl ta) [] 0 (((hd ta) + 1)::res)
                        else
                            helper (tl ta) [] 0 ((hd ta)::res)
        else if ta = [] andalso tb <> [] then if c = 1 then
                            if (hd tb) + 1 > 9 then
                                helper [] (tl tb) 1 ((u ((hd tb) + 1))::res)
                            else
                                helper [] (tl tb) 0 (((hd tb) + 1)::res)
                        else
                            helper [] (tl tb) 0 ((hd tb)::res)
                    else if c = 1 then
                                if (hd ta) + (hd tb) + 1 > 9 then
                                    helper (tl ta) (tl tb) 1 ((u ((hd ta) + (hd tb) + 1))::res)
                                else
                                    helper (tl ta) (tl tb) 0 (((hd ta)+(hd tb)+1)::res)
                            else
                                if (hd ta) + (hd tb) > 9 then
                                    helper (tl ta) (tl tb) 1 ((u ((hd ta) + (hd tb)))::res)
                                else
                                    helper (tl ta) (tl tb) 0 (((hd ta) + (hd tb))::res)
    in
        helper (rev a) (rev b) 0 []
    end;

(*  E 8.14.10 Write a function called toposort which topologically sorts a dag given as parameter.*)
