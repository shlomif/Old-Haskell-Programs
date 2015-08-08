

gen_perms :: [a] -> [[a]]

gen_perms [] = [[]]
gen_perms set =  helper set [] where
    helper [] ps = []
    helper (a:as) ps = [ (a:rest) | rest <- gen_perms(reverse(ps) ++ as) ] ++
                       (helper as (a:ps))
