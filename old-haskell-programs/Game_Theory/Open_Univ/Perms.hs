
gradual_transfer :: [a] -> [a] -> [([a],[a])]
gradual_transfer (a:[]) ps = [((a:[]),ps)]
gradual_transfer (a:as) ps = ((a:as),ps):(gradual_transfer as (a:ps))

dump :: [a] -> [a] -> [a]
dump [] as = as
dump (p:ps) as = dump ps (p:as)

gen_perms :: [a] -> [[a]]

gen_perms [] = [[]]

gen_perms set = [ (a:rest) | 
                        (a:as,ps) <- (gradual_transfer set []), 
                        rest <- gen_perms(dump ps as)
                ]
