
gradual_transfer :: [a] -> [a] -> [([a],[a])]
gradual_transfer (a:[]) ps = [((a:[]),ps)]
gradual_transfer (a:as) ps = ((a:as),ps):(gradual_transfer as (a:ps))

gen_perms :: [a] -> [[a]]

gen_perms [] = [[]]

-- The reverse is not absolutely needed, but it does help maintain a more
-- consistent order. I'd like to find a way to get rid of the ++, though
gen_perms set = [ (a:rest) | (a:as,ps) <- (gradual_transfer set []), rest <- gen_perms(reverse(ps) ++ as)]
