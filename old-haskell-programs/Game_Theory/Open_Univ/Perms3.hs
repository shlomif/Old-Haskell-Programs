
-- (gradual_transfer set empty_set)
--
-- Gradually pops elements out of set and [1..5] pushes them into empty_set,
-- and makes a list of both stacks in their intermediate phases.
--
-- E.g:
-- gradual_transfer [1 .. 5] [] =
-- [([1,2,3,4,5],[]),([2,3,4,5],[1]),([3,4,5],[2,1]),
--  ([4,5],[3,2,1]),([5],[4,3,2,1])]
gradual_transfer :: [a] -> [a] -> [([a],[a])]
-- I stop when the list contains a single element, not when it contains
-- no elements at all. The reason for this is that gen_perms like it better
-- this way, as it has no use of a zero element (a:as).
gradual_transfer (a:[]) ps = [((a:[]),ps)]
gradual_transfer (a:as) ps = ((a:as),ps):(gradual_transfer as (a:ps))

-- (dump ps as) is equivalent to (reverse ps) ++ as, only it should
-- be much faster.
dump :: [a] -> [a] -> [a]
dump [] as = as
dump (p:ps) as = dump ps (p:as)

gen_perms :: [a] -> [[a]]

gen_perms [] = [[]]

gen_perms set = [ (a:rest) |
                        (a:as,ps) <- (gradual_transfer set []),
                        rest <- gen_perms(dump ps as)
                ]


print_perms [] = return ()
print_perms (a:as) = do print a
                        print_perms as

main = print_perms (gen_perms [1 .. 8])
