reverse list = helper list []
    where
        helper [] bs = bs
        helper (a:as) bs = helper as (a:bs)
