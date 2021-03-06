orig = [1 .. 3]
one = [ i*2 | i <- orig ]
two = [ i*3+1 | i <- orig ]
three = orig
four = [ 100-i | i <- orig ]

lists = [one,two,three,four]

inverse_list ([]:as) = []
inverse_list as = [ head(i) | i <- as] : inverse_list ([ tail (i) | i <- as])

multimap func list_of_lists = (map func (inverse_list list_of_lists))

cartesian_product [] = [[]]
cartesian_product (a:as) = [ (i:is) | i <- a, is <- cartesian_product(as) ]

result = multimap sum lists
