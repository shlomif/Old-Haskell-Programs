import GHC.Exts

fac :: Int -> Int
fac = \n -> case n of
                 I# n# -> case work n# of
                          x# -> I# x#

work :: Int# -> Int#
work = \n# -> case n# of
              0# -> 1#
              n# -> case n# -# 1# of
                         x# -> case work x# of
                                    y# -> n# *# y#

