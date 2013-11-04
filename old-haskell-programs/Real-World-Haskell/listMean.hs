mean :: [Double] -> Maybe Double

mean [] = Nothing
mean x = Just ((sum x) / (fromIntegral (length x)))
