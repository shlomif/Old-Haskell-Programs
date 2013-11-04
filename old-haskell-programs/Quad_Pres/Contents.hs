module Contents where

import Contents_Abs

contents = (make_contents_dir_with_images "" "Contents"
            [
                (make_contents_doc "intro.html" "Introduction"),
                (make_contents_doc "properties.html" "Properties"),
                (make_contents_dir "recursion" "Recursion"
                [
                    (make_contents_doc "fib1.html" "Fibonnaci (Take 1)"),
                    (make_contents_doc "qsort.html" "Quick Sort")
                ]
                ),
                (make_contents_dir "lazy_eval" "Lazy Evaluation"
                [
                    (make_contents_doc "primes1.html" "Primes (Take 1)"),
                    (make_contents_doc "fib2.html" "Fibonnaci (Take 2)")
                ]
                )
            ]
            [
                (make_image "style.css" "text/css")
            ]
            )
