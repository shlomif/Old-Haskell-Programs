module Url where

data Mode = Cgi | Harddisk | Server

data Url url is_dir mode = MakeUrl url is_dir mode

url_get_url (MakeUrl url is_dir mode) = url

url_get_is_dir (MakeUrl url is_dir mode) = is_dir

url_get_mode (MakeUrl url is_dir mode) = mode

type Dir = String
type Dirs = [String]

instance Eq Mode where
    Cgi == Cgi = True
    Harddisk == Harddisk = True
    Server == Server = True
    a == b = False

instance Show Mode where
    show Cgi = "Cgi"
    show Harddisk = "Harddisk"
    show Server = "Server"

instance (Show a, Show b, Show c) => Show (Url a b c) where
    show (MakeUrl url is_dir mode) =
        "Url (url = " ++ (show url) ++ ", " ++
        "is_dir = " ++ (show is_dir) ++ ", " ++
        "mode = " ++ (show mode) ++ ")"

type UrlType = Url Dirs Bool Mode

join :: String -> [String] -> String
join separator [] = ""
join separator (a:[]) = a
join separator (a:as) = a ++ separator ++ (join separator as)

fst3 (a,b,c) = a
snd3 (a,b,c) = b
third3 (a,b,c) = c

remove_common :: (Dirs,Dirs,Dir) -> (Dirs,Dirs,Dir)
remove_common ([],a,last) = ([],a,last)
remove_common (b,[],last) = (b,[],last)
remove_common (a:as,b:bs,last) = if a == b
                                 then (remove_common (as,bs,a))
                                 else (a:as,b:bs,last)


get_relative_url :: (Url Dirs Bool Mode) -> (Url Dirs Bool Mode) -> Bool -> String
get_relative_url base to slash_terminated = ret_value where
    this_url = (url_get_url base)
    other_url = (url_get_url to)
    uncommon :: (Dirs,Dirs,Dir)
    uncommon = (remove_common (this_url,other_url,""))

    this_uncommon = (fst3 uncommon)
    other_uncommon_proto = (snd3 uncommon)
    last_in_other = (third3 uncommon)

    index_dot_html :: Dirs
    index_dot_html = if ((url_get_mode base) == Harddisk) && (url_get_is_dir to)
                     then [ "index.html" ]
                     else []

    other_uncommon = other_uncommon_proto ++ index_dot_html

    ret_value = if slash_terminated
                then (if ((length this_uncommon) == 0) && ((length other_uncommon) == 0)
                      then "./"
                      else ret_slash_term)
                else ret_not_slash_term

    ret_slash_term = (join "/"
        ((map (\x -> "..") this_uncommon) ++ other_uncommon)
                     ) ++ (if (url_get_is_dir to) && (not ((url_get_mode base) == Harddisk))
                           then "/"
                           else "")

    ret_not_slash_term = "./" ++ (join "/" components) ++ mysuffix where
        components = (map (\x -> "..") (tail_this_uncommon)) ++
                        one_before ++
                        other_uncommon
        tail_this_uncommon = if (length this_uncommon == 0)
                             then []
                             else (tail this_uncommon)
        one_before :: Dirs
        one_before = (if ((url_get_mode base) == Cgi) &&
                         ((length other_uncommon) == 0) &&
                         ((length this_url) < (length other_url))
                      then [ last_in_other ]
                      else [ ]
                     )

        mysuffix = (if (
                            (url_get_is_dir to) &&
                            (not ((url_get_mode base) == Harddisk)) &&
                            (length(components) > 0)
                       )
                    then "/"
                    else ""
                   )



test_base = (MakeUrl ["regexps", "hello", "index.html"] True Harddisk)
test_to = (MakeUrl ["regexps", "you", "roung"] True Harddisk)
test_st = False

test_result = get_relative_url test_base test_to test_st



