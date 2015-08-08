module QuadPres where

import IO

import Url
import Contents_Abs
import Contents


navigation_style_class = "nav"
contents_style_class = "contents"

data QuadPresCoords coords doc_type mime =
    MakeQuadPresCoords coords doc_type mime

get_coords_coords (MakeQuadPresCoords coords doc_type mime) = coords
get_coords_doc_type (MakeQuadPresCoords coords doc_type mime) = doc_type
get_coords_mime (MakeQuadPresCoords coords doc_type mime) = mime

data QuadPresCoordsDocType = Image | Node

instance Eq QuadPresCoordsDocType where
    Image == Image = True
    Node == Node = True
    a == b = False

instance Show QuadPresCoordsDocType where
    show Image = "Image"
    show Node = "Node"


type QuadPresCoordsType = QuadPresCoords [Int] QuadPresCoordsDocType String

instance (Show a, Show b, Show c) => Show (QuadPresCoords a b c) where
    show (MakeQuadPresCoords coords doc_type mime) =
        "QuadPresCoords (coords = " ++ (show coords) ++ ", " ++
        "doc_type = " ++ (show doc_type) ++ ", " ++
        "mime = " ++ (show mime) ++ ")"

data QuadPres contents doc_id mode
    doc_id_slash_terminated coords =
        MakeQuadPres contents doc_id mode doc_id_slash_terminated coords

get_contents (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords) = contents
get_doc_id (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords) = doc_id
get_mode (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords) = mode
get_doc_id_slash_terminated (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords) = doc_id_slash_terminated
get_coords (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords) = coords


type QuadPresType = QuadPres Contents_Node UrlType Mode Bool QuadPresCoordsType

instance (Show a, Show b, Show c, Show d, Show e) => Show (QuadPres a b c d e) where
    show (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords) =
        "QuadPres (doc_id = " ++ (show doc_id) ++ ", " ++
        "mode = " ++ (show mode) ++ ", " ++
        "doc_id_slash_terminated = " ++ (show doc_id_slash_terminated) ++ ", " ++
        "coords = " ++ (show coords) ++ ")"


church_N 0 f x = x
church_N n f x = (f (church_N (n-1) f x))


-- This function splits a string (or any list for that matter) according
-- to a static separator string.

--mysplit :: String -> String -> [String]
mysplit separator "" = [""]
mysplit separator base =
    let len = (length separator)
    in (if ((take len base) == separator)
        then "":(mysplit separator (church_N len tail base))
        else let ret = (mysplit separator (tail base))
             in (head(base):head(ret)) : tail(ret)
       )

remove_first_instances a [] = []
remove_first_instances a (b:bs) =
    if a == b
    then remove_first_instances a bs
    else b:bs

-- mysplit_perl behaves in a similar manner to perl's split in the sense
-- that it removes empty strings that are found at the end

mysplit_perl separator base = (reverse (remove_first_instances [] (reverse (mysplit separator base))))

--make_quadpres_instance :: Contents_Node -> String -> Mode -> QuadPresType
make_quadpres_instance contents doc_id_proto mode =
    (MakeQuadPres
        contents
        doc_id
        mode
        doc_id_slash_terminated
        coords
    ) where
        doc_id_slash_terminated :: Bool
        doc_id_slash_terminated = is_ending_with_slash doc_id_proto where
            is_ending_with_slash :: String -> Bool
            is_ending_with_slash [] = False
            is_ending_with_slash "/" = True
            is_ending_with_slash (a:as) = is_ending_with_slash as

        doc_id_parsed :: [String]
        doc_id_parsed = (mysplit_perl "/" doc_id_proto)

        -- coords should contain
        -- coords = [Int]
        -- type = Image | Node
        -- mime = String
        coords :: QuadPresCoordsType
        coords = (find_coords contents ("":doc_id_parsed))

        doc_id = (MakeUrl
                    doc_id_parsed
                    (if (get_coords_doc_type coords) == Image
                     then False
                     else (get_is_dir branch)
                    )
                    mode
                 ) where
            branch = traverse (get_coords_coords coords) contents
            traverse [] branch = branch
            traverse (a:as) branch = traverse as ((get_subs branch)!!a)



find_coords contents doc_id_parsed =
    (traverse [] contents []) where
    traverse coords branch old_path =
        ret where
            ret = (if path /= (take len_path doc_id_parsed)
                   then (MakeQuadPresCoords [] Node "")
                   else if (len_path == (length(doc_id_parsed)))
                   then (MakeQuadPresCoords coords Node "")
                   else if ((length matching_subs) > 0)
                        then (head matching_subs)
                        else if (len_path < ((length doc_id_parsed) - 1 ))
                             then (MakeQuadPresCoords [] Node "")
                             else (check_images 0 (get_images branch))
                  )
            path = old_path ++ [(get_url branch)]
            len_path = (length path)
            descend :: Int -> QuadPresCoordsType
            descend new_coord =
                (traverse
                    (coords ++ [new_coord])
                    ((get_subs branch)!!new_coord)
                    path
                )
            matching_subs =
                (filter
                    (\x -> ((length (get_coords_coords x)) > 0))
                   (map descend [0 .. ((length (get_subs branch))-1)])
                )
            last_element = (myle doc_id_parsed) where
                myle (a:[]) = a
                myle (a:as) = (myle as)
            check_images :: Int -> [Contents_Image String String] -> QuadPresCoordsType
            check_images n [] = (MakeQuadPresCoords [] Node "")
            check_images n images =
                (if last_element == (get_image_url (head images))
                 then (MakeQuadPresCoords (coords ++ [n]) Image (get_image_mime_type (head images)))
                 else check_images (n+1) (tail images))

read_file = readFile
--read_file :: String -> String
--read_file filename = catch myread (\e -> return "") where
--    myread = do f <- (openFile filename)
--                return (myread2 f)
--    myread2 f = do c <- (getChar f)
--                   return c:(myread2 f)

get_document_base_text :: QuadPresType -> IO String
get_document_base_text (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords) =
    readFile filename where
        document_id = (join "/" (url_get_url doc_id))
        filename = "./src/" ++ document_id ++
            (if (url_get_is_dir doc_id)
             then "/index.html"
             else "")

get_url_by_coords :: QuadPresType -> [Int] -> UrlType
get_url_by_coords (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords2) coords =
    ret where
        (branch, url_path) = (get_branch coords contents [])
        get_branch [] branch path = (branch,path ++ [(get_url branch)])
        get_branch (a:as) branch path = (get_branch
            as
            ((get_subs branch)!!a)
            (path ++ [(get_url branch)])
            )
        ret = (MakeUrl (tail url_path) (get_is_dir branch) mode)


type QuadPresControlFunctionType = QuadPresType -> (Bool,UrlType)

get_contents_url :: QuadPresControlFunctionType
get_contents_url (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords2) =
    (True,(MakeUrl [] True mode))


get_next_url :: QuadPresControlFunctionType
get_next_url qp =
    (if (head next_coords) /= -1
     then (True,(get_url_by_coords qp next_coords))
     else (False,(MakeUrl [] False (get_mode qp)))) where
        contents = (get_contents qp)
        coords = (get_coords_coords (get_coords qp))
        next_coords = (reverse (descend contents coords []))
        -- descend keeps the coords list to be returned in reverse
        -- because it results in a lower complexity
        descend :: Contents_Node -> [Int] -> [Int] -> [Int]
        descend branch [] coords_to_ret =
            (if (get_is_dir branch)
             then 0:coords_to_ret
             else [-1]
            )
        descend branch (coord:coords) coords_to_ret =
            (if (head descend_next) /= -1
             then descend_next
             else this_next) where
                subs = (get_subs branch)
                len_subs = (length subs)
                this_next = (if (len_subs > (coord+1))
                             then (coord+1):coords_to_ret
                             else [-1]
                            )
                descend_next = (descend
                    (subs!!coord)
                    coords
                    (coord:coords_to_ret)
                    )

get_prev_url :: QuadPresControlFunctionType
get_prev_url (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords_obj) =
    (if len_coords == 0
     then (False,(MakeUrl [] False (get_mode qp)))
     else if (coords!!(len_coords-1) > 0)
          then (True,(get_url_by_coords
            qp
            ((take (len_coords-1) coords) ++
                [(coords!!(len_coords-1))-1])
            ))
          else (True,(get_url_by_coords
            qp
            (take (len_coords-1) coords)
            ))
    ) where
        qp = (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords_obj)
        coords = (get_coords_coords coords_obj)
        len_coords = (length coords)

get_up_url :: QuadPresControlFunctionType
get_up_url (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords_obj) =
    (if len_coords == 0
     then (False , (MakeUrl [] False (get_mode qp)))
     else (True , (get_url_by_coords qp (take (len_coords-1) coords)))
    ) where
        qp = (MakeQuadPres contents doc_id mode doc_id_slash_terminated coords_obj)
        coords = (get_coords_coords coords_obj)
        len_coords = (length coords)

data QuadPresControlSpec url_func caption = MakeQuadPresControlSpec url_func caption

control_spec_get_url_func (MakeQuadPresControlSpec url_func caption) = url_func
control_spec_get_caption (MakeQuadPresControlSpec url_func caption) = caption

type QuadPresControlSpecType = QuadPresControlSpec QuadPresControlFunctionType String

get_control_text :: QuadPresType -> QuadPresControlSpecType -> String
get_control_text qp (MakeQuadPresControlSpec url_func caption) =
    (if other_url_valid
     then ("<a href=\"" ++
        (get_relative_url this_url other_url doc_id_slash_terminated) ++
        "\" class=\"" ++ navigation_style_class ++ "\">" ++
        caption ++ "</a>")
     else ("<b class=\"" ++ navigation_style_class ++ "\">" ++ caption ++ "</b>")
    ) where
        this_url = (get_doc_id qp)
        (other_url_valid, other_url) = (url_func qp)
        doc_id_slash_terminated = (get_doc_id_slash_terminated qp)

get_navigation_bar :: QuadPresType -> String
get_navigation_bar qp =
    "<table>\n<tr>\n<td\n" ++
    (join "</td>\n<td>\n" [
        (get_control_text qp (MakeQuadPresControlSpec get_contents_url "Contents")),
        (get_control_text qp (MakeQuadPresControlSpec get_up_url "Up")),
        (get_control_text qp (MakeQuadPresControlSpec get_prev_url "Previous")),
        (get_control_text qp (MakeQuadPresControlSpec get_next_url "Next"))
    ]) ++
    "</td>\n</tr>\n</table>\n"

type QuadPresBlockFunctionType = QuadPresType -> String
get_header :: QuadPresBlockFunctionType
get_header qp =
    (
    "<html>\n<head>\n<title>" ++ title ++ "</title>\n" ++
    "<link rel=\"StyleSheet\" href=\"" ++
        (get_relative_url
            doc_id
            (MakeUrl ["style.css"] False mode)
            (get_doc_id_slash_terminated qp)
        ) ++
    "\" type=\"text/css\">\n" ++
    "</head>\n" ++
    "<body>\n" ++ (get_navigation_bar qp) ++
    "<h1>" ++ title ++ "</h1>\n"
    )
    where
        coords_obj = (get_coords qp)
        coords = (get_coords_coords coords_obj)
        branch = (get_branch coords (get_contents qp))
        title = (get_title branch)
        get_branch [] branch = branch
        get_branch (c:cs) branch = (get_branch cs ((get_subs branch)!!c))
        doc_id = (get_doc_id qp)
        mode = (get_mode qp)

get_footer :: QuadPresBlockFunctionType
get_footer qp =
   ("\n\n<hr>\n" ++
    (get_navigation_bar qp) ++
    "</body>\n" ++
    "</html>\n"
    )

get_contents_helper :: QuadPresType -> Contents_Node -> [String] -> String
get_contents_helper qp branch url =
    ("<a href=\"" ++
    (get_relative_url
        doc_id
        (MakeUrl url (get_is_dir branch) mode)
        doc_id_slash_terminated
    ) ++
    "\" class=\"" ++ contents_style_class ++ "\">" ++
    (get_title branch) ++
    "</a><br>\n" ++
    (if (get_is_dir branch)
     then ("<ul>\n" ++
        (join ""
            [ (get_contents_helper qp sb (url ++ [(get_url sb)])) | sb <- (get_subs branch) ]
        ) ++ "</ul>\n")
     else ""
     )
     )
    where
        mode = (get_mode qp)
        doc_id = (get_doc_id qp)
        doc_id_slash_terminated = (get_doc_id_slash_terminated qp)

get_contents_block :: QuadPresBlockFunctionType
get_contents_block qp =
    (
        "<ul>\n" ++
        (if (get_is_dir branch)
         then (join
            ""
            [ (get_contents_helper qp sb (url ++ [(get_url sb)])) | sb <- (get_subs branch) ]
         )
         else ""
        )
        ++ "</ul>\n"
    ) where
        coords_obj = (get_coords qp)
        coords = (get_coords_coords coords_obj)
        (branch,url) = (get_branch coords (get_contents qp) [])
        get_branch [] branch url = (branch,url)
        get_branch (c:cs) branch url = (get_branch cs ((get_subs branch)!!c) (url++[(get_url branch)]))






match_command_tags :: QuadPresType -> String -> String -> QuadPresBlockFunctionType -> (String,Bool)
match_command_tags qp text command_string function =
    (loop text) where
        loop :: String -> (String,Bool)
        loop "" = ("",False)
        loop str = let (was_matched, inside, rest) = (match ("begin_"++command_string) str)
                   in if (was_matched)
                      then let (loop_rest,ret) = (loop_end rest) --((function qp) ++ rest, True)
                           in (loop_rest,ret)
                      else let (loop_rest,ret) = (loop rest)
                           in ((take ((length str)-(length rest)) str) ++ loop_rest , ret)
        loop_end :: String -> (String,Bool)
        loop_end "" = ("",False)
        loop_end str = let (was_matched, inside, rest) = (match ("end_"++command_string) str)
                       in if (was_matched)
                          then ((function qp) ++ rest, True)
                          else (loop_end rest)
        match :: String -> String -> (Bool,String,String)
        -- match :: text -> (Found,inside, rest of string)
        --match :: String -> (Bool,String,String)
        match cmd_str str =
                    let (str1,ret1) = (match_begin_html_comment str)
                    in if (not ret1)
                       then (False,"",str1)
                       else let str2 = (match_minus_asterisk str1)
                                str3 = (match_ws_asterisk str2)
                                (str4,ret4) =  (match_ampersand str3)
                            in if (not ret4)
                               then (False,"",str4)
                               else let str5 = (match_ws_asterisk str4)
                                        (str6,ret6) = (match_str cmd_str str5)
                                    in if (not ret6)
                                       then (False,"",str6)
                                       else let str7 = (match_ws_asterisk str6)
                                                (str8,ret8) = (match_minus str7)
                                            in if (not ret8)
                                               then (False,"",str8)
                                               else let str9 = (match_minus_asterisk str8)
                                                        (str10,ret10) = (match_gt str9)
                                                    in if (not ret10)
                                                       then (False,"",str10)
                                                       else (True,"",str10)
        match_begin_html_comment :: String -> (String, Bool)
        match_begin_html_comment "" = ("",False)
        match_begin_html_comment string =
            (if (length string) < 3
             then ((tail string),False)
             else if (take 3 string) == "<!-"
             then ((church_N 3 tail string),True)
             else ((tail string), True)
            )
        match_char_asterisk :: Char -> String -> String
        match_char_asterisk c "" = ""
        match_char_asterisk c (a:as) =
            (if a == c
             then (match_char_asterisk c as)
             else (a:as)
            )
        match_minus_asterisk string = match_char_asterisk '-' string
        match_ws_asterisk string = match_char_asterisk ' ' string
        match_char :: Char -> String -> (String, Bool)
        match_char c "" = ("",False)
        match_char c (a:as) =
            (if a == c
             then (as,True)
             else ((a:as),False)
            )
        match_ampersand str = match_char '&' str
        match_minus str = match_char '-' str
        match_str :: String -> String -> (String, Bool)
        match_str prefix "" = ("",False)
        match_str prefix str =
            (if (length str) < len_mystr
             then ("",False)
             else if (take len_mystr str) == mystr
             then ((church_N len_mystr tail str),True)
             else (str,False)
            ) where
                mystr = prefix
                len_mystr = (length mystr)
        match_begin_cmd = match_str ("begin_"++command_string)
        match_end_cmd = match_str ("end_"++command_string)
        match_gt str = match_char '>' str




result = make_quadpres_instance contents "intro.html" Server
result2 = make_quadpres_instance contents "recursion/qsort.html" Server
result3 = make_quadpres_instance contents "" Server

result_process inst =
                 do text <- (get_document_base_text inst)
                    let (str1,ret1) = (match_command_tags inst text "header" get_header)
                        (str2,ret2) = (match_command_tags inst str1 "footer" get_footer)
                        (str3,ret3) = (match_command_tags inst str2 "contents" get_contents_block)
                        in (putStr str3)



