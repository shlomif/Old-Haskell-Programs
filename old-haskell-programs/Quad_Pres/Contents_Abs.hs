module Contents_Abs where

data Contents_Image url mime = MakeContents_Image url mime

get_image_url (MakeContents_Image url mime) = url
get_image_mime_type (MakeContents_Image url mime) = mime

make_image url mime = (MakeContents_Image url mime)

--data Contents_Node url title subs images is_dir =
--    MakeContents_Node url title subs images is_dir

--newtype QuadPres_Contents_Node = Contents_Node String String [QuadPres_Contents_Node] [Contents_Image String String] Bool

newtype Contents_Node = MakeContents_Node (String, String, [Contents_Node], [Contents_Image String String], Bool)

instance Show Contents_Node where
    show (MakeContents_Node (url,title,subs,images,is_dir)) =
        ""

get_url (MakeContents_Node (url,title,subs,images,is_dir)) = url
get_title (MakeContents_Node (url,title,subs,images,is_dir)) = title
get_subs (MakeContents_Node (url,title,subs,images,is_dir)) = subs
get_is_dir (MakeContents_Node (url,title,subs,images,is_dir)) = is_dir
get_images (MakeContents_Node (url,title,subs,images,is_dir)) = images
--type QuadPresContents_Node = Contents_Node String String [QuadPresContents_Node] [String] Bool

make_contents_doc :: String -> String -> Contents_Node
make_contents_doc url title = (MakeContents_Node (url,title,[],[],False))

make_contents_dir :: String -> String -> [Contents_Node] -> Contents_Node
make_contents_dir url title subs = (MakeContents_Node (url,title,subs,[],True))

make_contents_dir_with_images :: String -> String -> [Contents_Node] -> [Contents_Image String String] -> Contents_Node
make_contents_dir_with_images url title subs images = (MakeContents_Node (url,title,subs,images,True))

