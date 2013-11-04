echo 'get_relative_url (MakeUrl ["regexps", "hello"] True Harddisk) (MakeUrl
["regexps", "you", "doc.html"] False Harddisk) True' | hugs Url.hs | tail
+18 | head -1
