module Replacer 
(replace
) where



replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace pattern supplement list@(x:xs) = 
    if head pattern == x then
        if listBegins pattern list then
            supplement ++ 
                (drop (length pattern - 1 ) 
                    $ replace pattern supplement xs 
                )
        else
            x : replace pattern supplement xs
    else
        x : replace pattern supplement xs


listBegins :: Eq a => [a] -> [a] -> Bool
listBegins _ [] = True
listBegins [] _ = True
listBegins (x:xs) (y:ys) =
    if x == y then
        listBegins xs ys
    else
        False



