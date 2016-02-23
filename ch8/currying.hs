cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

apedCatty :: String -> String 
apedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"



