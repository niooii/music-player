module Parser ()
where
 
newtype Parser a = Parser { runParser :: String  -> [(a, String)] }

zero :: Parser a
zero = Parser $ const []