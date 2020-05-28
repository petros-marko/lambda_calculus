import Parser
import Interpreter

main = do
         input <- getLine
         case parse input of
              Just r -> putStrLn . show . eval $ r 
              None   -> putStrLn "Invalid Expression"
