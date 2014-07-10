import Text.ParserCombinators.Parsec
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.State as S
import Data.Char

interpret :: String -> String
interpret input = case parse parseBF "brainfuck" input of
    Left err -> error "Syntax error: " ++ show err
    Right val -> val

validCommands :: String
validCommands = "><+-.,[]"

command :: Parser Char
command = oneOf validCommands

comment :: Parser Char
comment = noneOf validCommands

parseBF :: Parser String
parseBF = fmap concat $ sepBy (many command) (many1 comment)

type ListZipper a = ([a],[a])

type Program = S.StateT (ListZipper Int) IO

increment :: Program ()
increment = S.modify $ \ (x:xs,xss) -> (x + 1:xs, xss)

decrement :: Program ()
decrement = S.modify $ \ (x:xs,xss) -> (x - 1:xs, xss)

zipRight :: ListZipper a -> ListZipper a
zipRight (x:xs,xss) = (xs,x:xss)

zipLeft :: ListZipper a -> ListZipper a
zipLeft (xs,x:xss) = (x:xs,xss)

goRight :: Program ()
goRight = S.modify zipRight

goLeft :: Program ()
goLeft = S.modify zipLeft

execute :: (ListZipper Char) -> Program ()
execute ([],_) = return ()
execute xs@(x:_,_) = do
    (y:ys,yss) <- S.get
    let next = execute $ zipRight xs in case x of
        '>' -> goRight >> next
        '<' -> goLeft >> next
        '+' -> increment >> next
        '-' -> decrement >> next
        '.' -> (T.liftIO $ putChar $ chr y) >> next
        ',' -> do { z <- T.liftIO $ getChar; S.put ((ord z):ys,yss) } >> next
        '[' -> if y /= 0 then next else execute $ helper 0 ']' '[' $ tail $ iterate zipRight xs
        ']' -> if y == 0 then next else execute $ helper 0 '[' ']' $ tail $ iterate zipLeft xs

isBrace b = (b ==) . head . fst

helper n o c (x:xs)
    | (n == 0) && isBrace o x = x 
    | isBrace o x = helper (n - 1) o c xs
    | isBrace c x = helper (n + 1) o c xs
    | otherwise = helper n o c xs

main = do
    program <- fmap interpret getLine
    S.evalStateT (execute (program,[])) (repeat 0,[])
