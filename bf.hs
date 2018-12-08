

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Data.Word
import Control.Applicative
import System.Environment



data BFT = F | B | I | D | P | G | CO | CC deriving Show

tokenize :: String -> [BFT]
tokenize []       = []
tokenize ('>':ts) = F  : tokenize ts
tokenize ('<':ts) = B  : tokenize ts
tokenize ('+':ts) = I  : tokenize ts
tokenize ('-':ts) = D  : tokenize ts
tokenize ('.':ts) = P  : tokenize ts
tokenize (',':ts) = G  : tokenize ts
tokenize ('[':ts) = CO : tokenize ts
tokenize (']':ts) = CC : tokenize ts
tokenize (_:ts)        = tokenize ts



data EOFHANDLE = ZERO | UNCH deriving (Eq, Show)

data OPTS = OPTS {stdNL :: Bool, eofW :: EOFHANDLE}

mkDEFOPTS :: OPTS
mkDEFOPTS = OPTS True ZERO



newtype CELL = CELL Word8 deriving (Eq, Num, Show)

toInt (CELL x) = fromIntegral x

instance Enum CELL where
  toEnum x = CELL $ toEnum x
  fromEnum (CELL x) = fromEnum x
  succ (CELL x) | x == maxBound = CELL minBound
  succ (CELL x)                 = CELL (succ x)
  pred (CELL x) | x == minBound = CELL maxBound
  pred (CELL x)                 = CELL (pred x)



data BFMS = BFMS [CELL] CELL [CELL] deriving Show

mkINIT :: BFMS
mkINIT = BFMS [] 0 []

next :: BFMS -> BFMS
next (BFMS p c (new:n)) = BFMS (c:p) new n
next (BFMS p c [])      = BFMS (c:p) 0 []

prev :: BFMS -> BFMS
prev (BFMS (new:p) c n) = BFMS p new (c:n)
prev (BFMS [] c n)      = BFMS [] 0 (c:n)

inc :: BFMS -> BFMS
inc (BFMS p c n) = BFMS p (succ c) n

dec :: BFMS -> BFMS
dec (BFMS p c n) = BFMS p (pred c) n



data BFP = WHL BFP BFP |
           NEXT BFP    |
           PREV BFP    |
           INC BFP     |
           DEC BFP     |
           PUT BFP     |
           GET BFP     |
           END         deriving Show

construct :: [BFT] -> ([BFT], BFP)
construct []        = ([], END)
construct (F:bfts)  = fmap NEXT $ construct bfts
construct (B:bfts)  = fmap PREV $ construct bfts
construct (I:bfts)  = fmap INC  $ construct bfts
construct (D:bfts)  = fmap DEC  $ construct bfts
construct (P:bfts)  = fmap PUT  $ construct bfts
construct (G:bfts)  = fmap GET  $ construct bfts
construct (CO:bfts) = case construct bfts of
                           ((CC:ots), i) -> fmap (\o -> WHL i o) $ construct ots
                           _             -> (CO:bfts, END)
construct bfts      = (bfts, END)



interpret :: OPTS -> BFP -> BFMS -> IO BFMS
interpret opts END       state = return state
interpret opts (NEXT p') state = interpret opts p' (next state)
interpret opts (PREV p') state = interpret opts p' (prev state)
interpret opts (INC p')  state = interpret opts p' (inc  state)
interpret opts (DEC p')  state = interpret opts p' (dec  state)
----------------------------------------------------------------------------------------------------
interpret opts (PUT p')  state@(BFMS _ c _) = do putChar $ chr $ toInt c
                                                 interpret opts p' state
----------------------------------------------------------------------------------------------------
interpret opts (GET p')  (BFMS p c n) = let
                                             eof = if eofW opts == ZERO then 0 else toInt c
                                        in
                                             do c' <- getChar <|> return (chr eof)
                                                interpret opts p' (BFMS p (fromIntegral $ ord c') n)
----------------------------------------------------------------------------------------------------
interpret opts (WHL _ o) state@(BFMS _ c _) | c == 0 = interpret opts o state
interpret opts (WHL i o) state = do ns <- (interpret opts i state)
                                    interpret opts (WHL i o) ns


main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> putStrLn "ERROR: NO INPUT SPECIFIED"
    (f:_) -> do target <- readFile f
                let bfts = construct . tokenize . filter (not . isSpace) $ target
                case bfts of
                     ([], p) -> do interpret mkDEFOPTS p mkINIT
                                   return ()
                     (up, _) -> putStrLn $ "ERROR: " ++ show up
