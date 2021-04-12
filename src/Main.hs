import Text.Parsing.Report
import Control.Monad (forM_, replicateM_)
import Data.Maybe (fromMaybe)
import Text.Printf
import Debug.Trace
-- 
type DollarAmount = Int

printDollarAmount a = "" ++ (show d) ++ "." ++ (if c < 10 then "0" else "") ++ (show c)  where
        (d, c) = quotRem a 100 

dollarAmount :: String -> DollarAmount
dollarAmount s = d * 100 + c where
    (posd, posc) = break ((==) '.') $ tail $ snd $ break ((==) '$') s
    d = read posd
    c = read $ tail $ posc

data LineCharges = LineCharges {
    number :: String,
    accountCharges :: DollarAmount
} deriving (Show)

find = skipUntil . startsWith

l4 = reverse . take 4 . reverse

phoneNumber = l4 <$> (find "(" *> currentLine)
getDollars = find "$" *> (dollarAmount <$> currentLine)

billPeriod = find "Bill period" *> find "Account"
             *> currentLine *> currentLine *> currentLine

lastDollars = snd <$> (within' (find "(") $ last <$> many getDollars)

line = LineCharges <$> phoneNumber <* currentLine <* startsWith "Voice" <*> lastDollars

withinN :: Int -> Parser a -> Parser a
withinN n p = snd <$> (within' (replicateM_ n currentLine) (p))

accChargeLine = (,) <$> phoneNumber <*> withinN 4 getDollars

accCharges = do
    find "VOICE LINES"
    (_, cs) <- within' (find "CONNECTED DEVICES") $ (many $ skipUntil $ accChargeLine)
    pure cs

fairSplit :: Int -> Int -> [Int]
fairSplit a n = f e r n where
    (e, r) = quotRem a n
    f _ _ 0 = []
    f e 0 n = e : (f e 0 (n - 1))
    f e r n = (e + 1) : (f e (r - 1) (n - 1)) 

main = do
    Just (bill, sharedCharges, lines, ac, total) <- processStdIn $ do
        bill <- billPeriod
        sharedCharges <- find "THIS BILL SUMMARY" *> find "-" *> getDollars
        (_, lines) <- within' (find "DETAILED CHARGES") (many $ skipUntil $ line)
        ac <- accCharges
        total <- find "Amount enclosed" *> getDollars
        pure (bill, sharedCharges, lines, ac, total)

    putStrLn bill
    putStrLn $ "Total: " ++ (printDollarAmount total)
    
    let
        lineCharges = sum $ map snd ac
        
        lineChargesAdjs l = fromMaybe 0 $ lookup l ac
        perLine = fairSplit (sharedCharges + lineCharges) $ length lines
        
    forM_ (zip perLine lines) $ \(p, LineCharges n c) ->
        putStrLn $ n ++ " " ++ (printDollarAmount $ p + c - (lineChargesAdjs n))

    let
        f (p, LineCharges n c) = (l4 n, p + c - (lineChargesAdjs n))

        m = [(2, "2305"), (3, "1991"), (6, "4681"), (5, "9507")]
        
        x c =
            fromMaybe "-" $ do
                n <- lookup c m
                printDollarAmount <$> (lookup n $ map f (zip perLine lines))

    putStrLn $ unwords $ map x [0 .. 7]