{- Plansza jest jednowymiarową listą liczb całkowitych.
   Aby pobrać wartość z np. 2 wiersza i 2 kolumny, należy pobrać element
   o indeksie n+1, gdzie n to rozmiar boku planszy.-}
type Board = [Int]

type Constraints = [(Char, Int, Int)]

upSymbol = 'u'
downSymbol = 'd'
leftSymbol = 'l'
rightSymbol = 'r'

{- Funkcja pomocnicza sprawdzająca czy dana liczba nie znajduje się w danej liście (zwraca prawdę, jeśli danej 
   liczby nie ma w liście). -}
notIn :: Int -> [Int] -> Bool
notIn _ []                                = True
notIn number (head:rest) | number == head = False
                         | otherwise      = notIn number rest

getPrevFromRow :: Board -> Int -> Int -> Int -> [Int]
getPrevFromRow board sideSize column row =
               let begin = (row - 1) * sideSize
                   end = begin + column - 1
                   getPrevFromRow' :: Board -> Int -> Int -> Int -> [Int]
                   getPrevFromRow' [] _ _ _                                        = []
                   getPrevFromRow' (head:rest) begin end current | current == end  = []
                                                                 | current < begin = getPrevFromRow' rest begin end (current + 1)
                                                                 | otherwise       = head:(getPrevFromRow' rest begin end (current + 1))
               in getPrevFromRow' board begin end 0

getPrevFromColumn :: Board -> Int -> Int -> Int -> [Int]
getPrevFromColumn board sideSize column row = 
                  let begin = (row - 1) * sideSize
                      end   = begin + column - 1
                      getPrevFromColumn' :: Board -> Int -> Int -> [Int]
                      getPrevFromColumn' [] _ _                                                           = []
                      getPrevFromColumn' (head:rest) end current | current == end                         = []
                                                                 | (mod current sideSize) /= (column - 1) = getPrevFromColumn' rest end (current + 1)
                                                                 | otherwise                              = head:(getPrevFromColumn' rest end (current + 1))
                  in getPrevFromColumn' board end 0

checkNotRepeating :: Board -> Int -> Int -> Int -> Int -> Bool
checkNotRepeating partialBoard sideSize column row element = 
                  let prevFromRow    = getPrevFromRow partialBoard sideSize column row
                      prevFromColumn = getPrevFromColumn partialBoard sideSize column row
                  in (notIn element prevFromRow) && (notIn element prevFromColumn)

visibleNumber :: [Int] -> Int
visibleNumber list = visibleNumber' list 0
                       where visibleNumber' :: [Int] -> Int -> Int
                             visibleNumber' [] _                                         = 0
                             visibleNumber' (x:rest) currentHighest | x > currentHighest = 1 + (visibleNumber' rest x)
                                                                    | otherwise          = visibleNumber' rest currentHighest

checkIfProperConstraint' :: (Char, Int, Int) -> Char -> Int -> Maybe Int
checkIfProperConstraint' (sym, num, constr) symbol number | sym == symbol && num == number = Just constr
                                                          | otherwise                      = Nothing

getConstraint :: Constraints -> Char -> Int -> Maybe Int
getConstraint [] _ _                               = Nothing
getConstraint (x:restCostraints) sideSymbol number = case checkIfProperConstraint' x sideSymbol number of
                                                         Nothing              -> getConstraint restCostraints sideSymbol number
                                                         Just constraintValue -> Just constraintValue

checkVisibilityConstraint :: Constraints -> Char -> Int -> [Int] -> Bool
checkVisibilityConstraint constraints sideSymbol number table = case getConstraint constraints sideSymbol number of
                                                                    Nothing         -> True
                                                                    Just constraint -> (visibleNumber table) == constraint

checkRowConstraints :: Board -> Int -> Constraints -> Int -> Int -> Bool
checkRowConstraints board sideSize constraints row element = notRepeating && leftConstr && rightConstr
                                                             where prevFromRow    = getPrevFromRow board sideSize sideSize row
                                                                   prevFromColumn = getPrevFromColumn board sideSize sideSize row
                                                                   notRepeating   = (notIn element prevFromRow) && (notIn element prevFromColumn)
                                                                   entireRow      = prevFromRow ++ [element]
                                                                   leftConstr     = checkVisibilityConstraint constraints leftSymbol row entireRow
                                                                   rightConstr    = checkVisibilityConstraint constraints rightSymbol row (reverse entireRow)


checkColumnConstraints :: Board -> Int -> Constraints -> Int -> Int -> Bool
checkColumnConstraints board sideSize constraints column element = notRepeating && upConstr && downConstr
                                                                      where prevFromRow    = getPrevFromRow board sideSize column sideSize
                                                                            prevFromColumn = getPrevFromColumn board sideSize column sideSize
                                                                            notRepeating   = (notIn element prevFromRow) && (notIn element prevFromColumn)
                                                                            entireColumn   = prevFromColumn ++ [element]
                                                                            upConstr       = checkVisibilityConstraint constraints upSymbol column entireColumn
                                                                            downConstr     = checkVisibilityConstraint constraints downSymbol column (reverse entireColumn)  

tryElementForRowEnd :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElementForRowEnd board sideSize constraints fieldNumber element | rowConstr = tryNext' board sideSize constraints fieldNumber element
                                                                   | otherwise = tryElement board sideSize constraints fieldNumber (element-1)
                                                                   where rowNumber = (div fieldNumber sideSize) + 1 
                                                                         rowConstr = checkRowConstraints board sideSize constraints rowNumber element 

tryElementForColumnEnd :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElementForColumnEnd board sideSize constraints fieldNumber element | columnConstr = tryNext' board sideSize constraints fieldNumber element
                                                                      | otherwise = tryElement board sideSize constraints fieldNumber (element-1)
                                                                      where columnNumber = (mod fieldNumber sideSize) + 1 
                                                                            columnConstr = checkColumnConstraints board sideSize constraints columnNumber element                                                                

tryElementForBoardEnd :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElementForBoardEnd board sideSize constraints fieldNumber element | columnConstr && rowConstr = tryNext' board sideSize constraints fieldNumber element
                                                                     | otherwise = tryElement board sideSize constraints fieldNumber (element-1)
                                                                     where columnNumber = (mod fieldNumber sideSize) + 1 
                                                                           columnConstr = checkColumnConstraints board sideSize constraints columnNumber element
                                                                           rowNumber = (div fieldNumber sideSize) + 1 
                                                                           rowConstr = checkRowConstraints board sideSize constraints rowNumber element


tryNext' :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryNext' board sideSize constraints fieldNumber element = case (tryElement (board ++ [element]) sideSize constraints (fieldNumber+1) sideSize) of
                                                               Nothing -> tryElement board sideSize constraints fieldNumber (element-1)
                                                               Just x  -> Just x                                                                      

tryElement :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElement _ _ _ _ 0                                      = Nothing
tryElement board sideSize constraints fieldNumber element | sideSize^2 == fieldNumber = Just board
                                                          | lastField                 = tryElementForBoardEnd board sideSize constraints fieldNumber element
                                                          | rowEnd                    = tryElementForRowEnd board sideSize constraints fieldNumber element
                                                          | columnEnd                 = tryElementForColumnEnd board sideSize constraints fieldNumber element
                                                          | notRepeating              = tryNext' board sideSize constraints fieldNumber element
                                                          | otherwise                 = tryElement board sideSize constraints fieldNumber (element-1)
                                                        where
                                                             rowNumber = (div fieldNumber sideSize) + 1
                                                             columnNumber = (mod fieldNumber sideSize) + 1
                                                             rowEnd = columnNumber == sideSize
                                                             columnEnd = rowNumber == sideSize
                                                             lastField = sideSize^2-1 == fieldNumber^2
                                                             notRepeating = checkNotRepeating board sideSize columnNumber rowNumber element                                                            


printBoard :: Maybe Board -> Int -> Int -> IO()
printBoard Nothing _ _                          = do putStr "Brak rozwiązania \n"
printBoard (Just []) _ _                        = return ()
printBoard (Just (x:rest)) sideSize fieldNumber = do putStr (show x)
                                                     if (mod fieldNumber sideSize) + 1 == sideSize then do 
                                                         putStr "\n"
                                                     else do
                                                         putStr " "
                                                     printBoard (Just rest) sideSize (fieldNumber + 1)

gameSolver :: Int -> Constraints -> IO ()
gameSolver sideSize constraints = do putStr "Wpisz symbol ściany planszy (u-górna, l-lewa, r-prawa, d-dolna, n-żadna -> koniec wpisywania): "
                                     sideSymbol <- getChar
                                     getLine
                                     if sideSymbol == 'n' then do
                                        printBoard (tryElement [] sideSize constraints 0 sideSize) sideSize 0
                                        return ()
                                     else do
                                        putStr "Wpisz numer rzędu/kolumny: "
                                        column <- getLine
                                        putStr "Wpisz ograniczenie widoczności: "
                                        visibility <- getLine
                                        gameSolver sideSize ([ (sideSymbol, (read column :: Int), (read visibility :: Int)) ] ++ constraints)

main = do putStr "Wpisz rozmiar boku planszy: "
          sideSize <- getLine
          gameSolver (read sideSize :: Int) []