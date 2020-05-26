{- Autor : Mateusz Bochenek, 283667 -}

{- Plansza jest jednowymiarową listą liczb całkowitych.
   W skład listy wchodzą liczby z zakresu 1-n, przy czym wartości w 
   wierszach/kolumnach nie mogą się powtarzać.
   Pola są numerowane od 0 do n-1, a wiersze/kolumny od 1 do n.
   Aby pobrać wartość z np. 2 wiersza i 2 kolumny, należy pobrać wartość pola
   o indeksie n+1, gdzie n to rozmiar boku planszy.
   Plansza na początku jest listą pustą, dopiero wraz z postępem w rozwiązywaniu
   zagadki "rozrasta się" (dodawane są nowe elementy reprezentujące wartości pól). -}
type Board = [Int]

{- Ograniczenia związane z liczbą widocznych z danej pozycji piramid
   są przechowywane w pojedynczej liście jednowymiarowej. Każdy element
   tej listy jest krotką składającą się z 3 pól. Pierwszy element krotki 
   to oznaczenie boku planszy (którego dotyczy ograniczenie), drugi element
   to numer wiersza/kolumny, a trzeci to liczba widocznych piramid. -}
type Constraints = [(Char, Int, Int)]

{- Symbole będące oznaczeniami boków planszy. -}
upSymbol = 'u'
downSymbol = 'd'
leftSymbol = 'l'
rightSymbol = 'r'
   
{- Funkcja pomocnicza sprawdzająca czy dana liczba nie znajduje się w danej 
   liście (zwraca prawdę, jeśli dana liczba nie znajduje się w liście). -}
notIn :: Int -> [Int] -> Bool
notIn _ [] = True
notIn number (head : rest) | number == head = False
                           | otherwise      = notIn number rest

{- Funkcja pomocnicza zwracająca wartości pól znajdujących przed danym polem
   w tym samym wierszu (w formie listy).
   Argumenty:
   - plansza
   - "długość" boku planszy
   - numer kolumny, przed którą szukane są wartości
   - numer wiersza, z którego zwracane są wartości
   Z racji tego, że plansza jest tablicą jednowymiarową, funkcja musi przejść
   po wszystkich elementach listy znajdujących się przed ostatnim szukanym elementem. -}   
getPrevFromRow :: Board -> Int -> Int -> Int -> [Int]
getPrevFromRow board sideSize column row =
   let begin = (row - 1) * sideSize   -- indeks pierwszego pola, z którego wartość ma zostać zwrócona
       end   = begin + column - 1     -- indeks pola, sprzed którego zwracane są elementy
       {- Argumenty: lista pól, obecny indeks pola -}
       getPrevFromRow' :: Board -> Int -> [Int]
       getPrevFromRow' [] _ = []
       getPrevFromRow' (head : rest) current
         | current == end  = []
         | current < begin = getPrevFromRow' rest (current + 1)
         | otherwise       = head : (getPrevFromRow' rest (current + 1))
   in  getPrevFromRow' board 0

{- Funkcja pomocnicza zwracająca wartości pól znajdujących przed danym polem
   w tej samej kolumnie (w formie listy).
   Argumenty:
   - plansza
   - "długość" boku planszy
   - numer kolumny, z której zwracane są wartości
   - numer wiersza, w którym znajduje się element, dla którego wykonywana jest funkcja
   Z racji tego, że plansza jest tablicą jednowymiarową, funkcja musi przejść
   po wszystkich elementach listy znajdujących się przed ostatnim szukanym elementem. -}    
getPrevFromColumn :: Board -> Int -> Int -> Int -> [Int]
getPrevFromColumn board sideSize column row =
   let begin = (row - 1) * sideSize    -- indeks pierwszego pola znajdującego się w wierszu, w którym znajduje się ostatni szukany element
       end   = begin + column - 1      -- indeks pola, znad którego zwracane są elementy
       {- Argumenty: lista pól, obecny indeks pola -}
       getPrevFromColumn' :: Board -> Int -> [Int]
       getPrevFromColumn' [] _ = []
       getPrevFromColumn' (head : rest) current
         | current == end                         = []
         | (mod current sideSize) /= (column - 1) = getPrevFromColumn' rest (current + 1)  -- kolumny są numerowane od 1, stąd odjęcie 1
         | otherwise                              = head : (getPrevFromColumn' rest (current + 1))
   in  getPrevFromColumn' board 0

{- Funkcja pomocnicza sprawdzająca czy dany element nie będzie się powtarzał w ramach 
   wiersza i kolumny planszy, gdzie ma zostać umieszczony. Zwraca prawdę, jeśli w danej kolumnie
   i wierszu nie ma pola o takiej wartości.
   Argumenty:
   - plansza
   - "długość" boku planszy
   - kolumna, dla której następuje sprawdzenie
   - wiersz, dla którego następuje sprawdzenie
   - wartość, która ma zostać nadana polu w danym wierszu i kolumnie -}   
checkNotRepeating :: Board -> Int -> Int -> Int -> Int -> Bool
checkNotRepeating partialBoard sideSize column row element =
   let prevFromRow    = getPrevFromRow partialBoard sideSize column row
       prevFromColumn = getPrevFromColumn partialBoard sideSize column row
   in  (notIn element prevFromRow) && (notIn element prevFromColumn)

{- Funkcja pomocnicza sprawdzająca liczbę widocznych piramid w danej liście
   patrząc od jej początku. Używana przy sprawdzaniu czy wiersz/kolumna spełnia
   ograniczenia. -}   
visibleNumber :: [Int] -> Int
visibleNumber list = visibleNumber' list 0
   where
   {- Argumenty: lista pól, wysokość najwyższej spotkanej dotychczas piramidy -}
   visibleNumber' :: [Int] -> Int -> Int
   visibleNumber' [] _ = 0
   visibleNumber' (x : rest) currentHighest
      | x > currentHighest = 1 + (visibleNumber' rest x)
      | otherwise          = visibleNumber' rest currentHighest

{- Funkcja pomocnicza sprawdzająca czy dana krotka zawiera szukane ograniczenie,
   czyli takie, które dotyczy właściwej ściany planszy oraz numeru wiersza/kolumny.
   Zwraca liczbę widocznych piramid, jeśli ograniczenie spełnia warunki, 
   a w przeciwnym przypadku "Nothing". -}   
checkIfProperConstraint' :: (Char, Int, Int) -> Char -> Int -> Maybe Int
checkIfProperConstraint' (sym, num, constr) symbol number
   | sym == symbol && num == number = Just constr
   | otherwise                      = Nothing

{- Funkcja pomocnicza sprawdzająca czy szukane ograniczenie, czyli takie, które dotyczy 
   właściwej ściany planszy oraz numeru wiersza/kolumny, znajduje się w liście ograniczeń.
   Zwraca liczbę widocznych piramid, jeśli ograniczenie spełniające warunki znajduje się w liście. 
   W przeciwnym przypadku zwraca "Nothing". -}   
getConstraint :: Constraints -> Char -> Int -> Maybe Int
getConstraint [] _ _ = Nothing
getConstraint (x : restCostraints) sideSymbol number =
   case checkIfProperConstraint' x sideSymbol number of
      Nothing              -> getConstraint restCostraints sideSymbol number
      Just constraintValue -> Just constraintValue

{- Funkcja pomocnicza sprawdzająca czy wiersz/kolumna (elementy wchodzące w jej skład 
   przekazane w argumencie jako lista) spełnia ograniczenie dot. liczby widocznych piramid.
   Jeśli ograniczenie jest spełnione lub go nie ma, zwraca prawdę, w.p.p. fałsz.
   Argumenty:
   - lista ograniczeń
   - symbol ściany planszy
   - number wiersza/kolumny
   - lista elementów wchodzących w skład sprawdzanego wiersza/kolumny (w przypadku ograniczeń
     od prawej strony planszy lub od dołu planszy należy podać odwróconą listę) -}   
checkVisibilityConstraint :: Constraints -> Char -> Int -> [Int] -> Bool
checkVisibilityConstraint constraints sideSymbol number table =
   case getConstraint constraints sideSymbol number of
      Nothing         -> True
      Just constraint -> (visibleNumber table) == constraint

{- Funkcja pomocnicza sprawdzająca czy dany wiersz planszy będzie spełniał wszelkie ograniczenia 
   (dot. liczby widocznych piramid i niepowtarzania się wartości pól w ramach wiersza/kolumny) 
   po wstawieniu danej wartości do ostatniego pola tego wiersza. Zwraca prawdę, jeśli ograniczenia 
   będą spełnione.
   Argumenty:
   - plansza
   - "długość" boku planszy
   - lista ograniczeń
   - numer wiersza
   - wartość mająca być wstawiona do ostatniego pola wiersza -}  
checkRowConstraints :: Board -> Int -> Constraints -> Int -> Int -> Bool
checkRowConstraints board sideSize constraints row element =
   notRepeating && leftConstr && rightConstr
   where
   prevFromRow    = getPrevFromRow board sideSize sideSize row
   prevFromColumn = getPrevFromColumn board sideSize sideSize row
   notRepeating   = (notIn element prevFromRow) && (notIn element prevFromColumn)
   entireRow      = prevFromRow ++ [element]
   leftConstr     = checkVisibilityConstraint constraints leftSymbol row entireRow
   rightConstr    = checkVisibilityConstraint constraints rightSymbol row (reverse entireRow)
   
{- Funkcja pomocnicza sprawdzająca czy dana kolumna planszy będzie spełniała wszelkie ograniczenia 
   (dot. liczby widocznych piramid i niepowtarzania się wartości pól w ramach wiersza/kolumny) 
   po wstawieniu danej wartości do ostatniego pola tej kolumny. Zwraca prawdę, jeśli ograniczenia 
   będą spełnione.
   Argumenty:
   - plansza
   - "długość" boku planszy
   - lista ograniczeń
   - numer kolumny
   - wartość mająca być wstawiona do ostatniego pola kolumny -} 
checkColumnConstraints :: Board -> Int -> Constraints -> Int -> Int -> Bool
checkColumnConstraints board sideSize constraints column element =
   notRepeating && upConstr && downConstr
   where
   prevFromRow    = getPrevFromRow board sideSize column sideSize
   prevFromColumn = getPrevFromColumn board sideSize column sideSize
   notRepeating   = (notIn element prevFromRow) && (notIn element prevFromColumn)
   entireColumn   = prevFromColumn ++ [element]
   upConstr       = checkVisibilityConstraint constraints upSymbol column entireColumn
   downConstr     = checkVisibilityConstraint constraints downSymbol column (reverse entireColumn)
   
{- Jedna z najważniejszych funkcji w programie. Jej zadaniem jest próba wstawienia danej
   wartości (piramidy o danej wysokości) do danego pola. W przypadku gdy się to nie powiedzie, 
   funkcja ta wywołuje siebie, ale dla wartości o 1 mniejszej (jeśli rozpatrywana wartość była 
   większa niż 1). W przeciwnym przypadku funkcja wywołuje siebie, ale dla następnego pola 
   (wstawiając obecną wartość na koniec listy reprezentującej planszę). Jeśli wartość przekazana w 
   argumencie funkcji jest równa 0, to żadna sprawdzana wcześniej piramida nie spełnia 
   ograniczeń i należy dokonać zmian w poprzednich polach planszy. Wyróżnia się różne sytuacje, 
   które zależą od położenia pola na planszy. Każda z nich zakłada sprawdzanie innych ograniczeń. 
   Te sytuacje to: koniec łamigłówki, ostatnie pole (jednocześnie koniec wiersza i kolumny), koniec wiersza, 
   koniec kolumny i "zwykłe" pole.
   Argumenty:
   - plansza
   - "długość" boku planszy
   - lista ograniczeń
   - indeks rozpatrywanego pola
   - wartość, którą próbuje się wstawić do danego pola -}
tryElement :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElement _ _ _ _ 0 = Nothing
tryElement board sideSize constraints fieldNumber element
   | puzzleComplete = Just board
   | lastField      = tryElementForBoardEnd board sideSize constraints fieldNumber element
   | rowEnd         = tryElementForRowEnd board sideSize constraints fieldNumber element
   | columnEnd      = tryElementForColumnEnd board sideSize constraints fieldNumber element
   | notRepeating   = tryNext' board sideSize constraints fieldNumber element
   | otherwise      = tryElement board sideSize constraints fieldNumber (element - 1)
   where
   puzzleComplete = sideSize ^ 2 == fieldNumber
   lastField      = sideSize ^ 2 - 1 == fieldNumber
   rowNumber      = (div fieldNumber sideSize) + 1
   columnNumber   = (mod fieldNumber sideSize) + 1
   rowEnd         = columnNumber == sideSize
   columnEnd      = rowNumber == sideSize
   notRepeating   = checkNotRepeating board sideSize columnNumber rowNumber element

{- Funkcja pomocnicza, której zadaniem jest próba wstawienia danej wartości do pola
   znajdującego się na końcu wiersza. W przypadku spełnienia ograniczeń, wartość zostaje
   wstawiona i następuje kontynuacja rozwiązywania łamigłówki. W przeciwnym przypadku
   próbuje się wstawić na to pole wartość o 1 mniejszą. -}
tryElementForRowEnd :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElementForRowEnd board sideSize constraints fieldNumber element
   | rowConstr = tryNext' board sideSize constraints fieldNumber element
   | otherwise = tryElement board sideSize constraints fieldNumber (element - 1)
   where
   rowNumber = (div fieldNumber sideSize) + 1
   rowConstr = checkRowConstraints board sideSize constraints rowNumber element

{- Funkcja pomocnicza, której zadaniem jest próba wstawienia danej wartości do pola
   znajdującego się na końcu kolumny. W przypadku spełnienia ograniczeń, wartość zostaje
   wstawiona i następuje kontynuacja rozwiązywania łamigłówki. W przeciwnym przypadku
   próbuje się wstawić na to pole wartość o 1 mniejszą. -}  
tryElementForColumnEnd :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElementForColumnEnd board sideSize constraints fieldNumber element
   | columnConstr = tryNext' board sideSize constraints fieldNumber element
   | otherwise    = tryElement board sideSize constraints fieldNumber (element - 1)
   where
   columnNumber = (mod fieldNumber sideSize) + 1
   columnConstr = checkColumnConstraints board sideSize constraints columnNumber element

{- Funkcja pomocnicza, której zadaniem jest próba wstawienia danej wartości do pola
   znajdującego się na końcu planszy. W przypadku spełnienia ograniczeń, wartość zostaje
   wstawiona i następuje kontynuacja rozwiązywania łamigłówki. W przeciwnym przypadku
   próbuje się wstawić na to pole wartość o 1 mniejszą. -}     
tryElementForBoardEnd :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryElementForBoardEnd board sideSize constraints fieldNumber element
   | columnConstr && rowConstr = tryNext' board sideSize constraints fieldNumber element
   | otherwise                 = tryElement board sideSize constraints fieldNumber (element - 1)
   where
   columnNumber = (mod fieldNumber sideSize) + 1
   columnConstr = checkColumnConstraints board sideSize constraints columnNumber element
   rowNumber    = (div fieldNumber sideSize) + 1
   rowConstr    = checkRowConstraints board sideSize constraints rowNumber element

{- Funkcja pomocnicza używana we wszystkich funkcjach, których nazwa zaczyna się od
   "tryElement". Jej zadaniem jest próba wykonania funkcji "tryElement" dla następnego
   pola planszy. W przypadku gdy zakończy się ona sukcesem (wynik nie jest "Nothing"), 
   zwraca wypełnioną planszę, w przeciwnym przypadku wywołuje metodę "tryElement" dla
   wartości o 1 mniejszej. -}     
tryNext' :: Board -> Int -> Constraints -> Int -> Int -> Maybe Board
tryNext' board sideSize constraints fieldNumber element =
   case tryElement (board ++ [element]) sideSize constraints (fieldNumber + 1) sideSize of
      Nothing -> tryElement board sideSize constraints fieldNumber (element - 1)
      Just x  -> Just x  

{- Funkcja pomocnicza wyświetlająca zawartość planszy. Używana po rozwiązaniu łamigłówki. -}   
printSolution :: Maybe Board -> Int -> Int -> IO ()
printSolution Nothing _ _ = do putStr "Brak rozwiązania \n"
printSolution (Just []        ) _        _           = return ()
printSolution (Just (x : rest)) sideSize fieldNumber = do
   putStr (show x)
   if (mod fieldNumber sideSize) + 1 == sideSize
      then do
        putStr "\n"
      else do
        putStr " "
   printSolution (Just rest) sideSize (fieldNumber + 1)

{- Funkcja pomocnicza pozwalająca na stworzenie krotki z ograniczeniem. -}
inputConstraint :: Char -> IO (Char, Int, Int)
inputConstraint sideSymbol = do
   putStr "Wpisz numer rzędu/kolumny: "
   column <- getLine
   putStr "Wpisz ograniczenie widoczności: "
   visibility <- getLine
   return (sideSymbol, (read column :: Int), (read visibility :: Int))

{- Funkcja będąca "główną pętlą programu". Służy do wczytania ograniczeń widoczności, wywołania
   funkcji rozwiązującej łamigłówkę oraz wyświetlenia wyniku.-}   
puzzleSolver :: Int -> Constraints -> IO ()
puzzleSolver sideSize constraints = do
   putStr
      "Wpisz symbol ściany planszy (u-górna, l-lewa, r-prawa, d-dolna, n-żadna -> koniec wpisywania): "
   sideSymbol <- getChar
   getLine
   if sideSymbol == 'n'
      then do
         printSolution (tryElement [] sideSize constraints 0 sideSize) sideSize 0
         return ()
      else do
         newConstraint <- inputConstraint sideSymbol
         puzzleSolver sideSize ( [newConstraint] ++ constraints )
   
main = do
   putStr "Wpisz rozmiar boku planszy: "
   sideSize <- getLine
   puzzleSolver (read sideSize :: Int) []
   