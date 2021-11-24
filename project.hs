import System.Random
import Data.List
import Data.Maybe

-- STEP 1
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Show, Enum)

data Pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace deriving (Eq, Show, Enum)

type Card = (Pip, Suit)
type Deck = [Card]


-- STEP 2
pack :: Deck
pack = [(p, s) | p <- [Two .. Ace], s <- [Hearts .. Diamonds]]

sCard :: Card -> Card -- exception when no succ (probably need to use isAce)
sCard (Ace, s) = (Two, s) 
sCard (p, s) = (succ p, s)

pCard :: Card -> Card -- exception when no pred
pCard (Two, s) = (Ace, s)
pCard (p, s) = (pred p, s)

isAce :: Card -> Bool
isAce (p, s) = p == Ace

isKing :: Card -> Bool
isKing (p, s) = p == King


cmp :: Ord b => (a, b) -> (a, b) -> Ordering -- think about types
cmp (x1,y1) (x2,y2) = compare y1 y2

shuffle :: Int -> Deck
shuffle n = [card | (card, n) <- sortBy cmp (zip pack (randoms (mkStdGen n) :: [Int]))] -- how to get random n


-- STEP 3
type Foundation = [Card]
type Column = [[Card]]
type Reserve = [Card]

data Board = EOBoard Foundation Column Reserve | SBoard deriving Eq

instance Show Board where
    show t = showBoard t
        where
          showBoard (EOBoard foundations columns reserves) = "EOBoard \nFoundations  " 
            ++ (wrapSquareBracket.showCards) foundations
            ++ "\nColumns\n" ++ unpackColumns "\n" columns ++ "\nReserve      "
            ++ (wrapSquareBracket.showCards) reserves

          showBoard SBoard = "board" ++ "\n" ++ "of S"

          -- create a String representation of a list of cards
          showCards [] = "" -- might be redundant
          showCards [card] = displayCard card
          -- showCards [(card:rest)] = displayCard card
          showCards (card:restCards) = displayCard card ++ "," ++ (showCards restCards)

          -- creates a String representation of a card pair
          displayCard card = "(" ++ (show (fst card)) ++ "," ++ (show (snd card)) ++ ")"

          -- it works
          wrapSquareBracket :: String -> String
          wrapSquareBracket cards = "[" ++ cards ++ "]"

          -- cretates a string representation of nested lists of cards
          unpackColumns :: String -> [[Card]] -> String
          unpackColumns separator [] = "" -- might be redundant
          unpackColumns separator [card] = "  " ++ ((wrapSquareBracket.showCards) card)
          unpackColumns separator (column:restColumns) = "  " ++ ((wrapSquareBracket.showCards) column) 
            ++ separator ++ (unpackColumns separator restColumns)


displayEO = EOBoard [] [[]] []
eODeal n = EOBoard [] ((getColumns.groupCards.shuffle) n) ((getReserve.groupCards.shuffle) n)

groupCards [] = []
groupCards list = [fst (splitAt 6 (list))] ++ groupCards (snd (splitAt 6 (list)))

getColumns list = tail (reverse list)
getReserve :: [[Card]] -> [Card]
getReserve list = head (reverse list)


-- toFoundations
toFoundations (EOBoard f c r) | newBoard /= (EOBoard f c r) = toFoundations newBoard
                              | otherwise                              = EOBoard f c r
                               where 
                                 newBoard = manageSubsC . manageSubsR . manageAcesC  . manageAcesR $ EOBoard f c r

manageAcesR (EOBoard f c r) = aceInList (EOBoard f c r) r
manageAcesC (EOBoard f c r) = aceInColumns (EOBoard f c r) c
manageSubsR (EOBoard f c r) = lookForSubsR (EOBoard f c r) (length f - 1)
manageSubsC (EOBoard f c r) = lookForSubsC (EOBoard f c r) (length f - 1)

aceInList :: Board -> [Card] -> Board
aceInList (EOBoard f c r) [] = EOBoard f c r
aceInList (EOBoard f c r) (x:xs) | fst x == Ace  = EOBoard (f ++ [x]) c (deleteCard r x)
                                 | otherwise     = aceInList (EOBoard f c r) xs

aceInColumns :: Board -> [[Card]] -> Board
aceInColumns (EOBoard f c r) [] = EOBoard f c r
aceInColumns (EOBoard f c r) (x:xs) | (length x) == 0        = aceInColumns (EOBoard f c r) xs
                                    | (fst (head x)) == Ace  = EOBoard (f ++ [(head x)]) (replaceColumn c x) r
                                    | otherwise           = aceInColumns (EOBoard f c r) xs

-- takes a board, list and card, then goes through all the cards in list looking for succesor, if found than adding it to f deleteting from r
subInList :: Board -> [Card] -> Card -> Board
subInList (EOBoard f c r) [] card = EOBoard f c r
subInList (EOBoard f c r) (x:xs) card | x == succ  = EOBoard (replaceCard f card x) c (deleteCard r x)
                                      | otherwise  = subInList (EOBoard f c r) xs card
                                        where succ = sCard (card)

-- 
subInColumns :: Board -> [[Card]] -> Card -> Board
subInColumns (EOBoard f c r) [] card = EOBoard f c r
subInColumns (EOBoard f c r) (x:xs) card  | (length x) == 0           =  subInColumns (EOBoard f c r) xs card
                                          | (head x) == succ  = EOBoard (replaceCard f card (head x)) (replaceColumn c x) r
                                          | otherwise         = subInColumns (EOBoard f c r) xs card
                                           where succ = sCard (card)

lookForSubsR :: Board -> Int -> Board
lookForSubsR (EOBoard f c r) (-1) = EOBoard f c r
lookForSubsR (EOBoard f c r) index | newBoard /= EOBoard f c r   = newBoard
                                   | otherwise                   = lookForSubsR (EOBoard f c r) (index - 1)
                                    where
                                      newBoard = subInList (EOBoard f c r) r (f!!index)

lookForSubsC :: Board -> Int -> Board
lookForSubsC (EOBoard f c r) (-1) = EOBoard f c r
lookForSubsC (EOBoard f c r) index | newBoard /= EOBoard f c r   = newBoard
                                   | otherwise                   = lookForSubsC (EOBoard f c r) (index - 1)
                                    where
                                      newBoard = subInColumns (EOBoard f c r) c (f!!index)

-- helper
replaceColumn :: [[Card]] -> [Card] -> [[Card]]
replaceColumn c column = replace column (deleteCard column (head column)) c


replaceCard :: [Card] -> Card -> Card -> [Card]
replaceCard [] card1 card2 = []  
replaceCard (x:xs) card1 card2 | x == card1     = card2 : xs
                               | otherwise      = x : replaceCard xs card1 card2

deleteCard :: [Card] -> Card -> [Card]
deleteCard list card = delete card list

-- replace all occurences in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs) | old == x     = new:(replace old new xs)
                       | otherwise    = x:(replace old new xs)


-- findMoves :: Board -> [Board]

-- if there is free space in reserve then it adds a card there
freeSpaceInR :: Board -> Card -> Board
freeSpaceInR (EOBoard f c r) card | length (r) < 8  = EOBoard f c (r ++ [card])
                                  | otherwise = EOBoard f c r

-- the same as before except the action (create just one function)
-- successorInC :: Board -> Card -> [[Card]] -> Board
-- successorInC (EOBoard f c r) (x:xs) card |  (length x) == 0           =  subInColumns (EOBoard f c r) xs card
--                                          |  (head x) == succ  = EOBoard f (replaceColumn c x) (replaceColumn c x) r
--                                          | 


-- chooseMove :: Board -> Maybe Board

haveWon :: Board -> Bool
haveWon (EOBoard f [[]] []) = True
haveWon board = False 

-- playSolitair :: Board -> Int
