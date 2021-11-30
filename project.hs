import System.Random
import Data.List
import Data.Maybe


data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Show, Enum)

data Pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace deriving (Eq, Show, Enum)

type Card = (Pip, Suit)
type Deck = [Card]
type Stock = [Card]
type Foundation = [Card]
type Column = [[Card]]
type Reserve = [Card]
type Hidden = [Int]

pack :: Deck
pack = [(p, s) | p <- [Two .. Ace], s <- [Hearts .. Diamonds]]

sCard :: Card -> Card
sCard (Ace, s) = (Two, s) 
sCard (p, s) = (succ p, s)

pCard :: Card -> Card
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




data Board = EOBoard Foundation Column Reserve | SBoard Foundation Column Hidden Stock deriving Eq

instance Show Board where
    show t = showBoard t
        where
          showBoard (EOBoard foundations columns reserves) = "EOBoard \nFoundations  " 
            ++ wrapSquareBracket (showCards foundations 0)
            ++ "\nColumns\n" ++ unpackColumns "\n" columns [0,0,0,0,0,0,0,0] ++ "\nReserve      "
            ++ wrapSquareBracket (showCards reserves 0)

          
          showBoard (SBoard foundations columns hidden stock) = "SBoard \nFoundations  " 
            ++ (wrapSquareBracket.showCards foundations) 0
            ++ "\nColumns\n" ++ unpackColumns "\n" columns hidden ++ "\nStock  "
            ++ show ((length (stock)) `div` 10) ++ " Deals remaining"


          -- create a String representation of a list of cards
          showCards :: [Card] -> Int -> String
          showCards [] _ = "" -- might be redundant
          showCards [card] 0 = displayCard card
          showCards [card] n  = "<unknown>"
          showCards (card:restCards) n | (length (card:restCards)) > n  = displayCard card ++ "," ++ (showCards restCards n)
                                       | otherwise                      = "<unknown>" ++ "," ++ (showCards restCards n)

          -- creates a String representation of a card pair
          displayCard card = "(" ++ (show (fst card)) ++ "," ++ (show (snd card)) ++ ")"

          wrapSquareBracket :: String -> String
          wrapSquareBracket cards = "[" ++ cards ++ "]"

          -- cretates a string representation of nested lists of cards
          unpackColumns :: String -> [[Card]] -> [Int] -> String
          unpackColumns separator [] _ = "" -- might be redundant
          unpackColumns separator [card] [x] = "  " ++ (wrapSquareBracket(showCards card x))
          unpackColumns separator (column:restColumns) (x:xs) = "  " ++ (wrapSquareBracket (showCards column x)) 
            ++ separator ++ (unpackColumns separator restColumns xs)


displayEO = EOBoard [] [[]] []
eODeal n = EOBoard [] ((getColumns.groupCards.shuffle) n) ((getReserve.groupCards.shuffle) n)

groupCards [] = []
groupCards list = [fst (splitAt 6 (list))] ++ groupCards (snd (splitAt 6 (list)))

getColumns list = tail (reverse list)

getReserve :: [[Card]] -> [Card]
getReserve list = head (reverse list)

splitColumns _ [] = []
splitColumns n list =  [fst (splitAt n (list))] ++ splitColumns n (snd (splitAt n (list)))

sDeal n = SBoard [] ((splitColumns 6 (fst cardsToDeal)) ++ (splitColumns 5 (snd cardsToDeal))) [5,5,5,5,4,4,4,4,4,4] (snd cards)
    where 
      cards = splitAt 54 (shuffleS n)
      cardsToDeal = splitAt 24 (fst cards)

shuffleS n = [card | (card, n) <- sortBy cmp (zip (pack ++ (shuffle n)) (randoms (mkStdGen n) :: [Int]))]


-- toFoundations
-- applies toFoundation until the supplied board is the same as the one after applying all subfunctions (no moves to foundations are possible)
toFoundations (EOBoard f c r) | newBoard /= (EOBoard f c r) = toFoundations newBoard
                              | otherwise                              = EOBoard f c r
                               where 
                                 newBoard = manageSubsC . manageSubsR . manageAcesC  . manageAcesR $ EOBoard f c r

-- hides complicated calls in simple definition that only takes EOBoard as an argument
manageAcesR (EOBoard f c r) = aceInList (EOBoard f c r) r
manageAcesC (EOBoard f c r) = aceInColumns (EOBoard f c r) c
manageSubsR (EOBoard f c r) = lookForSubsR (EOBoard f c r) (length f - 1)
manageSubsC (EOBoard f c r) = lookForSubsC (EOBoard f c r) (length f - 1)

-- takes board and list of cards, then if card first card in list is ace, it's getting added to f and deleted from r 
aceInList :: Board -> [Card] -> Board
aceInList (EOBoard f c r) [] = EOBoard f c r
aceInList (EOBoard f c r) (x:xs) | fst x == Ace  = EOBoard (f ++ [x]) c (deleteCard r x)
                                 | otherwise     = aceInList (EOBoard f c r) xs

aceInColumns :: Board -> [[Card]] -> Board
aceInColumns (EOBoard f c r) [] = EOBoard f c r
aceInColumns (EOBoard f c r) (x:xs) | (length x) == 0        = aceInColumns (EOBoard f c r) xs
                                    | (fst (head x)) == Ace  = EOBoard (f ++ [(head x)]) (replace x newX c) r
                                    | otherwise           = aceInColumns (EOBoard f c r) xs
                                      where
                                        newX = deleteCard x (head x)

-- takes a board, list and card, then goes through all the cards in list looking for succesor, if found than adding it to f deleteting from r
subInList :: Board -> [Card] -> Card -> Board
subInList (EOBoard f c r) [] card = EOBoard f c r
subInList (EOBoard f c r) (x:xs) card | x == succ  = EOBoard (replaceCard f card x) c (deleteCard r x)
                                      | otherwise  = subInList (EOBoard f c r) xs card
                                        where succ = sCard (card)

-- 
subInColumns :: Board -> [[Card]] -> Card -> Board
subInColumns (EOBoard f c r) [] card = EOBoard f c r
subInColumns (EOBoard f c r) (x:xs) card  | (length x) == 0           = subInColumns (EOBoard f c r) xs card
                                          | (head x) == succ          = EOBoard (replaceCard f card (head x)) (replace x newX c) r
                                          | otherwise                 = subInColumns (EOBoard f c r) xs card
                                           where 
                                             succ = sCard (card)
                                             newX = deleteCard x (head x)


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
replaceCard :: [Card] -> Card -> Card -> [Card]
replaceCard [] old new = []  
replaceCard (x:xs) old new | x == old     = new : xs
                               | otherwise      = x : replaceCard xs old new

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
successorInC :: [Board] -> Board -> [[Card]] -> Card -> [Board]
successorInC boards board [] _ = boards
successorInC boards (EOBoard f c r) (x:xs) card 
                                | and [((fst card) == King), ((length x) == 0)]         = boards ++ [newBoard]  
                                | (length x) == 0                                       = successorInC boards (EOBoard f c r) xs card
                                | (head x) == succ                                      = if newBoard `elem` boards then
                                                                                             successorInC boards (EOBoard f c r) xs card else boards ++ [newBoard]
                                | otherwise                                             = successorInC boards (EOBoard f c r) xs card
                                    where 
                                      succ = sCard (card)
                                      newX = [card] ++ x
                                      newBoard = EOBoard f (replace x newX c) r

callMoves (EOBoard f c r) card = (successorInC [] (EOBoard f c r) c card) ++ [freeSpaceInR (EOBoard f c r) card]

-- chooseMove :: Board -> Maybe Board

haveWon :: Board -> Bool
haveWon (EOBoard f [[]] []) = True
haveWon board = False 

-- playSolitair :: Board -> Int




-- {- Paste the contents of this file, including this comment, into your source file, below all
--      of your code. You can change the indentation to align with your own, but other than this,
--      ONLY make changes as instructed in the comments.
--    -}
--   -- Constants that YOU must set:
--   studentName = "Your Name Here"
--   studentNumber = "Your Student Number"
--   studentUsername = "Your Student Username"

--   initialBoardDefined = XXX {- replace XXX with the name of the constant that you defined
--                                in step 3 of part 1 -}
--   secondBoardDefined = YYY {- replace YYY with the constant defined in step 5 of part 1,
--                               or if you have chosen to demonstrate play in a different game
--                               of solitaire for part 2, a suitable contstant that will show
--                               your play to good effect for that game -}

--   {- Beyond this point, the ONLY change you should make is to change the comments so that the
--      work you have completed is tested. DO NOT change anything other than comments (and indentation
--      if needed). The comments in the template file are set up so that only the constant eight-off
--      board from part 1 and the toFoundations function from part 1 are tested. You will probably
--      want more than this tested.

--      CHECK with Emma or one of the demonstrators if you are unsure how to change this.

--      If you mess this up, your code will not compile, which will lead to being awarded 0 marks
--      for functionality and style.
--   -}

--   main :: IO()
--   main =
--     do
--       putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

--       putStrLn "***The eight-off initial board constant from part 1:"
--       print initialBoardDefined

--       let board = toFoundations initialBoardDefined
--       putStrLn "***The result of calling toFoundations on that board:"
--       print board

--       {- Move the start comment marker below to the appropriate position.
--         If you have completed ALL the tasks for the assignment, you can
--         remove the comments from the main function entirely.
--         DO NOT try to submit/run non-functional code - you will receive 0 marks
--         for ALL your code if you do, even if *some* of your code is correct.
--       -}

--       {- start comment marker - move this if appropriate

--       let boards = findMoves board      -- show that findMoves is working
--       putStrLn "***The possible next moves after that:"
--       print boards

--       let chosen = chooseMove board     -- show that chooseMove is working
--       putStrLn "***The chosen move from that set:"
--       print chosen

--       putStrLn "***Now showing a full game"     -- display a full game
--       score <- displayGame initialBoardDefined 0
--       putStrLn $ "Score: " ++ score
--       putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


--       putStrLn "\n\n\n************\nNow looking at the alternative game:"

--       putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
--       print secondBoardDefined          -- show the suitable constant. For spider solitaire this
--                                         -- is not an initial game, but a point from which the game
--                                         -- can be won

--       putStrLn "***Now showing a full game for alternative solitaire"
--       score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
--                                                 -- works correctly)
--       putStrLn $ "Score: " ++ score
--       putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

--       -}

--   {- displayGame takes a Board and move number (should initially be 0) and
--      displays the game step-by-step (board-by-board). The result *should* be
--      the same as performing playSolitaire on the initial board, if it has been
--      implemented correctly.
--      DO NOT CHANGE THIS CODE other than aligning indentation with your own.
--   -}
--   displayGame :: Board -> Int ->IO String
--   displayGame board n =
--     if haveWon board
--       then return "A WIN"
--       else
--         do
--           putStr ("Move " ++ show n ++ ": " ++ show board)
--           let maybeBoard = chooseMove board
--           if isJust maybeBoard then
--             do
--               let (Just newBoard) = maybeBoard
--               displayGame newBoard (n+1)
--           else
--             do
--               let score = show (playSolitaire board)
--               return score
