import System.Random
import Data.List
import Data.Maybe



-- 1. TYPES
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Show, Enum)

data Pip =  Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King deriving (Eq, Show, Enum)

data Board = EOBoard Foundation Column Reserve | SBoard Foundation Column Hidden Stock  deriving Eq

type Card = (Pip, Suit)
type Deck = [Card]
type Foundation = [Card]
type Column = [[Card]]
type Reserve = [Card]
type Hidden = [Int] -- List of numbers of cards that should not be visible in SBoard
type Stock = [Card]

pack :: Deck
pack = [(p, s) | p <- [Ace .. King], s <- [Hearts .. Diamonds]]

sCard :: Card -> Card
sCard (King, s) = (Ace, s)
sCard (p, s) = (succ p, s)

pCard :: Card -> Card
pCard (Ace, s) = (King, s)
pCard (p, s) = (pred p, s)

isAce :: Card -> Bool
isAce (p, s) = p == Ace

isKing :: Card -> Bool
isKing (p, s) = p == King



-- 2. DISPLAYING BOARDS
-- Displaying boards (f - foundations, c - columns, r - reserves, h - hidden, s - stock)
instance Show Board where
    show t = showBoard t
        where
          -- Displaying EOBoard
          showBoard (EOBoard f c r) = "EOBoard \nFoundations  " 
            ++ wrapSquareBracket (displayCards f 0)
            ++ "\nColumns\n" ++ displayColumns "\n" c [0,0,0,0,0,0,0,0] ++ "\nReserve      "
            ++ wrapSquareBracket (displayCards r 0)

          -- Displaying SBoard
          showBoard (SBoard f c h s) = "SBoard \nFoundations  " 
            ++ wrapSquareBracket (displayCards f 0)
            ++ "\nColumns\n" ++ displayColumns "\n" c h ++ "\nStock  "
            ++ show ((length s) `div` 10) ++ " Deals remaining"

          -- Creates a String representation of a card pair
          displayCard :: Card -> String
          displayCard (pip, suit) = "(" ++ (show pip) ++ "," ++ (show suit) ++ ")"

          -- Creates a String representation of a list of cards
          displayCards :: [Card] -> Int -> String
          displayCards [] _ = ""
          displayCards [card] 0 = displayCard card
          displayCards [card] n  = "<unknown>"
          displayCards (card:restCards) n | (length (card:restCards)) > n  = displayCard card ++ "," ++ (displayCards restCards n)
                                          | otherwise                      = "<unknown>" ++ "," ++ (displayCards restCards n)

          -- Wraps a list of cards in a square bracket
          wrapSquareBracket :: String -> String
          wrapSquareBracket cards = "[" ++ cards ++ "]"

          -- Creates a string representation of nested lists of cards
          displayColumns :: String -> [[Card]] -> [Int] -> String
          displayColumns separator [] _ = "" 
          displayColumns separator [card] [x] = "  " ++ (wrapSquareBracket(displayCards card x))
          displayColumns separator (column:restColumns) (x:xs) = "  " ++ (wrapSquareBracket (displayCards column x)) 
            ++ separator ++ (displayColumns separator restColumns xs)

-- Deals a deck of cards
eODeal :: Int -> Board
eODeal n = EOBoard [] ((getColumns.groupCards.shuffle) n) ((getReserve.groupCards.shuffle) n)

sDeal :: Int -> Board
sDeal n = SBoard [] ((splitColumns 6 (fst cardsToDeal)) ++ (splitColumns 5 (snd cardsToDeal))) [5,5,5,5,4,4,4,4,4,4] (snd cards)
    where 
      cards = splitAt 54 (shuffleS n)
      cardsToDeal = splitAt 24 (fst cards)

-- Helpers fot initial layouts
cmp :: Ord b => (a, b) -> (a, b) -> Ordering
cmp (x1,y1) (x2,y2) = compare y1 y2

shuffle :: Int -> Deck
shuffle n = [card | (card, n) <- sortBy cmp (zip pack (randoms (mkStdGen n) :: [Int]))]

-- Shuffles pack then adds another and shuffles both of them again
shuffleS :: Int -> Deck
shuffleS n = [card | (card, n) <- sortBy cmp (zip (pack ++ (shuffle n)) (randoms (mkStdGen n) :: [Int]))]

groupCards :: [Card] -> [[Card]]
groupCards [] = []
groupCards list = [fst (splitAt 6 (list))] ++ groupCards (snd (splitAt 6 (list)))

getColumns :: [[Card]] -> [[Card]]
getColumns list = tail (reverse list)

getReserve :: [[Card]] -> [Card]
getReserve list = head (reverse list)

splitColumns :: Int -> [Card] -> [[Card]]
splitColumns _ [] = []
splitColumns n list =  [fst (splitAt n (list))] ++ splitColumns n (snd (splitAt n (list)))

-- calling layouts
displayEO = eODeal 1236
displayS = sDeal 1238



-- 3.TOFOUNDATIONS
-- Applies toFoundation until the supplied board is the same as the one after applying all subfunctions (no moves to foundations are possible)
toFoundations :: Board -> Board
toFoundations (EOBoard f c r) | newBoard /= (EOBoard f c r) = toFoundations newBoard
                              | otherwise                   = EOBoard f c r
                               where 
                                 newBoard = callSuccInC . callSuccInR . callAcesInC  . callAcesInR $ EOBoard f c r

-- Hides complicated calls in simple definition that only takes EOBoard as an argument
callAcesInR :: Board -> Board
callAcesInR (EOBoard f c r) = acesInR (EOBoard f c r) r

callAcesInC :: Board -> Board
callAcesInC (EOBoard f c r) = acesInC (EOBoard f c r) c

callSuccInR :: Board -> Board
callSuccInR (EOBoard f c r) = lookForSuccR (EOBoard f c r) (length f - 1)

callSuccInC :: Board -> Board
callSuccInC (EOBoard f c r) = lookForSuccC (EOBoard f c r) (length f - 1)

-- takes board and list of cards, then if card first card in list is ace, it's getting added to f and deleted from r 
acesInR :: Board -> [Card] -> Board
acesInR (EOBoard f c r) [] = EOBoard f c r
acesInR (EOBoard f c r) (x:xs) | fst x == Ace  = EOBoard (f ++ [x]) c (delete x r)
                                 | otherwise     = acesInR (EOBoard f c r) xs

acesInC :: Board -> [[Card]] -> Board
acesInC (EOBoard f c r) [] = EOBoard f c r
acesInC (EOBoard f c r) (x:xs) | (length x) == 0        = acesInC (EOBoard f c r) xs
                                    | (fst (head x)) == Ace  = EOBoard (f ++ [(head x)]) (replace x newX c) r
                                    | otherwise           = acesInC (EOBoard f c r) xs
                                      where
                                        newX = delete (head x) x 

-- takes a board, list and card, then goes through all the cards in list looking for succesor, if found than adding it to f deleteting from r
aceSuccessorInR :: Board -> [Card] -> Card -> Board
aceSuccessorInR (EOBoard f c r) [] card = EOBoard f c r
aceSuccessorInR (EOBoard f c r) (x:xs) card | x == succ  = EOBoard (replace card x f) c (delete x r)
                                      | otherwise  = aceSuccessorInR (EOBoard f c r) xs card
                                        where succ = sCard (card)

aceSuccessorInC :: Board -> [[Card]] -> Card -> Board
aceSuccessorInC (EOBoard f c r) [] card = EOBoard f c r
aceSuccessorInC (EOBoard f c r) (x:xs) card  | (length x) == 0           = aceSuccessorInC (EOBoard f c r) xs card
                                          | (head x) == succ          = EOBoard (replace card (head x) f) (replace x newX c) r
                                          | otherwise                 = aceSuccessorInC (EOBoard f c r) xs card
                                           where 
                                             succ = sCard (card)
                                             newX = delete (head x) x

lookForSuccR :: Board -> Int -> Board
lookForSuccR (EOBoard f c r) (-1) = EOBoard f c r
lookForSuccR (EOBoard f c r) index | newBoard /= EOBoard f c r   = newBoard
                                   | otherwise                   = lookForSuccR (EOBoard f c r) (index - 1)
                                    where
                                      newBoard = aceSuccessorInR (EOBoard f c r) r (f!!index)

lookForSuccC :: Board -> Int -> Board
lookForSuccC (EOBoard f c r) (-1) = EOBoard f c r
lookForSuccC (EOBoard f c r) index | newBoard /= EOBoard f c r   = newBoard
                                   | otherwise                   = lookForSuccC (EOBoard f c r) (index - 1)
                                    where
                                      newBoard = aceSuccessorInC (EOBoard f c r) c (f!!index)

-- replace all occurences in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs) | old == x     = new:(replace old new xs)
                       | otherwise    = x:(replace old new xs)



-- 4. FINDMOVES

---- 4.1 FREESPACEINR AND SUCCESSORINC
-- Adds card to a free space in Reserves
freeSpaceInR :: Board -> Card -> [Board]
freeSpaceInR (EOBoard f c r) card | (length r) < 8  = [EOBoard f c (r ++ [card])]
                                  | otherwise = []

-- Finds all possible position of given card and returns the results as list of boards
successorInC :: [Board] -> Board -> [[Card]] -> Card -> [Board]
successorInC boards board [] _ = boards
successorInC boards (EOBoard f c r) (x:xs) card 
                                | x == []                                               = successorInC boards (EOBoard f c r) xs card
                                | (head x) == succ                                      = if newBoard `elem` boards then
                                                                                             successorInC boards (EOBoard f c r) xs card else boards ++ [newBoard]
                                | otherwise                                             = successorInC boards (EOBoard f c r) xs card
                                    where 
                                      succ = sCard (card)
                                      newX = [card] ++ x
                                      newBoard = EOBoard f (replace x newX c) r


---- 4.2 FINDMOVEKING
-- Count number of possible places for a King
countEmptyColumns :: [[Card]] -> Int
countEmptyColumns [] = 0
countEmptyColumns (x:xs) | x == [] = 1 + countEmptyColumns xs
                         | otherwise = countEmptyColumns xs

-- Insert a King when coundown will reach 0 (to avoid inserting them in all empty possition at ones)
insertKing :: [[Card]] -> Card -> Int -> [[Card]]
insertKing [] _ _ = []
insertKing (x:xs) card 0 | x == []   = [card]:xs
                         | otherwise = x : insertKing xs card 0
insertKing (x:xs) card n | x == []    = x : insertKing xs card (n - 1)
                         | otherwise  = x : insertKing xs card n

-- Generate all possible Columns with King (number of empty spaces)
populateAllEmpty :: [[Card]] -> Card -> Int -> [[[Card]]]
populateAllEmpty list card (-1) = []
populateAllEmpty (x:xs) card n = [insertKing (x:xs) card n] ++ populateAllEmpty (x:xs) card (n-1)

-- Connects Foundations and Reserves with generated Columns
generateBoardKings :: Board -> [[[Card]]] -> [Board]
generateBoardKings board [] = []
generateBoardKings (EOBoard f c r) (x:xs) = [EOBoard f x r] ++ generateBoardKings (EOBoard f c r) xs

-- Encapsulates all functions above in one call
findMoveKing :: Board -> Card -> [Board]
findMoveKing (EOBoard f c r) card | isKing(card)   = generateBoardKings (EOBoard f c r) (populateAllEmpty c card ((countEmptyColumns c) - 1))
                                  | otherwise      = []


---- 4.3 MOVEGROUPCARDS
-- Adds a group of cards to available position
addList :: [Card] -> [[Card]] -> [[Card]]
addList list [] = []
addList list (col:cols) | col == []                                  = col : addList list cols
                        | (head col) == sCard ( head (reverse list)) = (list ++ col) : cols
                        | otherwise                                  = col : addList list cols

-- Generates boards based on suplied new columns
addColBoards :: [Card] -> [Board] -> [Board]
addColBoards _ [] = []
addColBoards list (board:boards) = (EOBoard f newC r) : addColBoards list boards
                                      where
                                        (EOBoard f c r) = board
                                        newC = addList list c

deleteDuplicates :: [Board] -> [Board] -> [Board]
deleteDuplicates _ [] = []
deleteDuplicates [] _ = []
deleteDuplicates (newBoard:newBoards) boards  | newBoard `elem` boards =  deleteDuplicates newBoards boards
                                              | otherwise              = (toFoundations newBoard) : deleteDuplicates newBoards boards

-- Takes Cards from list up to the last successor
getListSucc :: [Card] -> [Card]
getListSucc [] = []
getListSucc [card1] = []
getListSucc (card1:card2:cards) | sCard(card1) == card2  = card1 : getListSucc (card2:cards)
                                | otherwise              = [] 

-- Generates boards by iterating through columns and using functions above
moveGroupCards :: Board -> [[Card]] -> [Board] -> [Board]
moveGroupCards _ [] boards = boards
moveGroupCards (EOBoard f c r) (col:cols) boards | (col:cols) == []  = boards
                                               | (col:cols) == [[]] = boards
                                               | col == []         = moveGroupCards (EOBoard f c r) cols boards
                                               | if ((length col) > 1) then (sCard (head col)) == (col!!1) else False  = moveGroupCards (EOBoard f c r) cols movedLists
                                               | (length col) == 1  = boards
                                               | length (col:cols) == 1  = boards
                                               | otherwise        = moveGroupCards (EOBoard f c r) cols boards
                                                 where
                                                   list = getListSucc col
                                                   combinations = forAllColumnHeads (EOBoard f newC r) newC []
                                                   newBoards = addColBoards list combinations
                                                   movedLists = if (length r) <= (8 - length list) then boards ++ deleteDuplicates newBoards combinations else []
                                                   newCol = filter (`notElem` list) col
                                                   newC = replace col newCol c

callMoveGroupCards :: Board -> [Board]
callMoveGroupCards (EOBoard f c r) = moveGroupCards (EOBoard  f c r) c []



---- 4.4 FORALLCOlUMNHEADS AND FORALLRESERVES
-- Iterates through all heads in c and returns new boards which are a result of finding places for mentioned heads
forAllColumnHeads :: Board -> [[Card]] -> [Board]  -> [Board]
forAllColumnHeads _ [] boards = boards
forAllColumnHeads (EOBoard f c r) (col:cols) boards | (col:cols) == []  = boards
                                                    | (col:cols) == [[]] = boards
                                                    | col == []         = forAllColumnHeads (EOBoard f c r) cols boards
                                                    | if ((length col) > 1) then (sCard (head col)) == (col!!1) else False  = forAllColumnHeads (EOBoard f c r) cols boards
                                                    | length (col:cols) == 1  = newBoards
                                                    | otherwise         = forAllColumnHeads (EOBoard f c r) cols newBoards
                                                    where 
                                                      -- calls moves on board without that card
                                                      newBoards =  boards ++ callMoves (EOBoard f (replace col newCol c) r) (head col)
                                                      newCol = delete (head col) col
-- Iterates through all heads in r and returns new boards
forAllReserves :: Board -> [Card] -> [Board] -> [Board]
forAllReserves (EOBoard f c r) [] boards = boards
forAllReserves (EOBoard f c r) (card:cards) boards = forAllReserves (EOBoard f c r) cards newBoards
                                                        where 
                                                          newBoards =  boards ++ callMoves (EOBoard f c (delete card r)) card



---- 4.5 CALLMOVES AND CALLITERATION
 -- Generating all the possible boards (with duplicates)
callMoves (EOBoard f c r) card = if length kings > 0 then [toFoundations (head kings)] else []  ++ (successorInC [] (EOBoard newF newC newR) newC card) 
                                    ++ freeSpaceInR (EOBoard newF newC newR) card
                                  where
                                    kings = findMoveKing (EOBoard f c r) card
                                    (EOBoard newF newC newR) = toFoundations (EOBoard f c r)


callIteration (EOBoard f c r) = (forAllColumnHeads (EOBoard newF newC newR) newC  []) ++ (forAllReserves (EOBoard newF newC newR) newR [])
                                  where
                                    (EOBoard newF newC newR) = toFoundations (EOBoard f c r)


---- 4.6 REMOVEDUPLICATES AND FINALDUPLICATESREMOVAL
-- remove from possible moves original board
removeDuplicatesInitial _ [] = []
removeDuplicatesInitial startBoard (board:boards) | equals   = removeDuplicatesInitial startBoard boards
                                                  | otherwise             = toFoundations(board) : removeDuplicatesInitial startBoard boards
                                                      where 
                                                        (EOBoard f c r) = startBoard
                                                        (EOBoard newF newC newR) = board
                                                        equals = f == newF && c == newC && contains r newR
-- removes duplicates inside list of boards                                                
removeInnerDuplicates [] = []
removeInnerDuplicates (board:boards) | board `elem` boards  = removeInnerDuplicates boards
                                     | otherwise            = board : removeInnerDuplicates boards

-- checks if two lists are equal despite order
contains [] y = True
contains (x:xs) y = elem x y && contains xs y



---- 4.7 FINDMOVES
findMoves :: Board -> [Board]
findMoves board = removeInnerDuplicates ((removeDuplicatesInitial (toFoundations board) (callMoveGroupCards (toFoundations board))
                     ++ removeDuplicatesInitial (toFoundations board) (callIteration board) ))
              
-- 5 GAMEPLAY
-- Chooses a move from all available (first one in the list)
chooseMove :: Board -> Maybe Board
chooseMove board | result == []  = Nothing
                 | otherwise     = Just (result !! 0)
                    where
                      result = findMoves board

haveWon :: Board -> Bool
haveWon (EOBoard _ [[],[],[],[],[],[],[],[]] []) = True
haveWon board = False 

-- Returns a number of cards in Foundations after applying all chosen moves
playSolitaire :: Board -> Int
playSolitaire board | findMoves board == [] = countFoundationCards f
                    | otherwise = playSolitaire ((findMoves board) !! 0)
                    where
                      (EOBoard f c r) = board

countFoundationCards :: [Card] -> Int
countFoundationCards [] = 0
countFoundationCards (f:fs) = length ([Ace .. (fst f)]) + countFoundationCards fs

-- Returns a pair (number of wins, average number of cards in Foundations) (increments seed by 50)
analyseEO :: Int -> Int -> (Int, Int)
analyseEO seed plays = performPlays plays 0 seed 0 0

-- helpers for analyseEO
performPlays 0 _ _ _ _ = (0,0)
performPlays max current seed wins score | current == max  = (wins, score `div` max)
                                         | otherwise       = performPlays max (current + 1) (seed + 1) (wins + (addWin result)) (score + result)
                                            where
                                                result = playSolitaire (eODeal seed)
addWin :: Int -> Int
addWin 52 = 1
addWin _ = 0

{- Paste the contents of this file, including this comment, into your source file, below all
    of your code. You can change the indentation to align with your own, but other than this,
    ONLY make changes as instructed in the comments.
  -}
-- Constants that YOU must set:
studentName = "Milosz Tabaka"
studentNumber = "200131955"
studentUsername = "aca20mmt"

initialBoardDefined = displayEO {- replace XXX with the name of the constant that you defined
                              in step 3 of part 1 -}
secondBoardDefined = displayS {- replace YYY with the constant defined in step 5 of part 1,
                            or if you have chosen to demonstrate play in a different game
                            of solitaire for part 2, a suitable contstant that will show
                            your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
-}

main :: IO()
main =
  do
    putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

    putStrLn "***The eight-off initial board constant from part 1:"
    print initialBoardDefined

    let board = toFoundations initialBoardDefined
    putStrLn "***The result of calling toFoundations on that board:"
    print board

    {- Move the start comment marker below to the appropriate position.
      If you have completed ALL the tasks for the assignment, you can
      remove the comments from the main function entirely.
      DO NOT try to submit/run non-functional code - you will receive 0 marks
      for ALL your code if you do, even if *some* of your code is correct.
    -}

    

    let boards = findMoves board      -- show that findMoves is working
    putStrLn "***The possible next moves after that:"
    print boards


    let chosen = chooseMove board     -- show that chooseMove is working
    putStrLn "***The chosen move from that set:"
    print chosen


    putStrLn "***Now showing a full game"     -- display a full game
    score <- displayGame initialBoardDefined 0
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


    putStrLn "\n\n\n************\nNow looking at the alternative game:"

    putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
    print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                      -- is not an initial game, but a point from which the game
                                      -- can be won
{- start comment marker - move this if appropriate
    putStrLn "***Now showing a full game for alternative solitaire"
    score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                              -- works correctly)
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

  

 displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
    DO NOT CHANGE THIS CODE other than aligning indentation with your own.
-}
displayGame :: Board -> Int ->IO String
displayGame board n =
  if haveWon board
    then return "A WIN"
    else
      do
        putStr ("Move " ++ show n ++ ": " ++ show board)
        let maybeBoard = chooseMove board
        if isJust maybeBoard then
          do
            let (Just newBoard) = maybeBoard
            displayGame newBoard (n+1)
        else
          do
            let score = show (playSolitaire board)
            return score
