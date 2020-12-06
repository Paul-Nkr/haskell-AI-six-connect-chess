import Data.List
import System.IO
import System.Directory
import Control.Concurrent
import Data.Time.Clock

connectcnt = 6
rowcnt :: Int
colcnt :: Int
rowcnt = 15
colcnt = 15
boardChar = ' '
backgroundchar :: Char
backgroundchar = '.'
whitechar = 'X'
blackchar = 'O'

printPicture pic=putStr(unlines pic)

-- Data 数据类型 type 类型别名
type Position = (Int,Int)
type Board = [[Char]]
data Time = Time Int Int Int deriving(Show)
data Move = Move { t :: Time, p :: [Position] } deriving(Show)
type Blacklist = [Position]
type Whitelist = [Position]

emptyBoard = replicate rowcnt $ replicate colcnt backgroundchar

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take (r-1) m ++
  [take (c-1) (m !! (r-1)) ++ [x] ++ drop c (m !! (r-1))] ++
  drop r m

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

stringToMove :: [Char] -> Move
stringToMove s
  | length vs >= 6 && length vs `mod` 2 == 0 = Move {t = t' ,
    p = [((read $ vs !! (2 * x) :: Int) , (read $ vs !! (2 * x + 1) :: Int))
          | x <- [2..(length vs `div` 2 - 1)]]}
  | otherwise = Move {t = t' , p = []}
    where vs = wordsWhen (\ch -> any (==ch) "/,()\"") s
          t' = Time (read $ vs !! 0 :: Int) (read $ vs !! 1 :: Int) (read $ vs !! 2 :: Int)

toDouble :: Time -> Int
toDouble (Time h m s) = ((h) * 3600) + ((m) * 60) + s

timeElapse :: Time -> Time -> Int
timeElapse from to | t1 <= t2 = t2 - t1 | otherwise = t1 - t2 --t2 - t1 + 86400
  where t1 = toDouble from;t2 = toDouble to

playerName ch | ch == blackchar = "Black" | ch == whitechar = "White" | otherwise = "Unknown"

endMessage :: Char -> [Char] -> [Char] -> [Char]
endMessage ch m1 m2 = (playerName ch) ++ " " ++ m1 ++ " (" ++ m2 ++ ")"

placePieces :: [Position] -> Char -> Board -> Board
placePieces ps ch b = foldr (\pos b -> updateMatrix b ch pos) b ps

printBoard :: Board -> IO()
printBoard b = do
  mapM_ putStrLn $ concatMap (\s -> [s," "]) [x ++ (show id) | (id , x) <- zip [1..] $ map (concatMap (:"  ")) b]
  putStrLn $ concatMap (\x -> show x ++ replicate (3 - length (show x)) ' ') [1..rowcnt]

checkNumberPiece :: [Position] -> Int -> Bool
checkNumberPiece p round
  | round == 1 && length p /= 1 = False
  | round /= 1 && length p /= 2 = False
  | otherwise = True

checkNumberPiece' ::[Position]->Int->Bool
checkNumberPiece' p round
 --  | round == 57 && length p /=1 =False
   | round ==0 && length p /=1 =False
   |  round /= 0 && length p /=2 =False
   | otherwise =True 

checkCorrectPlace :: [Position] -> Board -> Bool
checkCorrectPlace [] _ = True
checkCorrectPlace ((x,y):ps) b
  | b !! x !! y == backgroundchar = checkCorrectPlace ps $ placePieces [(x,y)] whitechar b
  | otherwise = False

checkBoardfull :: Board -> Bool
checkBoardfull b  = ((length (whitelist b))+(length (blacklist b))) ==225

checkConnect :: Char -> Board -> Bool
checkConnect ch b = or $ map
  (\(dx,dy) -> or $ map (== replicate connectcnt ch)
  [[if checkInRange [(x+z*dx,y+z*dy)] then b !! (x+z*dx) !! (y+z*dy) else backgroundchar | z<-[0..connectcnt-1]]
  | x<-[0..rowcnt-1], y<-[0..colcnt-1]]) direction
  where direction = [(0,1),(1,0),(1,-1),(1,1)]

runGame :: [Move] -> [Move] -> Char -> Char -> Int -> Time -> Board -> IO()
runGame [] _ p1char _ _ _ _ = putStrLn $ "\n\n" ++ (endMessage p1char "lose" "give up to action")
runGame (x:xs) ys p1char p2char round time board = do
  putStrLn $ "\n\nRound " ++ (show round) ++ " | " ++ (playerName p1char) ++ "' turn"
  putStrLn $ (playerName whitechar) ++ " : " ++ [whitechar]
  putStrLn $ (playerName blackchar) ++ " : " ++ [blackchar]
  let pos = p x
  if round /= 1 && timeElapse time (t x) > 5
  then putStrLn $ endMessage p1char "lose" "time exceed 5s"
  else if checkNumberPiece pos round == False
  then putStrLn $ endMessage p1char "lose" "wrong number placed pieces"
  else if checkInRange pos == False
  then putStrLn $ endMessage p1char "lose" "place pieces out of range"
  else if checkCorrectPlace pos board == False
  then putStrLn $ endMessage p1char "lose" "place on the previous pieces"
  else do
    let newboard = placePieces pos p1char board
    mapM_ (\(x,y) -> putStrLn $ (playerName p1char) ++ " move : " ++ (show (x+1,y+1))) pos
    putStrLn " "
    printBoard newboard
    if checkConnect p1char newboard
    then putStrLn $ endMessage p1char "win" "connect6!"
    else runGame ys xs p2char p1char (round+1) (t x) newboard

arrivalChar :: Char -> Char
arrivalChar ch
  | ch == blackchar = whitechar
  | ch == whitechar = blackchar
  | otherwise = backgroundchar

checkInRange :: [Position] -> Bool
checkInRange p = and $ map (\(x,y) -> 0 <= x && x < rowcnt && 0 <= y && y < colcnt) p

--判断是否存在一个可能6连的区域（即该区域内无对方落子）
rangehasnoarrival ::Position -> Char -> Board -> Bool
rangehasnoarrival (x,y) ch b = 
    or [rangehasnoarrival_right (x,y) ch b,rangehasnoarrival_up (x,y) ch b,rangehasnoarrival_rightup (x,y) ch b,rangehasnoarrival_rightdown (x,y) ch b ]

rangehasnoarrival_right ::Position -> Char -> Board -> Bool
rangehasnoarrival_right (x,y) ch b =
  if checkInRange [(x,y+5)]
  then and $ map ( /= arrivalChar ch )  [ b !! x !! (y+z)  | z<-[0..5]]
  else False

rangehasnoarrival_up ::Position -> Char -> Board -> Bool
rangehasnoarrival_up (x,y) ch b =
  if checkInRange [(x-5,y)]
  then and $ map ( /= arrivalChar ch )  [ b !! (x-z) !! y  | z<-[0..5]]
  else False

rangehasnoarrival_rightup ::Position -> Char -> Board -> Bool
rangehasnoarrival_rightup (x,y) ch b =
  if checkInRange [(x-5,y+5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x-z) !! (y+z)  | z<-[0..5]]
  else False

rangehasnoarrival_rightdown ::Position -> Char -> Board -> Bool
rangehasnoarrival_rightdown (x,y) ch b =
  if checkInRange [(x+5,y+5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x+z) !! (y+z)  | z<-[0..5]]
  else False

--备用暂时不需要
rangehasnoarrival_left ::Position -> Char -> Board -> Bool
rangehasnoarrival_left (x,y) ch b =
  if checkInRange [(x,y-5)]
  then and $ map ( /= arrivalChar ch )  [ b !! x !! (y-z) | z<-[0..5]]
  else False

rangehasnoarrival_down ::Position -> Char -> Board -> Bool
rangehasnoarrival_down (x,y) ch b =
  if checkInRange [(x+5,y)]
  then and $ map ( /= arrivalChar ch )  [ b !! (x+z) !! y  | z<-[0..5]]
  else False

rangehasnoarrival_leftup ::Position -> Char -> Board -> Bool
rangehasnoarrival_leftup (x,y) ch b =
  if checkInRange [(x-5,y-5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x-z) !! (y-z)  | z<-[0..5]]
  else False

rangehasnoarrival_leftdown ::Position -> Char -> Board -> Bool
rangehasnoarrival_leftdown (x,y) ch b =
  if checkInRange [(x+5,y-5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x+z) !! (y-z)  | z<-[0..5]]
  else False

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs

--rangehasnoarrival (x,y) ch b == True 才有资格讨论数量
--numberIntherange输出4或者5均代表可以立即赢下（ch=自己）或者需要立刻堵截（ch=对手）
numberIntherangemax :: Position -> Char -> Board -> Int
numberIntherangemax (x,y) ch b = maximum' [numberIntherange_right (x,y) ch b,numberIntherange_up (x,y) ch b,numberIntherange_rightdown (x,y) ch b,numberIntherange_rightup (x,y) ch b, numberIntherange_left (x,y) ch b,numberIntherange_down (x,y) ch b,numberIntherange_leftdown (x,y) ch b,numberIntherange_leftup(x,y) ch b]


numberIntherangesum :: Position -> Char -> Board -> Int
numberIntherangesum (x,y) ch b = sum [numberIntherange_right (x,y) ch b,numberIntherange_up (x,y) ch b,numberIntherange_rightdown (x,y) ch b,numberIntherange_rightup (x,y) ch b, numberIntherange_left (x,y) ch b,numberIntherange_down (x,y) ch b,numberIntherange_leftdown (x,y) ch b,numberIntherange_leftup(x,y) ch b]


numberIntherange_right :: Position -> Char -> Board -> Int
numberIntherange_right (x,y) ch b =
  if rangehasnoarrival_right (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! x !! (y+z)  | z<-[0..5]]
  else 0

numberIntherange_up :: Position -> Char -> Board -> Int
numberIntherange_up (x,y) ch b =
  if rangehasnoarrival_up (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [ b !! (x-z) !! y | z<-[0..5]]
  else 0

numberIntherange_rightdown :: Position -> Char -> Board -> Int
numberIntherange_rightdown (x,y) ch b =
  if rangehasnoarrival_rightdown (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x+z) !! (y+z) | z<-[0..5]]
  else 0

numberIntherange_rightup :: Position -> Char -> Board -> Int
numberIntherange_rightup (x,y) ch b =
  if rangehasnoarrival_rightup (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x-z) !! (y+z) | z<-[0..5]]
  else 0


numberIntherange_left :: Position -> Char -> Board -> Int
numberIntherange_left  (x,y) ch b =
  if rangehasnoarrival_left (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! x !! (y-z)  | z<-[0..5]]
  else 0

numberIntherange_down :: Position -> Char -> Board -> Int
numberIntherange_down (x,y) ch b =
  if rangehasnoarrival_down (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [ b !! (x+z) !! y | z<-[0..5]]
  else 0

numberIntherange_leftdown :: Position -> Char -> Board -> Int
numberIntherange_leftdown (x,y) ch b =
  if rangehasnoarrival_leftdown (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x+z) !! (y-z) | z<-[0..5]]
  else 0

numberIntherange_leftup :: Position -> Char -> Board -> Int
numberIntherange_leftup (x,y) ch b =
  if rangehasnoarrival_leftup (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x-z) !! (y-z) | z<-[0..5]]
  else 0

couldwin ::  Char -> Board -> Bool
couldwin ch b =
    or $ map (\(x,y)-> if ((numberIntherangemax (x,y) ch b) >= 4) then True else False ) $ filter (\(x,y) -> b!!x!!y /= (arrivalChar ch) )[(x,y) |  x <- [0..colcnt-1], y <- [0..rowcnt-1]]



--在011110时会同时产出两个坐标
getthepostioncould :: Char -> Board -> [Position]
getthepostioncould ch b =
  filter (\(x,y)->(numberIntherangemax (x,y) ch b) >= 4) $ filter (\(x,y) -> b !! x !! y /= (arrivalChar ch)) [(x,y) |  x <- [0..colcnt-1], y <- [0..rowcnt-1]]



--要设立多层计算 valueallboard
getnormalpos :: Char -> Board -> [Position]
getnormalpos ch b 
    | ch == whitechar =  (quickSortvalue (rmDup(aroundhaspieces ((whitelist b)++(blacklist b))  b )) whitechar b) 
    | ch == blackchar =  (quickSortvalue (rmDup(aroundhaspieces ((whitelist b)++(blacklist b))  b )) blackchar b) 
    | otherwise = []

--value只负责输和一般落子，赢的情况由getblankposition负责
--value的优先级 
--涉及输的优先级最高| (numberIntherangemax (x,y) ch b >= 4) ==True = 25
--一般步骤应该以落子后的棋盘的总分值为依据
--value接收的是未修正的pos
value :: Position -> Char -> Board -> Int
value (x,y) ch b 
--出现这种情况 couldwin必置位True 轮不着getnormal进行value    | (numberIntherangemax (x-1,y-1) ch b >= 4) ==True = 99999
    | (numberIntherangemax (x,y) ch b >= 3) ==True = 99999
    | (numberIntherangemax (x,y) ch b >= 2) ==True = 88888
    | otherwise = valueallboard (x,y) ch b 

valueallboard :: Position ->Char -> Board -> Int
valueallboard (x,y) ch b 
  |ch == whitechar = sum ( map (\(x,y) -> numberIntherangesum (x,y) ch b ) ((rmDup(aroundhaspieces (removeItem (x,y) (whitelist b)) b))))
  |ch == blackchar = sum ( map (\(x,y) -> numberIntherangesum (x,y) ch b ) ((rmDup(aroundhaspieces (removeItem (x,y) (blacklist b)) b))))
  |otherwise = 0




--value不能使用numberIntherangesum 



removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


quickSortvalue ::  [Position] -> Char -> Board -> [Position]
quickSortvalue [] ch board = []
quickSortvalue (x:xs) ch board = quickSortvalue mini ch board ++ [x] ++ quickSortvalue maxi ch board
        where mini = filter (\p -> value p ch board < value x ch board) xs
              maxi = filter (\p -> value p ch board >= value x ch board) xs



--接受的和产出的都是未修正的pos
--该函数内部存在重复需要引入一个去重复的函数rmDup
aroundhaspieces :: [Position] -> Board -> [Position]
aroundhaspieces []  b = []--此时的xy是未经过坐标修正的需要修正
aroundhaspieces [(x,y)]  b
  | (0 < x) && (x < 14) && (0 < y) && (y < 14) = filter (\(x,y) -> b!!x!!y == backgroundchar ) [(x+1,y),(x+1,y-1),(x+1,y+1),(x,y+1),(x,y-1),(x-1,y),(x-1,y+1),(x-1,y-1)]
  | (x == 0) && (y == 0) = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x+1,y),(x+1,y+1),(x,y+1)]
  | (x == 14) && (y == 14) = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x-1,y),(x-1,y-1),(x,y-1)]
  | (x == 0) && (y == 14) = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x+1,y),(x+1,y-1),(x,y-1)]
  | (x == 14) && (y == 0) = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x-1,y),(x-1,y+1),(x,y+1)]
  | x == 0 = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x+1,y),(x+1,y-1),(x+1,y+1),(x,y+1),(x,y-1)]
  | x == 14 = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x,y+1),(x,y-1),(x-1,y),(x-1,y+1),(x-1,y-1)]
  | y == 0 = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y),(x-1,y+1)]
  | y == 14 = filter ( \(x,y) -> b!!x!!y == backgroundchar ) [(x+1,y),(x+1,y-1),(x,y-1),(x-1,y),(x-1,y-1)]
  | otherwise = []
aroundhaspieces (x:xs)  b = aroundhaspieces [x]  b ++ aroundhaspieces xs  b



rmDup :: [Position] -> [Position]
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (\y -> not(x == y)) xs)


--两个list产出的是未修正的坐标值
blacklist :: Board -> [Position]
blacklist b = filter (\(x,y) -> b !! x !! y == blackchar) [(x,y) | x <- [0..colcnt-1], y <- [0..rowcnt-1]]

whitelist :: Board -> [Position]
whitelist b = filter (\(x,y) -> b !! x !! y == whitechar) [(x,y) | x <- [0..colcnt-1], y <- [0..rowcnt-1]]

blanklist :: Board -> [Position]
blanklist b = filter (\(x,y) -> b !! x !! y == backgroundchar) [(x,y) | x <- [0..colcnt-1], y <- [0..rowcnt-1]]

valuelist ::  Char -> Board -> [Position]
valuelist  ch b
  | ch == whitechar = quickSortvalue (aroundhaspieces (whitelist b) b) whitechar b
  | ch == blackchar = quickSortvalue (aroundhaspieces (blacklist b) b) blackchar b
  | otherwise = []


--getnormalpos已修正
{-getpos :: Char -> Board -> [Position]
getpos ch b 
  | couldwin ch b == True = revisePos (getblankpostion (getdirection (getthepostioncould ch b) ch b)  (getthepostioncould ch b) ch b )
  | couldwin  (arrivalChar ch) b == True = getnormalpos (arrivalChar ch ) b
  | (b == emptyBoard) && (ch == blackchar) == True = [(8,8)]
  | whitelist b == [] = [(9,8),(9,9)]
  | otherwise = getnormalpos ch b
-}

getonesteptostop :: Int -> [Position] -> Char -> Board -> [Position]
getonesteptostop v [p] ch b 
    | v == 1 = take 1 (getthepos_right p ch b )
    | v == 2 = take 1 (getthepos_up p ch b )
    | v == 3 = take 1 (getthepos_rightdown p ch b )
    | v == 4 = take 1 (getthepos_rightup p ch b )
    | v == 5 = take 1 (getthepos_left p ch b )
    | v == 6 = take 1 (getthepos_down p ch b )
    | v == 7 = take 1 (getthepos_leftup p ch b )
    | v == 8 = take 1 (getthepos_leftdown p ch b )
    | otherwise = [(999,999)]--列表溢出报错
    
    
--黑白子第一轮指定
-- 对方落子在getposone中判定couldwin == Ture经过第一轮修正还可能 == True 此时getblank的基点方向棋盘都要更新
--己方落子在getposone中判定couldwin == false 此时第二轮不可能couldwin了但是检测新的棋盘可能为True 所以要检测旧的棋盘 最多四连不可能五连  --这里有待思考 似乎五连肯定比四连强？再裸一个似乎也没什么不好的
--己方落子在getposone中判定couldwin == True 在第二轮必为Truw getblank中基点不变 方向不变 但是棋盘更新
getpos :: Char -> Board -> [Position]
getpos ch b
  | (b == emptyBoard) && (ch == blackchar) == True = [(8,8)]
  | (whitelist b == []) && (ch == whitechar) = [(9,8),(8,9)]
  | otherwise = (getposone ch b) ++ (getpostwo ch b)

getposone :: Char -> Board -> [Position]
getposone ch b 
  | couldwin ch b == True = revisePos (take 1(getblankpostion (getdirection (take 1 (getthepostioncould ch b)) ch b) (take 1 (getthepostioncould ch b)) ch b ))
  | couldwin  (arrivalChar ch) b == True = revisePos (take 1(getblankpostion (getdirection (take 1 (getthepostioncould (arrivalChar ch) b)) (arrivalChar ch) b)  (take 1 (getthepostioncould (arrivalChar ch) b)) (arrivalChar ch) b ))
  | otherwise = take 1 (revisePos( (getnormalpos ch b)))

--此时的b应该是经过更新的b
getpostwo :: Char -> Board -> [Position]
getpostwo ch b 
  | couldwin ch b == True = revisePos (take 1(getblankpostion (getdirection (take 1 (getthepostioncould ch b)) ch b) (take 1 (getthepostioncould ch b)) ch z ))
  | couldwin  (arrivalChar ch) z == True = revisePos (take 1(getblankpostion (getdirection (take 1 (getthepostioncould (arrivalChar ch) z)) (arrivalChar ch) z)  (take 1 (getthepostioncould (arrivalChar ch) z)) (arrivalChar ch) z ))
  | otherwise = take 1  (revisePos ((getnormalpos ch z)))
  where z = placePieces (getposone ch b) ch b


revisePos :: [Position] -> [Position]
revisePos [] = []
revisePos [(a,b)] =  map (\(x,y) -> ((x+1),(y+1))) [(a,b)]
revisePos [(a,b),(c,d)] = map (\(x,y) -> ((x+1),(y+1))) [(a,b),(c,d)]
revisePos (x:xs) = revisePos [x] ++ revisePos xs

-- 遍历棋盘 找空白落子罢了 给活5获胜凑人头的
getrandpos :: Char -> Board -> [Position]
getrandpos ch b = filter (\(x,y) -> b!!x!!y == backgroundchar ) [(x,y) |  x<- [0..colcnt-1], y <- [0..rowcnt-1]]

--getthepostioncould的结果为[Position] 
getdirection :: [Position] -> Char -> Board -> Int
getdirection [p] ch b 
    | ((numberIntherange_right p ch b) >= 4) == True = 1
    | ((numberIntherange_up p ch b) >= 4) == True =2
    | ((numberIntherange_rightdown p ch b) >= 4) == True =3
    | ((numberIntherange_rightup p ch b) >= 4) == True =4
    | ((numberIntherange_left p ch b) >= 4) == True =5
    | ((numberIntherange_down p ch b) >= 4) == True =6
    | ((numberIntherange_leftup p ch b) >= 4) == True =7
    | ((numberIntherange_leftdown p ch b) >= 4) == True =8
    | otherwise = 0

--getthepos类函数在五子时只能产生一个空格 需要补充
--按照符合couldwin的初始位置找到的落子位置
--Int 类型由getdirection产生来确定方向
--Position由getthepostioncould产出并用head提取头元素获得position
getblankpostion :: Int -> [Position] -> Char -> Board -> [Position]
getblankpostion v [p] ch b 
    | v == 1 = take 2 (getthepos_right p ch b ++ getrandpos ch b)
    | v == 2 = take 2 (getthepos_up p ch b ++ getrandpos ch b)
    | v == 3 = take 2 (getthepos_rightdown p ch b ++ getrandpos ch b)
    | v == 4 = take 2 (getthepos_rightup p ch b ++ getrandpos ch b)
    | v == 5 = take 2 (getthepos_left p ch b ++ getrandpos ch b)
    | v == 6 = take 2 (getthepos_down p ch b ++ getrandpos ch b)
    | v == 7 = take 2 (getthepos_leftup p ch b ++ getrandpos ch b)
    | v == 8 = take 2 (getthepos_leftdown p ch b ++ getrandpos ch b)
    | otherwise = [(16,16)]  --(16,16)表示溢出-}

getthepos_right :: Position -> Char -> Board -> [Position]
getthepos_right (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x,y+z) |z<-[0..5]]

getthepos_up :: Position -> Char -> Board -> [Position]
getthepos_up (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x-z,y) |z<-[0..5]]

getthepos_rightup :: Position -> Char -> Board -> [Position]
getthepos_rightup (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x-z,y+z) |z<-[0..5]]

getthepos_rightdown :: Position -> Char -> Board -> [Position]
getthepos_rightdown (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x+z,y+z)|z<-[0..5]]

getthepos_left :: Position -> Char -> Board -> [Position]
getthepos_left (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x,y-z)|z<-[0..5]]

getthepos_down :: Position -> Char -> Board -> [Position]
getthepos_down (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x+z,y)|z<-[0..5]]

getthepos_leftup :: Position -> Char -> Board -> [Position]
getthepos_leftup (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x-z,y-z)|z<-[0..5]]

getthepos_leftdown :: Position -> Char -> Board -> [Position]
getthepos_leftdown (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x+z,y-z)|z<-[0..5]]

posToMove :: [Position] -> Move
posToMove [(x,y)] = Move {t = Time 0 0 4 ,p = [(x,y)]}
posToMove [(x,y),(a,b)] = Move {t = Time 0 0 4 ,p= [(x,y),(a,b)]}
--  where t' = Time 0 0 5

placePie :: [Move] -> Char->Board -> Board
placePie (x:xs) ch b =placePie xs ch (placePieces (p x) ch b)
placePie [] ch b = b
--placePieces' m ch b=foldr (\ms b -> updateMatrix b ch (p ms)) b m

placeBoard :: [Move] -> [Move] ->Char->Char->Board
placeBoard [] [] _ _ =emptyBoard
placeBoard (x:xs) [] ch1 ch2 = placePie [] ch2 ( placePie (x:xs) ch1 emptyBoard )
placeBoard [] (y:ys) ch1 ch2 = placePie (y:ys) ch2 ( placePie [] ch1 emptyBoard  )
placeBoard (x:xs) (y:ys) ch1 ch2 = placePie (y:ys) ch2 b
            where b = placePie (x:xs) ch1 emptyBoard
--Move {t = Time 0 0 3, p = [(5,5),(8,7)]}


moveLiToPosLi :: [Move] -> [Position]
moveLiToPosLi (x:xs) = (p x) ++ moveLiToPosLi xs
moveLiToPosLi [] = []

checkCorrectPlace' :: [Position]->Board->Bool
checkCorrectPlace' [] _ =True 
checkCorrectPlace' ((x,y):ps) b 
          | b !! (x-1) !! (y-1) ==backgroundchar = checkCorrectPlace' ps (placePie [ posToMove [(x,y)] ]  blackchar b)
          | otherwise = False

check :: [Move] ->[Move]->Bool
check [] _ =True
check  _ [] =  True
check (x:xs) (y:ys)  = checkCorrectPlace' (moveLiToPosLi (y:ys)) b
         where b = placePie (x:xs) blackchar emptyBoard

intToString :: Int->String
intToString  a = show ((a+3600*8) `div` 3600) ++ "/" ++ show (((a+3600*8) `mod` 3600) `div` 60) ++ "/" ++ show (((a+3600*8) `mod` 3600) `mod` 60) ++ ",\"(1,1)\"" 

mvToString :: Move->[Move]->Int -> String
mvToString m b c
       | length vs == 9  =if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >=0 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                          then "0"++show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""
                          else if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >=0 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `mod` 60)>9
                          then "0"++show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""
                          else if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >9 && ((c `mod` 3600) `mod` 60)>9
                          then "0"++show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""
                          else if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >9 && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                          then "0"++show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""
                          else if (c `div` 3600)>9 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `div` 60)>=0 && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                          then show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""
                          else if (c `div` 3600)>9 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `div` 60)>=0 && ((c `mod` 3600) `mod` 60)>9
                          then show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""
                          else if (c `div` 3600)>9 && ((c `mod` 3600) `div` 60) >9  && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                          then show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""                            
                          else show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""                                                    
       | length vs == 11  =if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >=0 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                           then "0"++show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""
                           else if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >=0 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `mod` 60)>9
                           then "0"++show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""
                           else if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >9 && ((c `mod`3600) `mod` 60)>9
                           then "0"++show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""                            
                           else if (c `div` 3600)>=0 && (c `div` 3600)<=9 && ((c `mod` 3600) `div` 60) >9 && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                          then "0"++show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""
                          else if (c `div` 3600)>9 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `div` 60)>=0 && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                          then show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""
                          else if (c `div` 3600)>9 && ((c `mod` 3600) `div` 60) <=9 && ((c `mod` 3600) `div` 60)>=0 && ((c `mod` 3600) `mod` 60)>9
                          then show (c `div` 3600) ++"/"++"0"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""
                          else if (c `div` 3600)>9 && ((c `mod` 3600) `div` 60) >9  && ((c `mod` 3600) `mod` 60)>=0 && ((c `mod` 3600) `mod` 60)<=9
                          then show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++"0"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\""  ++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""                        
                           else show (c `div` 3600) ++"/"++show ((c `mod` 3600) `div` 60)++"/"++show ((c `mod` 3600) `mod` 60)++"," ++ show as ++ "," ++"\"("++vs !! 7 ++ "," ++ vs !! 8 ++")\"" ++ "," ++ "\"(" ++ vs !! 9 ++ "," ++ vs !! 10 ++ ")\""
       | otherwise = error "error move"
         where vs = wordsWhen (\ch -> any (==ch) "Move{}t=Time,p=[]()") (show m)
               as = (length b) + 1

--main = IO()
main1 = do
   --time' = do
  currTime <- getCurrentTime
  let timed = floor $ utctDayTime currTime :: Int
  --  putStrLn $ show (timed `div` 2)
--  putStrLn $ "            "
--  putStrLn $ "black turns: "
--  threadDelay 1000000
  black <- readFile "black1.csv"
--  black3 <- readFile "black.csv"
  let blackmoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black
--      blackmoves3 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black3   
--  threadDelay 2000000
  white' <- readFile "white.csv" 
  let whitemoves' = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white'   
  if length whitemoves' == ((length blackmoves) - 1)
  then main1  --threadDelay 1000000
  else if length whitemoves' /= (length blackmoves)
  then appendFile "black.log" ("stop wrong,may be white run first"++"\n")        -- putStrLn $ "stop wrong "
  else do
  
 -- appendFile "black2.csv" (intToString timed ++ "\n")
 -- whiteTime<-readFile "black2.csv" 
  
  white <- readFile "white.csv"
  --let whiteTime' = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') whiteTime
  let whitemoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white  
      newBoard = placeBoard blackmoves whitemoves blackchar whitechar
      
  appendFile "checkWhite.csv" ( mvToString (last ([Move{t=Time 0 0 0,p=[(1,1)]}]++whitemoves)) whitemoves (timed+(3600*8)) ++ "\n")
  checkWhite'<-readFile "checkWhite.csv"
  let checkW = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') checkWhite'
  
  if checkConnect whitechar newBoard==True 
  then appendFile "black.log" ("white connect 6,win!"++"\n")        -- putStrLn $ "white connect 6,win!"
  else if checkBoardfull newBoard == True
  then appendFile "black.log" ("no one win,it's a tie!"++"\n")                --putStrLn $ "no one win,it's a tie!"
  else if check blackmoves whitemoves' == False
  then appendFile "black.log" ("white put in the place where have the chess"++"\n")-- putStrLn $ "white put in the place where have the chess"
  else if length blackmoves == 0 && length  whitemoves' /=0
  then appendFile "black.log" ("error,white play first"++"\n")   --putStrLn $ "error,white play first"
  else if timeElapse (t (last([Move{t = Time 0 0 0,p = [(0,0)]}]++blackmoves))) (t (last([Move{t = Time 0 0 0,p =[(0,0)]}]++whitemoves))) >10
  then  appendFile "black.log" ("white out of time"++"\n")          --putStrLn $ "white out of time"
  
  --else if timeElapse (t (last( whiteTime'++blackmoves))) (t (last(whiteTime'))) >10
 -- then  appendFile "black.log" ("may be white falsified(篡改) time data"++"\n")      
  
  else if timeElapse (t (last([Move{t = Time 0 0 0,p =[(0,0)]}]++blackmoves))) (t (last([Move{t = Time 0 0 0,p =[(0,0)]}]++whitemoves'))) <0
  then  appendFile "black.log" ("white time is error,maybe white put in more faster"++"\n")                                            --putStrLn $ "white time is error,maybe white put in more faster "
  else if checkNumberPiece' (p (last ([Move {t = Time 0 0 4, p = [(0,1)]}] ++ whitemoves')))  (length whitemoves')  == False
  then  appendFile "black.log" ("white input error,less or more"++"\n")      -- putStrLn $ "white input error,less or more"
  else do
  let  mov1 = posToMove(getpos blackchar newBoard)
--  putStrLn $ show (p mov1)
  (tempName,tempHandle)<- openTempFile "." "temp"
  hPutStr tempHandle black
  hClose tempHandle
  removeFile "black1.csv"
  renameFile tempName "black1.csv"
  appendFile "black1.csv"  (mvToString mov1 blackmoves (timed+(3600*8)) ++ "\n")
  blackPlayer1<-readFile "black1.csv"
--  appendFile "black.csv"  (show (last blackmoves) ++ "\n")
  let blackmoves1 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') blackPlayer1
      newBoard1 = placeBoard blackmoves1 whitemoves blackchar whitechar
--  putStr $ show blackmoves1 ++ "\n"
--  putStr $ show blackPlayer1 ++ "\n"
  appendFile "black.csv"  (mvToString (last blackmoves1) blackmoves (timed+(3600*8)) ++ "\n")
--  putStrLn $ show (last blackmoves1)
  putStrLn $ "            "
  putStrLn $ "black : O"
  putStrLn $ "white : X"
  putStrLn $ "black turns: " ++ show (length blackmoves1)
  putStrLn $ show (p mov1)
  printBoard newBoard1
  bb<-readFile "black.csv"
  if bb /= blackPlayer1
  then appendFile "black.log" ("maybe white falsified(篡改) black's file" ++ "\n")
  else if tail (moveLiToPosLi checkW) /= moveLiToPosLi whitemoves 
  then appendFile "black.log" ("maybe white falsified(篡改) itself's file" ++ "\n")
  else if checkConnect blackchar newBoard1==True
  then appendFile "black.log" ("black connect 6,win!"++"\n")--putStrLn $ "black connect 6,win!"
  else if checkBoardfull newBoard1 == True
  then appendFile "black.log" ("no one win,it's a tie!"++"\n")    --putStrLn $ "no one win,it's a tie!"
  else if checkNumberPiece (p (last blackmoves1)) (length blackmoves1) ==False
  then appendFile "black.log" ("black error input,more or less"++"\n") --putStrLn $ "black error input,more or less"
  else main1



--main2 = IO()
main2 = do
    --time' = do
  currTime <- getCurrentTime
  let timed = floor $ utctDayTime currTime :: Int
 --  putStrLn $ show (timed `div` 2)
--  putStrLn $ "            "
--  putStrLn $ "white turns: " 
  white <- readFile "white1.csv"
--  white3 <- readFile "white.csv"
  let whitemoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white
--      whitemoves3 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white3
--  threadDelay 2000000
  black' <- readFile "black.csv" 
  let blackmoves' = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black' 
--  putStrLn $ show (length whitemoves)
--  putStrLn $ show (length blackmoves')  
  if length whitemoves == (length blackmoves')  
  then main2-- threadDelay 4000000
  else if length whitemoves /= ( (length blackmoves') -1)
  then  appendFile "white.log" ("stop wrong,maybe black input more chess than it can"++"\n")     --putStrLn $ "stop wrong "
  else do
  
  --appendFile "white2.csv" (intToString timed ++ "\n")
  --blackTime<-readFile "white2.csv"  
  
  black <- readFile "black.csv"
  --let blackTime' = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') blackTime
  let blackmoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black 
      newBoard = placeBoard blackmoves whitemoves blackchar whitechar
          
  appendFile "checkBlack.csv" ( mvToString (last blackmoves) blackmoves (timed+(3600*8)) ++ "\n")
  checkBlack'<-readFile "checkBlack.csv"
  let checkB = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') checkBlack'
      
  if checkConnect blackchar newBoard==True
  then appendFile "white.log" ("black connect 6,win!"++"\n")  --putStrLn $ "black connect 6,win!"
  else if checkBoardfull newBoard == True
  then appendFile "white.log" ("no one win,it's a tie!"++"\n")--putStrLn $ "no one win,it's a tie!"
  else if check whitemoves blackmoves' == False
  then appendFile "white.log" ("black put in the place where have the chess"++"\n")--putStrLn $ "black put in the place where have the chess"
  else if timeElapse (t (last (blackmoves'++whitemoves))) (t (last (blackmoves))) >10
  then appendFile "white.log" ("black out of time"++"\n")--putStrLn $ "black out of time"
  
 -- else if timeElapse (t (last( blackTime'++whitemoves))) (t (last(blackTime'))) >10
 -- then  appendFile "white.log" ("may be black falsified(篡改) time data"++"\n") 
  
  else if timeElapse (t (last (blackmoves'++whitemoves))) (t (last (blackmoves'))) <0
  then appendFile "white.log" ("black time is error,maybe black put in more faster"++"\n")--putStrLn $ "black time is error,maybe black put in more faster "
  else if checkNumberPiece (p (last blackmoves')) (length blackmoves') ==False
  then appendFile "white.log" ("black error,input more or less"++"\n")--putStrLn $ "black error,input more or less" 
  else do
  let mov1 = posToMove(getpos whitechar newBoard)
  (tempName,tempHandle)<- openTempFile "." "temp"
  hPutStr tempHandle white
  hClose tempHandle
  removeFile "white1.csv"
  renameFile tempName "white1.csv"
  appendFile "white1.csv"  (mvToString mov1  whitemoves (timed+(3600*8)) ++ "\n")
  whitePlayer1<-readFile "white1.csv"
  let whitemoves1 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') whitePlayer1
      newBoard1 = placeBoard blackmoves whitemoves1 blackchar whitechar
--  putStr $ show blackmoves1 ++ "\n"
--  putStr $ show blackPlayer1 ++ "\n"
  appendFile "white.csv"  (mvToString (last whitemoves1) whitemoves (timed+(3600*8)) ++ "\n")
  putStrLn $ "            "
  putStrLn $ "black : O"
  putStrLn $ "white : X"
  putStrLn $ "white turns: "++ show (length whitemoves1)
  putStrLn $ show (p mov1)
  printBoard newBoard1
  ww<-readFile "white.csv"
  if ww /= whitePlayer1
  then appendFile "white.log" ("maybe black falsified(篡改) white's file" ++ "\n")
  else if (moveLiToPosLi checkB) /= moveLiToPosLi blackmoves 
  then appendFile "white.log" ("maybe black falsified(篡改) itself's file" ++ "\n")
  else if checkConnect whitechar newBoard1==True
  then appendFile "white.log" ("white connect 6,win!"++"\n")--putStrLn $ "white connect 6,win!"
  else if checkBoardfull newBoard1 == True
  then appendFile "white.log" ("no one win,it's a tie!"++"\n")--putStrLn $ "no one win,it's a tie!"
  else if checkNumberPiece' (p (last whitemoves1)) (length whitemoves1) == False
  then appendFile "white.log" ("white error,input more or less"++"\n") --putStrLn $ "white error,input more or less"
  else main2


main4 = do
--   removeFile "white.csv"
   appendFile "white.csv"  ("")
   appendFile "white1.csv" ("")
   main2
   black2<-readFile "black.csv"
   white2<-readFile "white.csv"
   let whitemoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white2
       blackmoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black2
       newBoard1 = placeBoard blackmoves2 whitemoves2 blackchar whitechar
   if checkConnect whitechar newBoard1==True && length blackmoves2 /= (length whitemoves2)
   then appendFile "white.log" ("black error,white win,but black continue to play"++"\n")--putStrLn $ "black error,white win,but black continue to play"
   else putStrLn $ "Game Over!"


main5 = do
   appendFile "black1.csv" ("")
   appendFile "white.csv" ("")
   appendFile "black.csv" ("")
   main1
   black2<-readFile "black.csv"
   white2<-readFile "white.csv"
   let whitemoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white2
       blackmoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black2
       newBoard1 = placeBoard blackmoves2 whitemoves2 blackchar whitechar
   if checkConnect blackchar newBoard1==True && length blackmoves2 /=( (length whitemoves2) +1)
   then appendFile "black.log" ("white error,black win,but white continue to play"++"\n")--putStrLn $ "white error,black win,but white continue to play"
   else putStrLn $ "Game Over!"


main = do
  putStr $ "if you are black,please input 0"++ "\n"
  putStr $ "if you are white,please input 1"++ "\n"
  putStr $ "    "
  a<-getLine
  let b = read a
  if b==0
  then  main5
  else if b==1
  then  main4
  else putStrLn $ "you put error number in"



--testboard
a = placePieces [(9,8),(9,9),(9,7),(9,6)] whitechar(placePieces [(8,8),(8,7),(8,6)] blackchar emptyBoard)
b = placePieces [(9,8),(8,9),(7,9),(6,8),(6,7),(6,6)] whitechar(placePieces [(8,8),(8,7),(7,8),(5,7),(5,6),(5,5),(5,4)] blackchar emptyBoard)
c = placePieces [(8,8)] blackchar emptyBoard
d = placePieces [(9,8),(8,9),(7,9),(6,8),(6,7),(6,6),(5,8),(5,3)] whitechar(placePieces [(8,8),(8,7),(7,8),(5,7),(5,6),(5,5),(5,4),(4,7),(4,2)] blackchar emptyBoard)
e = placePieces [(9,8),(8,9),(9,10),(8,10),(7,9),(7,10),(7,8),(7,7)] whitechar(placePieces [(8,8),(9,7),(9,9),(8,7),(9,6),(8,6),(9,5),(7,11),(7,6)] blackchar emptyBoard)
f = placePieces [(9,8),(8,9),(9,10),(8,10),(7,9),(7,10),(7,8),(7,7),(6,7),(6,8),(6,6),(7,5),(5,5),(6,4),(5,4),(6,3),(4,8),(4,3)] whitechar(placePieces [(8,8),(9,7),(9,9),(8,7),(9,6),(8,6),(9,5),(7,11),(7,6),(10,11),(5,6),(6,5),(5,7),(4,6),(4,7),(4,5),(4,4),(5,3),(3,4)] blackchar emptyBoard)
g = placePieces [(9,8),(8,9),(9,10),(8,10),(7,9),(7,10),(6,9),(6,10),(11,11),(6,6),(7,6),(7,5),(8,6),(3,11),(4,12),(3,12)] whitechar(placePieces [(8,8),(9,7),(9,9),(8,7),(7,8),(7,7),(6,8),(10,10),(5,10),(6,11),(5,11),(5,9),(4,10),(4,11),(4,9),(5,8),(4,8)] blackchar emptyBoard)
h = placePieces [(9,8),(8,9),(9,10),(8,10),(7,9),(7,10),(6,9),(6,10),(11,11),(6,6),(7,6),(7,5),(8,6),(3,11),(4,12),(3,12),(2,11),(2,12)] whitechar(placePieces [(8,8),(9,7),(9,9),(8,7),(7,8),(7,7),(6,8),(10,10),(5,10),(6,11),(5,11),(5,9),(4,10),(4,11),(3,10),(3,9),(2,10)] blackchar emptyBoard)
i = placePieces [(9,8),(8,9),(9,10),(8,10),(7,9),(7,10),(6,9),(8,11),(4,8),(9,11),(5,7),(4,7),(5,6),(4,6),(5,5),(4,5),(5,4),(3,5),(3,6),(3,4),(4,3),(3,3)] whitechar(placePieces [(8,8),(9,7),(9,9),(8,7),(7,8),(7,7),(6,8),(5,8),(5,9),(4,9),(6,10),(5,10),(3,9),(3,10),(3,8),(4,4),(2,9),(5,3),(2,8),(1,8),(1,9)] blackchar emptyBoard)
i' = placePieces [(9,8),(8,9),(9,10),(8,10),(7,9),(7,10),(6,9),(8,11),(4,8),(9,11),(5,7),(4,7),(5,6),(4,6),(5,5),(4,5),(5,4),(3,5),(3,6),(3,4),(4,3),(3,3)] whitechar(placePieces [(8,8),(9,7),(9,9),(8,7),(7,8),(7,7),(6,8),(5,8),(5,9),(4,9),(6,10),(5,10),(3,9),(3,10),(3,8),(4,4),(2,9),(5,3),(2,8),(1,8),(1,9),(3,2)] blackchar emptyBoard)
j = replicate rowcnt $ replicate colcnt whitechar
k= placePieces [(9,2),(9,9)] whitechar(placePieces [(9,4),(9,5),(9,6),(9,7)] blackchar emptyBoard)
l = placePieces [(9,2),(9,9),(9,7)] whitechar(placePieces [(13,3),(12,4),(11,5),(10,6)] blackchar emptyBoard)

--main :: IO()
--main = do
--  black <- readFile "black.csv"
--  let blackmoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black
--  white <- readFile "white.csv"
--  let whitemoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white
--  runGame blackmoves whitemoves blackchar whitechar 1 (Time 0 0 0.0) emptyBoard

--自我对弈过程
{-fakeboard :: Int -> Board
fakeboard n
  | n == 1 = placePieces (getpos blackchar emptyBoard) blackchar emptyBoard
  | n == 2 = placePieces (getpos whitechar (fakeboard 1)) whitechar (fakeboard 1)
  | n `mod` 2 == 1 = placePieces (getpos blackchar (fakeboard (n-1))) blackchar (fakeboard (n-1))
  | n `mod` 2 == 0 = placePieces (getpos whitechar (fakeboard (n-1))) whitechar (fakeboard (n-1))
-}

