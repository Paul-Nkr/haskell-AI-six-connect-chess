---引入文件库
--Data.list负责排序 Sysetem.IO 负责交互
import Data.List
import System.IO
import System.Directory
import Control.Concurrent
import Data.Time.Clock

--前期准备 连子数量设置为6 棋盘15*15 设置落子符号 背景符号
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
--设置时间格式 Time
--设置落子操作 Move
type Position = (Int,Int)
type Board = [[Char]]
data Time = Time Int Int Int deriving(Show)
data Move = Move { t :: Time, p :: [Position] } deriving(Show)
type Blacklist = [Position]
type Whitelist = [Position]

--初始化空棋盘
emptyBoard = replicate rowcnt $ replicate colcnt backgroundchar

--利用双层list设置为棋盘 类似于二维数字 
--由于index从0开始，需要-1操作
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take (r-1) m ++
  [take (c-1) (m !! (r-1)) ++ [x] ++ drop c (m !! (r-1))] ++
  drop r m

--stringToMove的前期准备
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

--读取对方csv时 将string [char]转化为move便于落子
stringToMove :: [Char] -> Move
stringToMove s
  | length vs >= 6 && length vs `mod` 2 == 0 = Move {t = t' ,
    p = [((read $ vs !! (2 * x) :: Int) , (read $ vs !! (2 * x + 1) :: Int))
          | x <- [2..(length vs `div` 2 - 1)]]}
  | otherwise = Move {t = t' , p = []}
    where vs = wordsWhen (\ch -> any (==ch) "/,()\"") s
          t' = Time (read $ vs !! 0 :: Int) (read $ vs !! 1 :: Int) (read $ vs !! 2 :: Int)

--HHMMSS转化为s 便于计算时间差（不然涉及小时、分钟的跳变）
toDouble :: Time -> Int
toDouble (Time h m s) = ((h) * 3600) + ((m) * 60) + s

--本来考虑了跨日存在86400的差距 后来删去了
timeElapse :: Time -> Time -> Int
timeElapse from to | t1 <= t2 = t2 - t1 | otherwise = t1 - t2 --t2 - t1 + 86400
  where t1 = toDouble from;t2 = toDouble to

--落子名称设置
playerName ch | ch == blackchar = "Black" | ch == whitechar = "White" | otherwise = "Unknown"

--结束语
endMessage :: Char -> [Char] -> [Char] -> [Char]
endMessage ch m1 m2 = (playerName ch) ++ " " ++ m1 ++ " (" ++ m2 ++ ")"

--调用更新棋盘实现落子
placePieces :: [Position] -> Char -> Board -> Board
placePieces ps ch b = foldr (\pos b -> updateMatrix b ch pos) b ps

--打印棋盘 用于可视化输出
printBoard :: Board -> IO()
printBoard b = do
  mapM_ putStrLn $ concatMap (\s -> [s," "]) [x ++ (show id) | (id , x) <- zip [1..] $ map (concatMap (:"  ")) b]
  putStrLn $ concatMap (\x -> show x ++ replicate (3 - length (show x)) ' ') [1..rowcnt]

--检查落子数量是否符合
--黑子第一轮落一子 其余情况均落2子
--白子均落2子
checkNumberPiece :: [Position] -> Int -> Bool
checkNumberPiece p round
  | round == 1 && length p /= 1 = False
  | round /= 1 && length p /= 2 = False
  | otherwise = True

--第57轮次 黑子只可落一子 此时不违反规则
checkNumberPiece' ::[Position]->Int->Bool
checkNumberPiece' p round
 --  | round == 57 && length p /=1 =False
   | round ==0 && length p /=1 =False
   |  round /= 0 && length p /=2 =False
   | otherwise =True 

--检查落子区域是否合法 即为空白区域
checkCorrectPlace :: [Position] -> Board -> Bool
checkCorrectPlace [] _ = True
checkCorrectPlace ((x,y):ps) b
  | b !! x !! y == backgroundchar = checkCorrectPlace ps $ placePieces [(x,y)] whitechar b
  | otherwise = False

--检测棋盘是否满 满则退出
checkBoardfull :: Board -> Bool
checkBoardfull b  = ((length (whitelist b))+(length (blacklist b))) ==225

--遍历棋盘 检测是否有人获胜
--由于是遍历 只需要设置四个方向即可
checkConnect :: Char -> Board -> Bool
checkConnect ch b = or $ map
  (\(dx,dy) -> or $ map (== replicate connectcnt ch)
  [[if checkInRange [(x+z*dx,y+z*dy)] then b !! (x+z*dx) !! (y+z*dy) else backgroundchar | z<-[0..connectcnt-1]]
  | x<-[0..rowcnt-1], y<-[0..colcnt-1]]) direction
  where direction = [(0,1),(1,0),(1,-1),(1,1)]

--前次作业读取csv自动落子的main程序
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

--获取对方棋子类型
--黑-白 白-黑
arrivalChar :: Char -> Char
arrivalChar ch
  | ch == blackchar = whitechar
  | ch == whitechar = blackchar
  | otherwise = backgroundchar

--检查落子是否在范围内 0-14为允许值
checkInRange :: [Position] -> Bool
checkInRange p = and $ map (\(x,y) -> 0 <= x && x < rowcnt && 0 <= y && y < colcnt) p

--判断是否存在一个可能6连的区域（即该区域内无对方落子）
rangehasnoarrival ::Position -> Char -> Board -> Bool
rangehasnoarrival (x,y) ch b = 
    or [rangehasnoarrival_right (x,y) ch b,rangehasnoarrival_up (x,y) ch b,rangehasnoarrival_rightup (x,y) ch b,rangehasnoarrival_rightdown (x,y) ch b ]

--判断是否存在一个可能6连的区域（即该区域内无对方落子） right方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_right ::Position -> Char -> Board -> Bool
rangehasnoarrival_right (x,y) ch b =
  if checkInRange [(x,y+5)]
  then and $ map ( /= arrivalChar ch )  [ b !! x !! (y+z)  | z<-[0..5]]
  else False

--判断是否存在一个可能6连的区域（即该区域内无对方落子） up方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_up ::Position -> Char -> Board -> Bool
rangehasnoarrival_up (x,y) ch b =
  if checkInRange [(x-5,y)]
  then and $ map ( /= arrivalChar ch )  [ b !! (x-z) !! y  | z<-[0..5]]
  else False

--判断是否存在一个可能6连的区域（即该区域内无对方落子）rightup方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_rightup ::Position -> Char -> Board -> Bool
rangehasnoarrival_rightup (x,y) ch b =
  if checkInRange [(x-5,y+5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x-z) !! (y+z)  | z<-[0..5]]
  else False

--判断是否存在一个可能6连的区域（即该区域内无对方落子）rightdown方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_rightdown ::Position -> Char -> Board -> Bool
rangehasnoarrival_rightdown (x,y) ch b =
  if checkInRange [(x+5,y+5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x+z) !! (y+z)  | z<-[0..5]]
  else False

--判断是否存在一个可能6连的区域（即该区域内无对方落子）left方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_left ::Position -> Char -> Board -> Bool
rangehasnoarrival_left (x,y) ch b =
  if checkInRange [(x,y-5)]
  then and $ map ( /= arrivalChar ch )  [ b !! x !! (y-z) | z<-[0..5]]
  else False

--判断是否存在一个可能6连的区域（即该区域内无对方落子）down方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_down ::Position -> Char -> Board -> Bool
rangehasnoarrival_down (x,y) ch b =
  if checkInRange [(x+5,y)]
  then and $ map ( /= arrivalChar ch )  [ b !! (x+z) !! y  | z<-[0..5]]
  else False

--判断是否存在一个可能6连的区域（即该区域内无对方落子）leftup方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_leftup ::Position -> Char -> Board -> Bool
rangehasnoarrival_leftup (x,y) ch b =
  if checkInRange [(x-5,y-5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x-z) !! (y-z)  | z<-[0..5]]
  else False

--判断是否存在一个可能6连的区域（即该区域内无对方落子）leftdown方向
--可能6连的区域=连续6子空间内只有我方子或者空白
rangehasnoarrival_leftdown ::Position -> Char -> Board -> Bool
rangehasnoarrival_leftdown (x,y) ch b =
  if checkInRange [(x+5,y-5)]
  then and $ map ( /= arrivalChar ch )  [b !! (x+z) !! (y-z)  | z<-[0..5]]
  else False

--递归调用maximum 输出列表最大元素
--empty list报错
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

--numberIntherangesum 表示某点（x，y）周围8个方向中 （在有可能六连区域的方向内）我方落子的综合
--用于后续赋分
numberIntherangesum :: Position -> Char -> Board -> Int
numberIntherangesum (x,y) ch b = sum [numberIntherange_right (x,y) ch b,numberIntherange_up (x,y) ch b,numberIntherange_rightdown (x,y) ch b,numberIntherange_rightup (x,y) ch b, numberIntherange_left (x,y) ch b,numberIntherange_down (x,y) ch b,numberIntherange_leftdown (x,y) ch b,numberIntherange_leftup(x,y) ch b]

--rangehasnoarrival_right是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_right :: Position -> Char -> Board -> Int
numberIntherange_right (x,y) ch b =
  if rangehasnoarrival_right (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! x !! (y+z)  | z<-[0..5]]
  else 0

--rangehasnoarrival_up是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_up :: Position -> Char -> Board -> Int
numberIntherange_up (x,y) ch b =
  if rangehasnoarrival_up (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [ b !! (x-z) !! y | z<-[0..5]]
  else 0

--rangehasnoarrival_rightdown是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_rightdown :: Position -> Char -> Board -> Int
numberIntherange_rightdown (x,y) ch b =
  if rangehasnoarrival_rightdown (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x+z) !! (y+z) | z<-[0..5]]
  else 0

--rangehasnoarrival_rightup是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_rightup :: Position -> Char -> Board -> Int
numberIntherange_rightup (x,y) ch b =
  if rangehasnoarrival_rightup (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x-z) !! (y+z) | z<-[0..5]]
  else 0

--rangehasnoarrival_left是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_left :: Position -> Char -> Board -> Int
numberIntherange_left  (x,y) ch b =
  if rangehasnoarrival_left (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! x !! (y-z)  | z<-[0..5]]
  else 0

--rangehasnoarrival_down是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_down :: Position -> Char -> Board -> Int
numberIntherange_down (x,y) ch b =
  if rangehasnoarrival_down (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [ b !! (x+z) !! y | z<-[0..5]]
  else 0

--rangehasnoarrival_leftdown是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_leftdown :: Position -> Char -> Board -> Int
numberIntherange_leftdown (x,y) ch b =
  if rangehasnoarrival_leftdown (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x+z) !! (y-z) | z<-[0..5]]
  else 0

--rangehasnoarrival_leftup是前提 否则该方向没有价值
--求该方向我方落子数
numberIntherange_leftup :: Position -> Char -> Board -> Int
numberIntherange_leftup (x,y) ch b =
  if rangehasnoarrival_leftup (x,y) ch b
  then sum $ map ( \a -> if a == ch then 1 else 0 ) [b !! (x-z) !! (y-z) | z<-[0..5]]
  else 0

--调用numberIntherangemax函数
--如果某点numberIntherangemax >=4 说明在某个六连区域存在4颗以上我方子
--再落两颗即可获胜
couldwin ::  Char -> Board -> Bool
couldwin ch b =
    or $ map (\(x,y)-> if ((numberIntherangemax (x,y) ch b) >= 4) then True else False ) $ filter (\(x,y) -> b!!x!!y == ch )[(x,y) |  x <- [0..colcnt-1], y <- [0..rowcnt-1]]

--在011110时会同时产出两个坐标
--检索20111102无法应对 属于bug 暂未修复
getthepostioncould :: Char -> Board -> [Position]
getthepostioncould ch b =
  filter (\(x,y)->(numberIntherangemax (x,y) ch b) >= 4) $ filter (\(x,y) -> b !! x !! y == ch) [(x,y) |  x <- [0..colcnt-1], y <- [0..rowcnt-1]]

--要设立多层计算 valueallboard
--落子的区域限制在已落子的周围
--whitelist ++ blacklist 产生已落子列表
--aroundhaspieces 产生以上列表周围的空区域
--存在重复 调用rmDup去重
--用quickSortvalue赋分并排列
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

--计算落完子后棋盘的总分
valueallboard :: Position ->Char -> Board -> Int
valueallboard (x,y) ch b 
  |ch == whitechar = sum ( map (\(x,y) -> numberIntherangesum (x,y) ch b ) ((rmDup(aroundhaspieces (removeItem (x,y) (whitelist b)) b))))
  |ch == blackchar = sum ( map (\(x,y) -> numberIntherangesum (x,y) ch b ) ((rmDup(aroundhaspieces (removeItem (x,y) (blacklist b)) b))))
  |otherwise = 0




--value不能使用numberIntherangesum 
--计算棋盘分数时 考察的是落子之后周围空白区域的分总和，移除该子
--实际上在这里应该调用的是blanklist而不是aroundhaspieces
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

--按照快速排序的思路设计的排序
--按照分值进行分段
--递归调用实现排序
quickSortvalue ::  [Position] -> Char -> Board -> [Position]
quickSortvalue [] ch board = []
quickSortvalue (x:xs) ch board = quickSortvalue mini ch board ++ [x] ++ quickSortvalue maxi ch board
        where mini = filter (\p -> value p ch board < value x ch board) xs
              maxi = filter (\p -> value p ch board >= value x ch board) xs



--接受的和产出的都是未修正的pos
--该函数内部存在重复需要引入一个去重复的函数rmDup
--分为几类情况
--4角落+4边+内部 共计9类情况
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


--去重叠 否则极易产生同一个坐标有多个对象
--不去重叠可能会只能落同一子
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

--阻止对方时存在两类情况
--只需要一子就可以拦截例如2111110 2111100的情况
--因此涉及到拦截的每次只考虑落1子
--这样即使对方有2个眠4 也可以都实现拦截
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
--把落2子拆分为落1子+落1子 这样判定更加精准 防守也比较灵活
-- 对方落子在getposone中判定couldwin == Ture经过第一轮修正还可能 == True 此时getblank的基点方向棋盘都要更新
--己方落子在getposone中判定couldwin == false 此时第二轮不可能couldwin了但是检测新的棋盘可能为True 所以要检测旧的棋盘 最多四连不可能五连  --这里有待思考 似乎五连肯定比四连强？再裸一个似乎也没什么不好的
--己方落子在getposone中判定couldwin == True 在第二轮必为Truw getblank中基点不变 方向不变 但是棋盘更新
--黑子第一轮指定88
--白子指定为98 89？？？别的组应该不会黑子下到这里吧 存在bug
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
--用第一步更新棋盘b成为z 这样不会出现重复的落子 算分更精准
--避免对方双4 如果三4必死
getpostwo :: Char -> Board -> [Position]
getpostwo ch b 
  | couldwin ch b == True = revisePos (take 1(getblankpostion (getdirection (take 1 (getthepostioncould ch b)) ch b) (take 1 (getthepostioncould ch b)) ch z ))
  | couldwin  (arrivalChar ch) z == True = revisePos (take 1(getblankpostion (getdirection (take 1 (getthepostioncould (arrivalChar ch) z)) (arrivalChar ch) z)  (take 1 (getthepostioncould (arrivalChar ch) z)) (arrivalChar ch) z ))
  | otherwise = take 1  (revisePos ((getnormalpos ch z)))
  where z = placePieces (getposone ch b) ch b

--修正 因为数组是0-14 棋盘代号（人类视角）为1-15
revisePos :: [Position] -> [Position]
revisePos [] = []
revisePos [(a,b)] =  map (\(x,y) -> ((x+1),(y+1))) [(a,b)]
revisePos [(a,b),(c,d)] = map (\(x,y) -> ((x+1),(y+1))) [(a,b),(c,d)]
revisePos (x:xs) = revisePos [x] ++ revisePos xs

-- 遍历棋盘 找空白落子罢了 给活5获胜凑人头的
getrandpos :: Char -> Board -> [Position]
getrandpos ch b = filter (\(x,y) -> b!!x!!y == backgroundchar ) [(x,y) |  x<- [0..colcnt-1], y <- [0..rowcnt-1]]

--getthepostioncould的结果为[Position]
--辅助函数 判断某点可以获胜的方向 
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
--有可能此时只有一个空子 如果take2就会报错 所以增加一个getrandpos 补一步棋 防止获胜回合只下1子
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

--沿right方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_right :: Position -> Char -> Board -> [Position]
getthepos_right (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x,y+z) |z<-[0..5]]

--沿up方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_up :: Position -> Char -> Board -> [Position]
getthepos_up (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x-z,y) |z<-[0..5]]

--沿rightup方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_rightup :: Position -> Char -> Board -> [Position]
getthepos_rightup (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x-z,y+z) |z<-[0..5]]

--沿rightdown方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_rightdown :: Position -> Char -> Board -> [Position]
getthepos_rightdown (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x+z,y+z)|z<-[0..5]]

--沿left方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_left :: Position -> Char -> Board -> [Position]
getthepos_left (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x,y-z)|z<-[0..5]]

--沿down方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_down :: Position -> Char -> Board -> [Position]
getthepos_down (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x+z,y)|z<-[0..5]]

--沿leftup方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_leftup :: Position -> Char -> Board -> [Position]
getthepos_leftup (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x-z,y-z)|z<-[0..5]]

--沿leftdown方向遍历空白区域 调用的前提同样是存在六子空间
getthepos_leftdown :: Position -> Char -> Board -> [Position]
getthepos_leftdown (x,y) ch b =
  filter (\(x,y) -> b!!x!!y == backgroundchar ) $ [(x+z,y-z)|z<-[0..5]]

--把输出的position格式的落子目标 转化为Move落子动作 实现接口匹配
posToMove :: [Position] -> Move
posToMove [(x,y)] = Move {t = Time 0 0 4 ,p = [(x,y)]}
posToMove [(x,y),(a,b)] = Move {t = Time 0 0 4 ,p= [(x,y),(a,b)]}
--  where t' = Time 0 0 5

--Move的落子动作 调用placepieces
placePie :: [Move] -> Char->Board -> Board
placePie (x:xs) ch b =placePie xs ch (placePieces (p x) ch b)
placePie [] ch b = b
--placePieces' m ch b=foldr (\ms b -> updateMatrix b ch (p ms)) b m

--输入黑子和白子的[Move]，以及黑子和白子的符号Char，输出在棋盘上的落子
placeBoard :: [Move] -> [Move] ->Char->Char->Board
placeBoard [] [] _ _ =emptyBoard
placeBoard (x:xs) [] ch1 ch2 = placePie [] ch2 ( placePie (x:xs) ch1 emptyBoard )
placeBoard [] (y:ys) ch1 ch2 = placePie (y:ys) ch2 ( placePie [] ch1 emptyBoard  )
placeBoard (x:xs) (y:ys) ch1 ch2 = placePie (y:ys) ch2 b
            where b = placePie (x:xs) ch1 emptyBoard
--Move {t = Time 0 0 3, p = [(5,5),(8,7)]}


--move转化为position格式 为辅助函数
moveLiToPosLi :: [Move] -> [Position]
moveLiToPosLi (x:xs) = (p x) ++ moveLiToPosLi xs
moveLiToPosLi [] = []

--输入为[Position],Board格式时,检查是否重复落子
checkCorrectPlace' :: [Position]->Board->Bool
checkCorrectPlace' [] _ =True 
checkCorrectPlace' ((x,y):ps) b 
          | b !! (x-1) !! (y-1) ==backgroundchar = checkCorrectPlace' ps (placePie [ posToMove [(x,y)] ]  blackchar b)
          | otherwise = False

--检查棋盘,看对方是否重复落子
check :: [Move] ->[Move]->Bool
check [] _ =True
check  _ [] =  True
check (x:xs) (y:ys)  = checkCorrectPlace' (moveLiToPosLi (y:ys)) b
         where b = placePie (x:xs) blackchar emptyBoard

intToString :: Int->String
intToString  a = show ((a+3600*8) `div` 3600) ++ "/" ++ show (((a+3600*8) `mod` 3600) `div` 60) ++ "/" ++ show (((a+3600*8) `mod` 3600) `mod` 60) ++ ",\"(1,1)\"" 


--把落子位置生成的Move解析成 Time2, M2 ，“(x,y)”，“(x,y)”格式，其中时间为个位时在其前添加一个0
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

--功能原理：设置main1和main2函数，互相监视“black.csv”和”white.csv”文件，当对方文件有变化时，即对方有落子，然后提取数据进行判断本方下一步落子情况。最后在main函数中调用两函数，最终实现输入0为黑子一方，输入1为白子一方
--本IO实现的功能如下：
--一：
--（1）A 程序产⽣black.csv，black.log⽂件 
--（2）B程序读取black.csv，产⽣white.csv，white.log 
--（3）A程序读取white.csv，更新black.csv，black.log 
--（4）B程序读取black.csv，更新white.csv，white.log 
--（5）重复（3）-（4）
--二：违规判定
--（1）是否超时判定，若超时120秒，则停止比赛，超时者输
--（2）落子是否重复判定，若对方落子处已有棋子，则对方输
--（3）多掷一子或少掷一子判定
--（4）无视对方胜利继续落子判定
--（5）判断是否白子违规先手
--（6）举报对方篡改本方落子或篡改对方落子判定


--此为黑子一方
--扫描白子的csv文件，并AI得到下一步落子写入黑子csv文件中
--并进行相应的违规判定
main1 = do
  currTime <- getCurrentTime
  let timed = floor $ utctDayTime currTime :: Int--得到可计算的系统时间
  black <- readFile "black1.csv"--为了防止黑子和白子读取black.csv文件冲突，引入black1.csv文件
  let blackmoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black--将字符串转为[Move]格式
  white' <- readFile "white.csv" 
  let whitemoves' = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white'--将字符串转为[Move]格式
  if length whitemoves' == ((length blackmoves) - 1)
  then main1  --判断“white.csv”是否落子，若无落子，则继续监视
  else if length whitemoves' /= (length blackmoves)
  then appendFile "black.log" ("stop wrong,may be white run first"++"\n")        
  else do 
  white <- readFile "white.csv"
  let whitemoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white --将字符串转为[Move]格式
      newBoard = placeBoard blackmoves whitemoves blackchar whitechar
      
  appendFile "checkWhite.csv" ( mvToString (last ([Move{t=Time 0 0 0,p=[(1,1)]}]++whitemoves)) whitemoves (timed+(3600*8)) ++ "\n")--为了检查是否重复落子而增设的一个csv文件
  checkWhite'<-readFile "checkWhite.csv"
  let checkW = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') checkWhite'
  
  if checkConnect whitechar newBoard==True --判断对方是否六连
  then appendFile "black.log" ("white connect 6,win!"++"\n")        
  else if checkBoardfull newBoard == True--判断是否平局
  then appendFile "black.log" ("no one win,it's a tie!"++"\n")                
  else if check blackmoves whitemoves' == False --判断白子是否重复落子
  then appendFile "black.log" ("white put in the place where have the chess"++"\n")
  else if length blackmoves == 0 && length  whitemoves' /=0 -- --判断白子是否违规先手
  then appendFile "black.log" ("error,white play first"++"\n")   
  else if timeElapse (t (last([Move{t = Time 0 0 0,p = [(0,0)]}]++blackmoves))) (t (last([Move{t = Time 0 0 0,p =[(0,0)]}]++whitemoves))) >120--判断对方是否超时
  then  appendFile "black.log" ("white out of time"++"\n")          
  else if timeElapse (t (last([Move{t = Time 0 0 0,p =[(0,0)]}]++blackmoves))) (t (last([Move{t = Time 0 0 0,p =[(0,0)]}]++whitemoves'))) <0
  then  appendFile "black.log" ("white time is error,maybe white put in more faster"++"\n")                                            
  else if checkNumberPiece' (p (last ([Move {t = Time 0 0 4, p = [(0,1)]}] ++ whitemoves')))  (length whitemoves')  == False --判断白子是否多落子或少落子
  then  appendFile "black.log" ("white input error,less or more"++"\n")     
  else do
  let  mov1 = posToMove(getpos blackchar newBoard)
  (tempName,tempHandle)<- openTempFile "." "temp"--为了预防写入和读取文件冲突，采用临时文件black1.csv，即删除替换操作
  hPutStr tempHandle black
  hClose tempHandle
  removeFile "black1.csv"
  renameFile tempName "black1.csv"
  appendFile "black1.csv"  (mvToString mov1 blackmoves (timed+(3600*8)) ++ "\n")
  blackPlayer1<-readFile "black1.csv"
  let blackmoves1 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') blackPlayer1
      newBoard1 = placeBoard blackmoves1 whitemoves blackchar whitechar
  appendFile "black.csv"  (mvToString (last blackmoves1) blackmoves (timed+(3600*8)) ++ "\n")--将black1.csv文件中最后的落子复制到black.csv文件
  putStrLn $ "            "
  putStrLn $ "black : O"
  putStrLn $ "white : X"
  putStrLn $ "black turns: " ++ show (length blackmoves1)
  putStrLn $ show (p mov1)
  printBoard newBoard1
  bb<-readFile "black.csv"
  if bb /= blackPlayer1
  then appendFile "black.log" ("maybe white falsified(篡改) black's file" ++ "\n")--判断对方是否篡改我方文件
  else if tail (moveLiToPosLi checkW) /= moveLiToPosLi whitemoves 
  then appendFile "black.log" ("maybe white falsified(篡改) itself's file" ++ "\n")--判断对方是否篡改自己的文件
  else if checkConnect blackchar newBoard1==True
  then appendFile "black.log" ("black connect 6,win!"++"\n")--putStrLn $ "black connect 6,win!" --判断我方是否六连
  else if checkBoardfull newBoard1 == True
  then appendFile "black.log" ("no one win,it's a tie!"++"\n")    --putStrLn $ "no one win,it's a tie!"--判断是否平局
  else main1--循环



--此为白子一方
--扫描黑子的csv文件，并AI得到下一步落子写入白子csv文件中
--并进行相应的违规判定
main2 = do
  currTime <- getCurrentTime
  let timed = floor $ utctDayTime currTime :: Int--得到可计算的系统时间
  white <- readFile "white1.csv"--为了防止黑子和白子读取white.csv文件冲突，引入white1.csv文件
  let whitemoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white--将字符串转为[Move]格式
  black' <- readFile "black.csv" 
  let blackmoves' = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black' --将字符串转为[Move]格式
  if length whitemoves == (length blackmoves')  
  then main2 --判断“black.csv”是否落子，若无落子，则继续监视
  else if length whitemoves /= ( (length blackmoves') -1) --判断黑子先手是否多走
  then  appendFile "white.log" ("stop wrong,maybe black input more chess than it can"++"\n")    
  else do
  black <- readFile "black.csv"
  let blackmoves = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black --将字符串转为[Move]格式
      newBoard = placeBoard blackmoves whitemoves blackchar whitechar
          
  appendFile "checkBlack.csv" ( mvToString (last blackmoves) blackmoves (timed+(3600*8)) ++ "\n")--为了检查是否重复落子而增设的一个csv文件
  checkBlack'<-readFile "checkBlack.csv"
  let checkB = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') checkBlack'
      
  if checkConnect blackchar newBoard==True--判断对方是否六连
  then appendFile "white.log" ("black connect 6,win!"++"\n")  --putStrLn $ "black connect 6,win!"
  else if checkBoardfull newBoard == True--判断是否平局
  then appendFile "white.log" ("no one win,it's a tie!"++"\n")--putStrLn $ "no one win,it's a tie!"
  else if check whitemoves blackmoves' == False--判断对方是否重复落子
  then appendFile "white.log" ("black put in the place where have the chess"++"\n")
  else if timeElapse (t (last (blackmoves'++whitemoves))) (t (last (blackmoves))) >120--判断对方是否超时
  then appendFile "white.log" ("black out of time"++"\n")--putStrLn $ "black out of time" 
  else if timeElapse (t (last (blackmoves'++whitemoves))) (t (last (blackmoves'))) <0
  then appendFile "white.log" ("black time is error,maybe black put in more faster"++"\n")
  else if checkNumberPiece (p (last blackmoves')) (length blackmoves') ==False --判断对方是否多落子或少落子
  then appendFile "white.log" ("black error,input more or less"++"\n")
  else do
  let mov1 = posToMove(getpos whitechar newBoard)
  (tempName,tempHandle)<- openTempFile "." "temp"--为了预防写入和读取文件冲突，采用临时文件white1.csv，即删除替换操作
  hPutStr tempHandle white
  hClose tempHandle
  removeFile "white1.csv"
  renameFile tempName "white1.csv"
  appendFile "white1.csv"  (mvToString mov1  whitemoves (timed+(3600*8)) ++ "\n")
  whitePlayer1<-readFile "white1.csv"
  let whitemoves1 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') whitePlayer1
      newBoard1 = placeBoard blackmoves whitemoves1 blackchar whitechar
  appendFile "white.csv"  (mvToString (last whitemoves1) whitemoves (timed+(3600*8)) ++ "\n")--将white1.csv文件中最后的落子复制到white.csv文件
  putStrLn $ "            "
  putStrLn $ "black : O"
  putStrLn $ "white : X"
  putStrLn $ "white turns: "++ show (length whitemoves1)
  putStrLn $ show (p mov1)
  printBoard newBoard1
  ww<-readFile "white.csv"
  if ww /= whitePlayer1
  then appendFile "white.log" ("maybe black falsified(篡改) white's file" ++ "\n") --判断对方是否篡改我方文件
  else if (moveLiToPosLi checkB) /= moveLiToPosLi blackmoves 
  then appendFile "white.log" ("maybe black falsified(篡改) itself's file" ++ "\n")--判断对方是否篡改自己的文件
  else if checkConnect whitechar newBoard1==True --判断己方是否六连
  then appendFile "white.log" ("white connect 6,win!"++"\n")
  else if checkBoardfull newBoard1 == True --判断是否平局
  then appendFile "white.log" ("no one win,it's a tie!"++"\n")
  else main2--循环

--调用main2,继而判断在白子赢得情况下，黑子是否继续落子
main4 = do
--   removeFile "white.csv"
   appendFile "white.csv"  ("")
   appendFile "white1.csv" ("")--为了防止黑子和白子读取white.csv文件冲突，引入white1.csv文件
   appendFile "black.csv" ("")
   main2--调用main2
   black2<-readFile "black.csv"
   white2<-readFile "white.csv"
   let whitemoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white2--将字符串转为[Move]格式
       blackmoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black2--将字符串转为[Move]格式
       newBoard1 = placeBoard blackmoves2 whitemoves2 blackchar whitechar
   if checkConnect whitechar newBoard1==True && length blackmoves2 /= (length whitemoves2)--判断在白子赢的情况下黑子是否继续落子
   then appendFile "white.log" ("black error,white win,but black continue to play"++"\n")--putStrLn $ "black error,white win,but black continue to play"
   else putStrLn $ "Game Over!"

--调用main1,继而判断在黑子赢得情况下，白子是否继续落子
main5 = do
   appendFile "black1.csv" ("")--为了防止黑子和白子读取black.csv文件冲突，引入black1.csv文件
   appendFile "white.csv" ("")
   appendFile "black.csv" ("")
   main1--调用main1
   black2<-readFile "black.csv"
   white2<-readFile "white.csv"
   let whitemoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') white2--将字符串转为[Move]格式
       blackmoves2 = map stringToMove $ wordsWhen (\ch -> ch == '\r' || ch == '\n') black2--将字符串转为[Move]格式
       newBoard1 = placeBoard blackmoves2 whitemoves2 blackchar whitechar
   if checkConnect blackchar newBoard1==True && length blackmoves2 /=( (length whitemoves2) +1) --判断在黑子赢得情况下，白子是否继续落子
   then appendFile "black.log" ("white error,black win,but white continue to play"++"\n")--putStrLn $ "white error,black win,but white continue to play"
   else putStrLn $ "Game Over!"

--main函数，在输入0时调用main5函数，在输入1时，调用main4函数,若输入0，1以外的数字，则提示输入错误，重新输入
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
  else do
  putStrLn $ "you put error number in,please put again!"
  main



--测试棋盘，用来测试各类情况
--以及debug棋盘 在报错情况下复现情形 同时进行测试
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
m = placePieces [(8,8),(8,9),(8,7),(8,6)] whitechar(placePieces [(7,7)] blackchar emptyBoard)
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

