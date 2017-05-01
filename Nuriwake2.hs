import Data.Array.Unboxed
import Data.List
import Data.Maybe

type Point = (Int,Int)
type Bitmap = UArray Point Bool

-- テキスト前処理
cropText :: [String] -> [String]
cropText strs = let
  h = length strs
  w = minimum $ map length strs
  indices = map (elemIndices '#') strs
  top = (length $ takeWhile (==[]) indices) 
  bottom = h - (length $ takeWhile (==[]) $ reverse indices)
  left = minimum $ concat indices 
  right = (maximum $ concat indices) +1
  in
     map (drop left) $ map (take right)(drop top $ take bottom strs)
  
textBitmap :: [String] -> Bitmap
textBitmap []  = listArray ((0,0),(0,0)) [False] -- 強引な処理
textBitmap strs  = let
  toBool '#' = True
  toBool '.' = False
  ixs = [(x,y) | y<-[0..(length strs)-1], x<-[0..(maximum $ map length strs)-1]]
  imax = maximum ixs
  in
    array ((0,0),imax) (zip ixs (map toBool $ concat strs))

-- ピクセル移動
-- 失敗したらNothing
shiftPixel :: Bitmap -> Point -> Point -> Maybe Bitmap
shiftPixel b src dest 
  | ps == False && pd == False = Just b
  | ps == False && pd == True  = Just b
  | ps == True  && pd == False = Nothing
  | ps == True  && pd == True  =
      Just $ b // [(src, False), (dest, False)]
  where
    ps = b ! src
    pd = b ! dest

-- 再帰的にチェック
-- 失敗したら探索打ち切り
checkPixels :: Maybe Bitmap -> [(Point,Point)] -> Bool
checkPixels Nothing _ = False
checkPixels (Just b) [] = not $ or $ elems b
checkPixels (Just b) (p:ps) = let
  (src,dest) = p
  b' = shiftPixel b src dest 
  in
    checkPixels b' ps

-- 移動量から移動前後の位置リストを作り探索
checkOneCase :: Bitmap -> Point -> Bool
checkOneCase b (x,y) = let
  (x',y') = maximum $ indices b
  ps = if x >= 0
       then [((xa,ya),(xa+x,ya+y)) | ya <- [0..y'-y], xa <- [0..x'-x]]
       else [((xa,ya),(xa+x,ya+y)) | ya <- [0..y'-y], xa <- [abs x..x']]
  in
    checkPixels (Just b) ps

-- 取りうる移動量全てに対し探索
solveNuriwake :: Bitmap -> Bool
solveNuriwake b = let
  (w,h) = maximum $ indices b 
  ss = [(x,y) | y <- [0..h], x <- [-w..w], not(x<=0 && y==0)]
  n = length $ filter id $ elems b
  in
    if  odd n || n == 0
    then False
    else or $ map (checkOneCase b)  ss
  
main = do
  getLine -- 1行目のサイズは必要ないので無視
  str <- getContents
  let problem = textBitmap . cropText $ lines str
  if solveNuriwake problem
    then putStr "YES"
    else putStr "NO"

--------------------以下テスト用
benchNuriwake :: IO()
benchNuriwake = do
  mapM_ expNuriwake testList
  return ()

expNuriwake x = do
    str <- readFile $ "sample/" ++ x
    let p = textBitmap . cropText . tail $ lines str
    if solveNuriwake p
      then putStrLn $ x++" YES"
      else putStrLn $ x++ " NO"
    return ()

testList = [
  "sample01.txt",
  "sample02.txt",
  "sample03.txt",
  "sample04.txt",
  "sample05.txt",
  "sample06.txt",
  "hand01.txt",
  "hand02.txt",
  "hand03.txt",
  "hand04.txt",
  "hand05.txt",
  "hand06.txt",
  "hand07.txt",
  "hand08.txt",
  "hand09.txt",
  "hand10.txt",
  "hand11.txt",
  "hand12.txt",
  "hand13.txt",
  "hand14.txt",
  "hand15.txt",
  "hand16.txt",
  "hand17.txt",
  "hand18.txt",
  "hand19.txt",
  "hand20.txt",
  "random01.txt",
  "random02.txt",
  "random03.txt",
  "random04.txt",
  "random05.txt",
  "random06.txt",
  "random07.txt",
  "random08.txt",
  "random09.txt",
  "random10.txt",
  "system_test1.txt",
  "system_test2.txt",
  "system_test3.txt",
  "system_test4.txt",
  "system_test5.txt",
  "system_test6.txt"]  
