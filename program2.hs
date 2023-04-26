{-
name: Alexandria Mwaura
date: Sunday April 09, 2023
class: CS424-01
explanation -- haskell program read ppm and manipulate image 
 ---- ALL Filters ---
grayscale
flatten red,green & blue
vertically flip
invert 
-}

import System.Random
import System.IO
import Data.List
import Data.List.Split 


toInt :: [String] -> [Int]
toInt [] = []
toInt t = foldr (\ h -> (++) [read h]) [] t

--remove red - subtracting R value
noRed :: Num a => [a] -> [a]
noRed list =
   case list of 
      [] -> []
      r:g:b:rest -> (r-r):g:b: noRed rest

--remove green - subtracting G value
noGreen :: Num r => [r] -> [r]
noGreen list =
   case list of
      [] -> []
      r:g:b:rest -> r:(g-g):b : noGreen rest

--remove blue - subtracting B value
noBlue :: Num r => [r] -> [r]
noBlue list =
   case list of
      [] -> []
      r:g:b:rest -> r:g:(b-b) : noBlue rest

{- Invert image colors
Invert the colors by negating them
-}
invertPPM :: Num r => [r] -> [r]
invertPPM list =
   case list of
      [] -> []       --or 255 for other ppm input file 
      r:g:b:rest -> (255 - r):(255 - g):(255 - b) : invertPPM rest

-- greyscale image
-- returns the gray value of a pixel 
greyPPM :: Integral r => [r] -> [r]
greyPPM list =
   case list of
      [] -> []
      r:g:b:rest -> div (r + g + b) 3 : div (r + g + b) 3: div (r + g + b) 3: greyPPM rest


--extreme contrast photo
exContrast :: Num r => [r] -> [r]
exContrast list = 
   case list of 
      [] -> []
      r:g:b:rest_int -> r : g : b : exContrast rest_int
      -- let x  = let x =randomRIO (1, 100)
      
main :: IO ()
main = do  
   let file = "cake.ppm"  -- file name 
   input <- readFile file     -- reading file into contents string
   --print input

   let contents = lines input
   --print contents             -- output contents of file 
   
   let flattened_contents = words (unwords contents)
   --print flattened_contents

-- Store the header and date seperately in list 
   let headerInfo = take 15 input  --take 29 input  -- flattened_contents
   let rest = drop 4 flattened_contents

-- Convert rest into a list of integers (currently a list of strings)
   let rest_ints = toInt rest
   print "orignal "
   
-- NO RED ---
   let rresult = noRed rest_ints
   print "no red"

-- NO GREEN -- 
   let gresult = noGreen rest_ints
   print "no Green"

-- NO BLUE -- 
   let bresult = noBlue rest_ints
   print "no Blue"

-- invert ppm image 
   let nVertppm = invertPPM rest_ints
   print "Invert Image"

-- greyscale 
   let greyppm = greyPPM rest_ints
   print "GreyScale Image"

   let extppm = exContrast rest_ints
   print "Extreme Image"

--vertical ppm  --x axis flip 
   print "vertical Flipped Image"
   print "reverse contents"  

   
   
--  Each sublist will have n items, and the start of each sublist
--  will be offset by m items from the previous one.
   let ori = divvy 100 100 rest_ints  -- orignal list 
   let ft = transpose ori
   --print ft 
   let new_Redlist = divvy 6 6 rresult
   let new_Greenlist = divvy 6 6 gresult
   let new_Bluelist = divvy 6 6 bresult
   let new_Nvertlist = divvy 6 6 nVertppm
   let new_greylist = divvy 6 6 greyppm
   let new_vertList  = reverse (divvy 6 6 rest_ints) -- vertical flipped list
   let new_extClist = divvy 6 6 extppm

   let nored = let strs = map (map show) new_Redlist
                          in unlines (map unwords strs)
   let nogreen = let strs = map (map show) new_Greenlist
                        in unlines (map unwords strs)
   let noblue = let strs = map (map show) new_Bluelist 
                          in unlines (map unwords strs)
   let invertImage = let strs = map (map show) new_Nvertlist 
                          in unlines (map unwords strs)
   let greyImage = let strs = map (map show) new_greylist
                           in unlines (map unwords strs)
   let vflipImage = let strs = map (map show) new_vertList 
                           in unlines (map unwords strs)
   let extConImage = let strs = map (map show) new_extClist
                           in unlines (map unwords strs)
   let redPPM = headerInfo ++ nored
   let greenPPM = headerInfo ++ nogreen
   let bluePPM = headerInfo ++ noblue
   let inVertPPM = headerInfo ++ invertImage
   let greyPPM = headerInfo ++ greyImage
   let vflipPPM = headerInfo ++ vflipImage
   let extremecont = headerInfo ++ extConImage
   
   -- output files  -- 
   writeFile "filename_nored.ppm" redPPM
   writeFile "filename_nogreen.ppm" greenPPM
   writeFile "filename_noblue.ppm" bluePPM
   writeFile "filename_inverted.ppm" inVertPPM
   writeFile "filename_greyscale.ppm" greyPPM
   writeFile "filename_vflip.ppm" vflipPPM
   writeFile "filename_extreme.ppm" extremecont


