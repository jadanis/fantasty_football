import Prob
import Control.Applicative
import Data.Ratio
import Statistics.Distribution.Normal


data Player
  = Josh
  | Pat
  | Ben
  | Kyle
  | Max
  | John
  | Jake
  | Sam
  | Grant
  | Sheri
  deriving (Show,Read,Eq,Ord)

type Record = (Player,Int,Int)

type Standings = [Record]

type Playoff = (Player,Player,Player,Player)

type NFLPlayer = [Float]

type LineUp = [NFLPlayer]

type Team = Map Player LineUp

type Match = (Player,Player)

playerStat :: NFLPlayer -> (Float,Float)
playerStat pl = (u,v)
  where av ls = (sum ls) / (fromIntegral $ length ls)
        u = av pl
        v = (av $ map (^2) pl) - u^2

teamStat :: LineUp -> (Float,Float)
teamStat ln = (u,v)
  where sts = map playerstat ln
        us = [ x | (x,y) <- sts ]
        vs = [ y | (x,y) <- sts ]
        u = sum us
        v = sum vs

matchStat :: Team -> Match -> (Float,Float)
matchStat t mtch = (u1-u2, v1+v2)
  where t1 = t ! (fst mtch)
        t2 = t ! (snd mtch)
        (u1,v1) = teamStat t1
        (u2,v2) = teamStat t2

-- Create the Normal Distribution from matchStat
-- Get probabilities from Distribution
-- Use this to create weighted Prob of matches 

fst' (x,_,_) = x

snd' (_,x,_) = x

trd (_,_,x) = x 

playoff :: Standings -> Playoff
playoff stds = (fst' p1,fst' p2,fst' p3,fst' p4)
  where m1 = playoff' stds
        p1s = filter ((flip $ (==) . snd') m1) stds
        s1 = foldl (flip $ max . trd) 0 p1s
        p1 = head $ filter ((flip $ (==) . trd) s1) p1s
        stds' = filter (/= p1) stds
        m2 = playoff' stds'
        p2s = filter ((flip $ (==) . snd') m2) stds'
        s2 = foldl (flip $ max . trd) 0 p2s
        p2 = head $ filter ((flip $ (==) . trd) s2) p2s
        stds'' = filter (/= p2) stds'
        m3 = playoff' stds''
        p3s = filter ((flip $ (==) . snd') m3) stds''
        s3 = foldl (flip $ max . trd) 0 p3s
        p3 = head $ filter ((flip $ (==) . trd) s3) p3s
        stds_3 = filter (/= p3) stds''
        m4 = playoff' stds_3
        p4s = filter ((flip $ (==) . snd') m4) stds_3
        s4 = foldl (flip $ max . trd) 0 p4s
        p4 = head $ filter ((flip $ (==) . trd) s4) p4s

playoff' :: Standings -> Int
playoff' stds = foldl (flip $ max . snd') 0 stds

matchUp :: (Player,Player) -> Prob Record
matchUp (p1,p2) = makeProb [(p1,1,0),(p2,1,0)]

matchUps :: [(Player,Player)] -> Prob [Record]
matchUps [] = pure []
matchUps (x:xs) = liftA2 (:) (matchUp x) (matchUps xs)

updateStanding' :: Record -> Standings -> Standings
updateStanding' rec stds =
  case stds of
    [] -> rec:[]
    s:res ->
      if (fst' rec) == (fst' s) then (fst' rec, (snd' rec) + (snd' s),(trd s) + (trd rec)):res else s:(updateStanding' rec res)

updateStanding :: [Record] -> Standings -> Standings
updateStanding recs stds =
  case recs of
    [] -> stds
    (r:rs) -> updateStanding rs $ updateStanding' r stds

--week11 = joinProb $ updateStanding <$> (matchUps week11Matchups) <*> pure week10

week12 = joinProb $ updateStanding <$> (matchUps week12Matchups) <*> week11

week13 = joinProb $ updateStanding <$> (matchUps week13Matchups) <*> week12

playoffs = joinProb $ fmap playoff week13

playerIn'' :: Player -> Playoff -> Bool
playerIn'' p (p1,p2,p3,p4) = (p == p1) || (p == p2) || (p == p3) || (p == p4)

playerIn' :: Player -> Prob Bool
playerIn' p = joinProb $ fmap (playerIn'' p) playoffs

playerIn :: Player -> String
playerIn p = (show p) ++ " is in " ++ (show num) ++ " out of " ++ (show den) ++ " times. (" ++ (show per) ++ "%)"
  where prob = playerIn' p
        rep = filter ((True ==) . fst) $ getProb prob
        num = if length rep > 0 then numerator $ snd $ head rep else 0
        den = if length rep > 0 then denominator $ snd $ head rep else 0
        per = if den /= 0 then (fromIntegral num) / (fromIntegral den) * 100 else 0

sam = playerIn Sam
ben = playerIn Ben
pat = playerIn Pat
josh = playerIn Josh
kyle = playerIn Kyle
john = playerIn John
jake = playerIn Jake
grant = playerIn Grant
sheri = playerIn Sheri
maxx = playerIn Max

players = [sam,ben,pat,josh,kyle,john,jake,grant,sheri,maxx]

isIn = map putStrLn players

main = do
  sequence_ isIn

week10 =
  [ (Sam,8,1164)
  , (Ben,7,1180)
  , (Pat,6,1216)
  , (Josh,6,1153)
  , (Kyle,6,1057)
  , (John, 4,1193)
  , (Jake,4,1177)
  , (Grant,4,1026)
  , (Sheri, 4,969)
  , (Max,1,925)
  ]

week11Matchups = 
  [ (Josh,Jake)
  , (Pat,Max)
  , (John,Ben)
  , (Grant,Sheri)
  , (Sam,Kyle)
  ]

week11 =
  pure
    [ (Sam,9,1264)
    , (Ben,7,1266)
    , (Pat,6,1330)
    , (Josh,6,1252)
    , (Kyle,6,1152)
    , (John, 5,1283)
    , (Jake,5,1290)
    , (Grant,5,1131)
    , (Sheri, 4,1050)
    , (Max,2,1069)
    ]

week12Matchups =
  [ (Josh,Pat)
  , (John, Max)
  , (Grant,Sam)
  , (Ben,Kyle)
  , (Sheri,Jake)
  ]

week13Matchups =
  [ (Josh, John)
  , (Pat,Sheri)
  , (Grant, Kyle)
  , (Ben,Max)
  , (Sam,Jake)
  ]
