import Prob
import Control.Applicative
import Data.Ratio


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

type Record = (Player,Int)

type Standings = [Record]

type Playoff = (Player,Player,Player,Player)

playoff :: Standings -> Playoff
playoff stds = (fst p1,fst p2,fst p3,fst p4)
  where m1 = playoff' stds
        p1 = head $ filter ((flip $ (==) . snd) m1) stds
        stds' = filter (/= p1) stds
        m2 = playoff' stds'
        p2 = head $ filter ((flip $ (==) . snd) m2) stds'
        stds'' = filter (/= p2) stds'
        m3 = playoff' stds''
        p3 = head $ filter ((flip $ (==) . snd) m3) stds''
        stds_3 = filter (/= p3) stds''
        m4 = playoff' stds_3
        p4 = head $ filter ((flip $ (==) . snd) m4) stds_3

playoff' :: Standings -> Int
playoff' stds = foldl (flip $ max . snd) 0 stds

matchUp :: (Player,Player) -> Prob Record
matchUp (p1,p2) = makeProb [(p1,1),(p2,1)]

matchUps :: [(Player,Player)] -> Prob [Record]
matchUps [] = pure []
matchUps (x:xs) = liftA2 (:) (matchUp x) (matchUps xs)

updateStanding' :: Record -> Standings -> Standings
updateStanding' rec stds =
  case stds of
    [] -> rec:[]
    s:res ->
      if (fst rec) == (fst s) then (fst rec, (snd rec) + (snd s)):res else s:(updateStanding' rec res)

updateStanding :: [Record] -> Standings -> Standings
updateStanding recs stds =
  case recs of
    [] -> stds
    (r:rs) -> updateStanding rs $ updateStanding' r stds

week11 = joinProb $ updateStanding <$> (matchUps week11Matchups) <*> pure week10

week12 = joinProb $ updateStanding <$> (matchUps week12Matchups) <*> week11

week13 = joinProb $ updateStanding <$> (matchUps week13Matchups) <*> week12

playoffs = joinProb $ fmap playoff week13

playerIn'' :: Player -> Playoff -> Bool
playerIn'' p (p1,p2,p3,p4) = (p == p1) || (p == p2) || (p == p3) || (p == p4)

playerIn' :: Player -> Prob Bool
playerIn' p = joinProb $ fmap (playerIn'' p) playoffs

playerIn :: Player -> String
playerIn p = (show p) ++ " is in " ++ (show num) ++ " out of " ++ (show den) ++ " times"
  where prob = playerIn' p
        rep = filter ((True ==) . fst) $ getProb prob
        num = if length rep > 0 then numerator $ snd $ head rep else 0
        den = if length rep > 0 then denominator $ snd $ head rep else 0

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
  [ (Sam,8)
  , (Ben,7)
  , (Pat,6)
  , (Josh,6)
  , (Kyle,6)
  , (John, 4)
  , (Jake,4)
  , (Grant,4)
  , (Sheri, 4)
  , (Max,1)
  ]

week11Matchups = 
  [ (Josh,Jake)
  , (Pat,Max)
  , (John,Ben)
  , (Grant,Sheri)
  , (Sam,Kyle)
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
