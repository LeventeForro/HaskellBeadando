{-# LANGUAGE LambdaCase #-}

module Beadando where
import Data.List (elemIndex, partition)

showState a = show a
showMage a = show a
eqMage a b =  a == b
showUnit a = show a
showOneVOne a = show a

type Name = String
type Health = Integer
type Spell = (Integer -> Integer)
type Army = [Unit]
type EnemyArmy = Army
type Amount = Integer

-- ..........................................1. Feladat ..........................................
-- a)

data State a = Alive a | Dead

instance Eq a => Eq (State a) where
  (==) state1 state2 = case (state1, state2) of
    (Dead, Dead) -> True
    (Alive x, Alive y) -> x == y
    (_, _) -> False

instance Show a => Show (State a) where
  show Dead = "Dead"
  show (Alive x) = show x

-- b)

data Entity  = Golem Health | HaskellElemental Health

instance Show Entity where
  show (Golem health) = "Golem " ++ show health
  show (HaskellElemental health) = "HaskellElemental " ++ show health

instance Eq Entity where
  (==) e1 e2 = case (e1, e2) of
    (Golem h1, Golem h2) -> h1 == h2
    (HaskellElemental h1, HaskellElemental h2) -> h1 == h2
    (_, _) -> False

-- c)
data Mage = Master {
  mageName :: String,
  mageHealth :: Health,
  mageSpell :: Spell
}


instance Show Mage where
  show (Master name health _) =
    if health < 5 && health >= 0
      then "Wounded " ++ name
      else if health <= 0
        then "Dead"
        else name

instance Eq Mage where
  (Master name1 health1 _) == (Master name2 health2 _) = name1 == name2 && health1 == health2

papi = let 
    tunderpor enemyHP
        | enemyHP < 8 = 0
        | even enemyHP = div (enemyHP * 3) 4
        | otherwise = enemyHP - 3
    in Master "Papi" 126 tunderpor
java = Master "Java" 100 (\x ->  x - (mod x 9))
traktor = Master "Traktor" 20 (\x -> div (x + 10) ((mod x 4) + 1))
jani = Master "Jani" 100 (\x -> x - div x 4)
skver = Master "Skver" 100 (\x -> div (x+4) 2)
potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
  in Master "PotionMaster" 170 plx

-- d)

data Unit = M (State Mage) | E (State Entity)

instance Show Unit where
  show (M (Alive mage)) = show mage
  show (E (Alive entity)) = show entity
  show (M Dead) = "Dead"
  show (E Dead) = "Dead"


instance Eq Unit where
  (==) (M state1) (M state2) = state1 == state2
  (==) (E state1) (E state2) = state1 == state2
  (==) _ _ = False

-- ..........................................2. Feladat ..........................................


formationFix :: Army -> Army
formationFix army = let
    fallenUnits = getFallenUnits army
    (aliveUnits, deadUnits) = partition isAlive army
  in aliveUnits ++ deadUnits

getFallenUnits :: Army -> Army
getFallenUnits army = [unit | unit <- army, not (isAlive unit)]

isAlive :: Unit -> Bool
isAlive (M state) = state /= Dead
isAlive (E state) = state /= Dead


-- ..........................................3. Feladat ..........................................

over :: Army -> Bool
over units = all halott units
  where
    halott :: Unit -> Bool
    halott (M Dead) = True
    halott (E Dead) = True
    halott _        = False

-- ..........................................4. Feladat ..........................................

fight :: EnemyArmy -> Army -> Army
fight attackingArmy [] = []
fight [] defendingArmy = defendingArmy
fight [E Dead] [E Dead] = [E Dead]
fight [E Dead] [M Dead] = [M Dead]
fight [M Dead] [E Dead] = [E Dead]
fight [E Dead] defendingArmy = defendingArmy
fight [M Dead] defendingArmy = defendingArmy
fight attackingArmy [E Dead] = [E Dead]
fight attackingArmy [M Dead] = [M Dead]

fight (attacker:restAttackers) (defender:restDefenders) =
  case (attacker, defender) of
    (E (Alive (Golem _)), E (Alive (Golem h))) ->
      isDead(E (Alive (Golem (h - 1)))) : fight restAttackers restDefenders
    (E (Alive (HaskellElemental _)), E (Alive (Golem h))) ->
      isDead(E (Alive (Golem (h - 3)))) : fight restAttackers restDefenders
    (M (Alive (Master name h varazs)), egyseg) ->
      ((damage varazs egyseg)) : fight restAttackers (applyMageSpell restDefenders varazs)
    (E (Alive (Golem _)), M (Alive (Master name h spell))) ->
      isDead(M (Alive (Master name (h - 1) spell))) : fight restAttackers restDefenders
    (E (Alive (Golem _)), E (Alive (HaskellElemental h))) ->
      isDead(E (Alive (HaskellElemental (h - 1)))) : fight restAttackers restDefenders
    (E Dead, E (Alive (Golem h))) ->
      isDead(E (Alive (Golem (h)))) : fight restAttackers restDefenders
    (E Dead, E (Alive (HaskellElemental h))) ->
      isDead(E (Alive (HaskellElemental (h)))) : fight restAttackers restDefenders
    (M Dead, E (Alive (Golem h))) ->
      isDead(E (Alive (Golem (h)))) : fight restAttackers restDefenders
    (M Dead, E (Alive (HaskellElemental h))) ->
      isDead(E (Alive (HaskellElemental (h)))) : fight restAttackers restDefenders
    (E Dead, M (Alive (Master name h spell))) ->
      isDead(M (Alive (Master name h spell))) : fight restAttackers restDefenders
    (E (Alive (HaskellElemental _)), E (Alive (HaskellElemental h))) ->
      isDead(E (Alive (HaskellElemental (h - 3)))) : fight restAttackers restDefenders
    (E (Alive (HaskellElemental _)), M (Alive (Master name h spell))) ->
      isDead(M (Alive (Master name (h - 3) spell))) : fight restAttackers restDefenders
    (M Dead, M (Alive (Master name h spell))) ->
      isDead(M (Alive (Master name (h) spell))) : fight restAttackers restDefenders
    (_, _) -> fight restAttackers restDefenders


applyMageSpell:: Army -> Spell -> Army
applyMageSpell (army) varazs = map (damage varazs) army

damage :: Spell -> Unit -> Unit
damage varazs (E (Alive (Golem h)))  = isDead(E (Alive (Golem (varazs h))))
damage varazs (E (Alive (HaskellElemental h)))  = isDead(E (Alive (HaskellElemental (varazs h))))
damage varazs (M (Alive (Master name h spell))) = isDead(M (Alive (Master name (varazs h) spell)))
damage varazs (E Dead) = (E Dead)
damage varazs (M Dead) = (M Dead)

isDead :: Unit -> Unit
isDead (E (Alive (Golem h)))
  | h <= 0 = (E Dead)
  | otherwise = (E (Alive (Golem h)))
isDead (E (Alive (HaskellElemental h)))
  | h <= 0 = (E Dead)
  | otherwise = (E (Alive (HaskellElemental h)))
isDead (M (Alive (Master name h spell)))
  | h <= 0 = (M Dead)
  | otherwise = (M (Alive (Master name h spell)))


-- ..........................................5. Feladat ..........................................

haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast army
  | length army < 5 = map blastUnit army
  | otherwise =
    case findOptimalBlast army of
      Nothing -> army 
      Just index ->
        let (before, after) = splitAt index army
            (blast, rest) = splitAt 5 after
            blasted = map blastUnit blast
        in before ++ blasted ++ rest

findOptimalBlast :: Army -> Maybe Int
findOptimalBlast army =
  let blastDamages = map (\i -> totalDamage (take 5 (drop i army))) [0..length army - 5]
  in elemIndex (maximum blastDamages) blastDamages

totalDamage :: Army -> Integer
totalDamage = sum . map getDamage

getDamage :: Unit -> Integer
getDamage (E (Alive (Golem health))) = min 5 health
getDamage (E (Alive (HaskellElemental health))) = min 5 health
getDamage (M (Alive (Master name health spell))) = min 5 health
getDamage _ = 0

blastUnit :: Unit -> Unit
blastUnit (E (Alive (Golem health)))
  | health <= 5 = (E Dead)
  | otherwise = E (Alive (Golem (health - 5)))
blastUnit (E (Alive (HaskellElemental health)))
  | health <= 5 = (E Dead)
  | otherwise = E (Alive (HaskellElemental (health - 5)))
blastUnit (M (Alive (Master name health spell)))
  | health <= 5 = (M Dead)
  | otherwise = M (Alive (Master name (health - 5) spell))
blastUnit unit = unit

-- ..........................................6. Feladat ..........................................

multiHeal :: Health -> Army -> Army
multiHeal healAmount army
    | checkValid healAmount army = multiHeal2 0 healAmount army
    | otherwise = multiHeal3 healAmount army

healUnit :: Health -> Unit -> Unit
healUnit _ (E Dead) = E Dead
healUnit _ (M Dead) = M Dead
healUnit healAmount (M (Alive (Master name health spell))) = M (Alive (Master name (health + healAmount) spell))
healUnit healAmount (E (Alive (Golem health))) = E (Alive (Golem (health + healAmount)))
healUnit healAmount (E (Alive (HaskellElemental health))) = E (Alive (HaskellElemental (health + healAmount)))

multiHeal2 :: Health -> Health -> Army -> Army
multiHeal2 _ _ [] = []
multiHeal2 healAmount extraHeal (firstUnit:restUnit)
    | showUnit firstUnit == "Dead" = firstUnit : multiHeal2 healAmount extraHeal restUnit
    | extraHeal < 1 = healUnit healAmount firstUnit : multiHeal2 healAmount extraHeal restUnit
    | otherwise = healUnit (healAmount + 1) firstUnit : multiHeal2 healAmount (extraHeal - 1) restUnit

multiHeal3 :: Health -> Army -> Army
multiHeal3 _ [] = []
multiHeal3 healAmount army
    | aliveCount army == 0 = army
    | healAmount `mod` aliveCount army == 0 = multiHeal2 (healAmount `div` aliveCount army) 0 army
    | otherwise = multiHeal2 (healAmount `div` aliveCount army) (healAmount `mod` aliveCount army) army

aliveCount :: Army -> Integer
aliveCount [] = 0
aliveCount (firstUnit:restUnit)
    | showUnit firstUnit /= "Dead" = 1 + aliveCount restUnit
    | otherwise = aliveCount restUnit

checkValid :: Integer -> [a] -> Bool
checkValid 0 _ = True
checkValid _ [] = False
checkValid n (_:restUnit) = checkValid (n - 1) restUnit


-- ..........................................10. Feladat ..........................................


data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving Eq

instance Show OneVOne where
  show (Winner "") = "<|| Winner  ||>"
  show (You health rest) = "<You " ++ show health ++ "; " ++ innerShow rest ++ ">"
  show (HaskellMage health rest) = "<HaskellMage " ++ show health ++ "; " ++ innerShow rest ++ ">"

innerShow :: OneVOne -> String
innerShow (Winner "") = "|| Winner  ||"
innerShow (You h (Winner w)) = "You " ++ show h ++ "; || Winner " ++ w ++ " ||"
innerShow (You h r) = "You " ++ show h ++ "; " ++ innerShow' r
innerShow (HaskellMage h (Winner w)) = "HaskellMage " ++ show h ++ "; || Winner " ++ w ++ " ||>"
innerShow (HaskellMage h r) = "HaskellMage " ++ show h ++ "; " ++ innerShow' r

innerShow' :: OneVOne -> String
innerShow' (Winner "") = "|| Winner  ||"
innerShow' x = show x