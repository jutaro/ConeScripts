
module  PeopleZoo (spawnPerson) where

import  ConeServer.ConeTypes    (ConeEntry(..), emptyLeaf)
import  System.Random           (randomRIO)
import  Data.Text               (append, pack)
import  Data.Bool               (bool)
import  Data.IORef
import  System.IO.Unsafe        (unsafePerformIO)



randomElem :: [a] -> IO (a, [a])
randomElem []   = error "randomElem: empty list"
randomElem [x]  = return (x, [])
randomElem xs   = do
    ix <- randomRIO (0, length xs - 1)
    let (pre, p:post) = splitAt ix xs
    return (p, pre++post)

femaleNames =
    [ "Patricia", "Linda", "Barbara", "Liz", "Jenny"
    , "Maria", "Susan", "Margaret", "Dorothy", "Lisa"
    , "Ellen", "Laura", "Kimberley", "Megan"
    , "Nancy", "Karen", "Ayla"
    ]

maleNames =
    [ "Jim", "John", "Robert", "William"
    , "David", "Richard", "Charles", "Joseph"
    , "Jürgen", "Govinda", "Yaw", "Michael", "Juri"
    , "Tom", "Chris", "Daniel", "Paul", "Ron"
    ]

familyNames_ =
    [ "Cho", "Nakamura", "McDouglas", "Swanson", "Hernandez", "Martin"
    , "Taylor", "DeBaldo", "Robinson", "Walker", "Garcia", "Hall"
    , "Young", "Allen", "Campbell", "Turner", "Roberts", "Murphy"
    , "Cruz", "Schultz", "Evans", "Torres", "Kelly", "Brooks", "Freeman"
    , "Olson", "Romero", "Carmack", "Patel", "Andrews", "Harper", "Bishop"
    , "Goldstein", "Lambert"
    ]

familyNames :: IORef [String]
familyNames = unsafePerformIO $ newIORef familyNames_
{-# NOINLINE familyNames #-}


spawnPerson :: Int -> IO ConeEntry
spawnPerson level = do
    isMale      <- (== 0) <$> randomRIO (0 :: Int, 1)
    (name, _)   <- randomElem $ if isMale then maleNames else femaleNames

    fNs         <- readIORef familyNames
    (fN, fNs')  <- randomElem $ if null fNs then familyNames_ else fNs
    writeIORef  familyNames fNs'

    let
        t   = pack $ name ++ " " ++ fN
        i   = pack $ concat [bool "female" "male" isMale, show level, ".png"]

    return emptyLeaf
        { ceLabel       = t
        , ceTextId      = pack "tId_" `append` t
        , ceIconName    = bool Nothing (Just i) (level >= 0)
        }
