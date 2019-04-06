module C11_ADTs.Programmers where

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill 
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript deriving (Eq, Show)

data Programmer =
    Programmer { 
       os :: OperatingSystem
     , lang :: ProgLang 
    } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems = 
        [ GnuPlusLinux
        , OpenBSDPlusNevermindJustBSDStill 
        , Mac
        , Windows
        ]

allLanguages :: [ProgLang] 
allLanguages = [Haskell, Agda, Idris, PureScript] 

allProgrammers :: [Programmer]
allProgrammers = go allOperatingSystems allLanguages [] 
  where
    go [] _ acc              = acc
    go os [] acc             = go (drop 1 os) allLanguages acc
    go oss@(o:os) (l:ls) acc = go oss ls (Programmer o l : acc)
