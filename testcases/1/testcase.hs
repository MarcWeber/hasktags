-- to be found A.B.testcase
module A.B.testcase(module System.FilePath.Windows) where
    import asdf

-- to be found Request
-- to be found Request2
-- to be found rqBody
-- to be found rqMethod
-- to be found rqPeer
-- to be found Request3
    data Request = Request2 { rqMethod::Method,
                             rqBody    :: RqBody,
                             rqPeer    :: Host
                           }
                  | Request3
    deriving(Show,Read,Typeable)
    --  http://hackage.haskell.org/trac/ghc/ticket/1184
    -- ! Convert Bool into another monad
-- to be found boolM
    boolM False = mzero

-- to be found sadlkfj
    sadlkfj
     = 7

-- to be found onlyTheFirstOne
    onlyTheFirstOne (x:xs) = 8
    onlyTheFirstOne [] = 8
-- to be found AC
    AC a b c d e f g = 7
-- to be found abc
    abc = let a = 7
              b = 8
              in a + b
            where x = 34
                  o = 423
-- to be found BB
-- to be found AA
    AA, BB :: Int


-- to be found foo
    ad `foo` oh = 90

-- to be found X
-- to be found xyz
    class (A a) => X a where
      xyz :: dummy
-- to be found Z
-- to be found o
    class (A a) => Z a where o :: Int

-- to be found ABC
    newtype ABC = Int
-- to be found DBM
    newtype IE.ISession sess => DBM mark sess a = DBM (ReaderT sess IO a)

-- TODO 

    -- to be found =~
    (=~)   :: (Regex rho) => String -> rho -> Bool


    -- not to be found  : join
    -- to be found runGetState
    runGetState m str off =
        case unGet m (mkState str off) of
          (a, ~(S s ss newOff)) -> (a, s `join` ss, newOff)

    -- to be found SAA
    newtype Symbol = SAA String
    -- to be found value
    -- not to be found valuex
    value = reference <|> (Value `valuex` number)

-- to be found assertEqual
    assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
    assertEqual preface expected actual =
