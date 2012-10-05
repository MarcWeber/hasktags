\begin{code}
-- to be found replayRepository
{- and much more, but that's the one wich got my attraction -}
module Darcs.Repository.Repair ( replayRepository, cleanupRepositoryReplay,
                                 RepositoryConsistency(..), CanRepair(..) )
       where
       
import Control.Monad ( when, unless )
import Data.Maybe ( catMaybes )
import Data.List ( sort )
import System.Directory ( createDirectoryIfMissing )

import Darcs.SlurpDirectory ( empty_slurpy, withSlurpy, Slurpy, SlurpMonad )
import Darcs.Lock( rm_recursive )
import Darcs.Hopefully ( PatchInfoAnd, info )

import Darcs.Ordered ( FL(..), RL(..), lengthFL, reverseFL, reverseRL, concatRL,
                     mapRL )
import Darcs.Patch.Depends ( get_patches_beyond_tag )
import Darcs.Patch.Patchy ( applyAndTryToFix )
import Darcs.Patch.Info ( human_friendly )
import Darcs.Patch ( RepoPatch, patch2patchinfo )

import Darcs.Repository.Format ( identifyRepoFormat, 
                                 RepoProperty ( HashedInventory ), format_has )
import Darcs.Repository.Cache ( Cache, HashedDir( HashedPristineDir ) )
import Darcs.Repository.HashedIO ( slurpHashedPristine, writeHashedPristine,
                                   clean_hashdir )
import Darcs.Repository.HashedRepo ( readHashedPristineRoot )
import Darcs.Repository.Checkpoint ( get_checkpoint_by_default )
import Darcs.Repository.InternalTypes ( extractCache )
import Darcs.Repository ( Repository, read_repo,
                          checkPristineAgainstSlurpy,
                          writePatchSet, makePatchLazy )

import Darcs.Sealed ( Sealed(..), unsafeUnflippedseal )
import Darcs.Progress ( debugMessage, beginTedious, endTedious, tediousSize, finishedOneIO )
import Darcs.Utils ( catchall )
import Darcs.Global ( darcsdir )
import Darcs.Flags ( compression )
import Printer ( Doc, putDocLn, text )
import Darcs.Arguments ( DarcsFlag( Verbose, Quiet ) )

run_slurpy :: Slurpy -> SlurpMonad a -> IO (Slurpy, a)
run_slurpy s f =
    case withSlurpy s f of
      Left err -> fail err
      Right x -> return x

update_slurpy :: Repository p -> Cache -> [DarcsFlag] -> Slurpy -> IO Slurpy
update_slurpy r c opts s = do
  current <- readHashedPristineRoot r
  h <- writeHashedPristine c (compression opts) s
  s' <- slurpHashedPristine c (compression opts) h
  clean_hashdir c HashedPristineDir $ catMaybes [Just h, current]
  return s'

applyAndFix :: RepoPatch p => Cache -> [DarcsFlag] -> Slurpy -> Repository p -> FL (PatchInfoAnd p) -> IO (FL (PatchInfoAnd p), Slurpy)
applyAndFix _ _ s _ NilFL = return (NilFL, s)
applyAndFix c opts s_ r psin =
    do beginTedious k
       tediousSize k $ lengthFL psin
       ps <- aaf 0 s_ psin
       endTedious k
       return ps
    where k = "Repairing patch" -- FIXME
          aaf _ s NilFL = return (NilFL, s)
          aaf i s (p:>:ps) = do
            (s', mp') <- run_slurpy s $ applyAndTryToFix p
            finishedOneIO k $ show $ human_friendly $ info p
            p' <- case mp' of
                    Nothing -> return p
                    Just (e,pp) -> do putStrLn e
                                      return pp
            p'' <- makePatchLazy r p'
            let j = if ((i::Int) + 1 < 100) then i + 1 else 0
            (ps', s'') <- aaf j s' ps
            s''' <- if j == 0 then update_slurpy r c opts s''
                      else return s''
            return ((p'':>:ps'), s''')

data RepositoryConsistency = RepositoryConsistent | RepositoryInconsistent Slurpy
data CanRepair = CanRepair | CannotRepair deriving Eq

check_uniqueness :: RepoPatch p => (Doc -> IO ()) -> (Doc -> IO ()) -> Repository p -> IO ()
check_uniqueness putVerbose putInfo repository =
    do putVerbose $ text "Checking that patch names are unique..."
       r <- read_repo repository
       case has_duplicate $ mapRL info $ concatRL r of
         Nothing -> return ()
         Just pinf -> do putInfo $ text "Error! Duplicate patch name:"
                         putInfo $ human_friendly pinf
                         fail "Duplicate patches found."

has_duplicate :: Ord a => [a] -> Maybe a
has_duplicate li = hd $ sort li
    where hd [_] = Nothing
          hd [] = Nothing
          hd (x1:x2:xs) | x1 == x2 = Just x1
                        | otherwise = hd (x2:xs)
replayRepository :: (RepoPatch p) => CanRepair -> Repository p -> [DarcsFlag] -> IO RepositoryConsistency
replayRepository canrepair repo opts = do
  let putVerbose s = when (Verbose `elem` opts) $ putDocLn s
      putInfo s = when (not $ Quiet `elem` opts) $ putDocLn s
  check_uniqueness putVerbose putInfo repo
  maybe_chk <- get_checkpoint_by_default repo
  let c = extractCache repo
  createDirectoryIfMissing False $ darcsdir ++ "/pristine.hashed"
  rooth <- writeHashedPristine c (compression opts) empty_slurpy
  s <- slurpHashedPristine c (compression opts) rooth
  putVerbose $ text "Applying patches..."
  s' <- case maybe_chk of
    Just (Sealed chk) ->
        do let chtg = patch2patchinfo chk
           putVerbose $ text "I am repairing from a checkpoint."
           patches <- read_repo repo
           (s'', _) <- run_slurpy s $ applyAndTryToFix chk
           (_, s_) <- applyAndFix c opts s'' repo
                      (reverseRL $ concatRL $ unsafeUnflippedseal $ get_patches_beyond_tag chtg patches)
           return s_
    Nothing -> do debugMessage "Fixing any broken patches..."
                  rawpatches <- read_repo repo
                  let psin = reverseRL $ concatRL rawpatches
                  (ps, s_) <- applyAndFix c opts s repo psin
                  when (canrepair == CanRepair) $ do
                       writePatchSet (reverseFL ps :<: NilRL) opts
                       return ()
                  debugMessage "Done fixing broken patches..."
                  return s_
  debugMessage "Checking pristine agains slurpy"
  is_same <- checkPristineAgainstSlurpy repo s' `catchall` return False
  if is_same
     then return RepositoryConsistent
     else return $ RepositoryInconsistent s'

cleanupRepositoryReplay :: Repository p -> IO ()
cleanupRepositoryReplay r = do
  let c = extractCache r
  rf_or_e <- identifyRepoFormat "."
  rf <- case rf_or_e of Left e -> fail e
                        Right x -> return x
  unless (format_has HashedInventory rf) $
         rm_recursive $ darcsdir ++ "/pristine.hashed" 
  when (format_has HashedInventory rf) $ do
       current <- readHashedPristineRoot r
       clean_hashdir c HashedPristineDir $ catMaybes [current]
\end{code}
