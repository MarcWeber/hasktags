{-# LANGUAGE CPP #-}
module DebugShow (
trace_
) where

#if debug

-- enable trace messages to understand why thing get/ get not found
import Debug.Trace
trace_ :: (Show a) => String -> a -> ret -> ret
trace_ msg thing ret = trace ("\nmsg: " ++ msg ++ " " ++ (show thing) ++ "\n") ret
{-# INLINE trace_ #-}

#else

-- id function - should have no effect
trace_ :: (Show a) => String -> a -> b -> b
trace_ _ _ ret = ret
{-# INLINE trace_ #-}

#endif
