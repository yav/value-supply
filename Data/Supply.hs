{-# LANGUAGE CPP #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.Supply
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <iavor.diatchki@gmail.com>
-- Stability : provisional
-- Portability: portable
--
-- The technique for generating new values is based on the paper
-- ''On Generating Unique Names''
-- by Lennart Augustsson, Mikael Rittri, and Dan Synek.

module Data.Supply
  (

  -- * Creating supplies
  Supply
  , newSupply
  , newEnumSupply
  , newNumSupply

  , newDupableSupply
  , newDupableEnumSupply
  , newDupableNumSupply

  -- * Obtaining values from supplies
  , supplyValue

  -- * Generating new supplies from old
  , modifySupply
  , split
  , split2
  , split3
  , split4
  ) where

-- NOTE: Using an IORef is thread-safe because we update it with
-- 'atomicModifyIORef'.  We need the 'atomicModifyRef' because multiple
-- threads may be evaluating different supplies that share the same
-- 'IORef' and we need to avoid race conditions.  This is the case for
-- both the normal and the dupable supplies.
import Data.IORef(newIORef,atomicModifyIORef)
import System.IO.Unsafe(unsafeInterleaveIO)

#if __GLASGOW_HASKELL__ >= 608 && __GLASGOW_HASKELL__ <= 610
import GHC.IOBase (unsafeDupableInterleaveIO)
#elif __GLASGOW_HASKELL__ >= 612
import GHC.IO (unsafeDupableInterleaveIO)
#else
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO = unsafeInterleaveIO
#endif

-- Basics ----------------------------------------------------------------------

-- | A type that can be used to generate values on demand.
data Supply a = Node a (Supply a) (Supply a)

-- | Get the value of a supply.  This function, together with
-- 'modifySupply' forms a comonad on 'Supply'.
supplyValue    :: Supply a -> a
supplyValue (Node a _ _) = a

-- | Generate an infinite list of supplies.
split          :: Supply a -> [Supply a]
split (Node _ s1 s2)  = s1 : split s2

-- | Split a supply into two different supplies.
-- The resulting supplies are different from the input supply.
split2         :: Supply a -> (Supply a, Supply a)
split2 (Node _ s1 s2) = (s1,s2)

-- | Split a supply into three different supplies.
split3         :: Supply a -> (Supply a, Supply a, Supply a)
split3 (Node _ s1 (Node _ s2 s3)) = (s1,s2,s3)

-- | Split a supply into four different supplies.
split4         :: Supply a -> (Supply a, Supply a, Supply a, Supply a)
split4 (Node _ s1 (Node _ s2 (Node _ s3 s4))) = (s1,s2,s3,s4)




instance Functor Supply where
  fmap f s = modifySupply s (f . supplyValue)


-- | Creates a new supply of values.
-- The arguments specify how to generate values:
-- the first argument is an initial value, the
-- second specifies how to generate a new value from an existing one.
{-# INLINE newSupply #-}
newSupply :: a -> (a -> a) -> IO (Supply a)
newSupply start next = gen =<< newIORef start
  where gen r = unsafeInterleaveIO
              $ do v  <- unsafeInterleaveIO (atomicModifyIORef r upd)
                   ls <- gen r
                   rs <- gen r
                   return (Node v ls rs)
        upd a = let b = next a in seq b (b, a)


-- | Create a new supply of values.
-- WARNING: This version is faster then 'newSupply' but it is not completely
-- thread safe, so use only if performance is an issue!
--
-- Rules for using the generated supplies:
--   * Supply splitting should be evaluated in a single thread.
--     For example, use "case" with 'split2' to force the splitting
--     of a supply.
--   * Different threads should work with different supplies.
--     For example, one could (strictly) split a supply, and then
--     fork new threads with the resulting supplies.
{-# INLINE newDupableSupply #-}
newDupableSupply :: a -> (a -> a) -> IO (Supply a)
newDupableSupply start next = gen =<< newIORef start
  where gen r = unsafeDupableInterleaveIO
              $ do v  <- unsafeDupableInterleaveIO (atomicModifyIORef r upd)
                   ls <- gen r
                   rs <- gen r
                   return (Node v ls rs)
        upd a = let b = next a in seq b (b, a)


-- XXX: Is the strictness of 'modifySupply' OK?

-- | Generate a new supply by systematically applying a function
-- to an existing supply.  This function, together with 'supplyValue'
-- form a comonad on 'Supply'.
modifySupply :: Supply a -> (Supply a -> b) -> Supply b
modifySupply s f = Node (f s) (modifySupply l f) (modifySupply r f)
  where Node _ l r = s

-- (Supply, supplyValue, modifySupply) form a comonad:
{-
law1 s      = [ modifySupply s supplyValue, s ]
law2 s f    = [ supplyValue (modifySupply s f), f s ]
law3 s f g  = [ (s `modifySupply` f) `modifySupply` g
              ,  s `modifySupply` \s1 -> g (s1 `modifySupply` f)
              ]
-}


{-# SPECIALIZE newEnumSupply :: IO (Supply Int) #-}
-- | A supply of values that are in the 'Enum' class.
-- The initial value is @toEnum 0@, new values are generates with 'succ'.
newEnumSupply        :: (Enum a) => IO (Supply a)
newEnumSupply         = newSupply (toEnum 0) succ

{-# SPECIALIZE newNumSupply :: IO (Supply Int) #-}
-- | A supply of values that are in the 'Num' class.
-- The initial value is 0, new values are generated by adding 1.
newNumSupply         :: (Num a) => IO (Supply a)
newNumSupply          = newSupply 0 (1+)

{-# SPECIALIZE newDupableEnumSupply :: IO (Supply Int) #-}
-- | A supply of values that are in the 'Enum' class.
-- The initial value is @toEnum 0@, new values are generates with 'succ'.
-- WARNING: See comment on 'newDupableSupply'
newDupableEnumSupply :: (Enum a) => IO (Supply a)
newDupableEnumSupply  = newDupableSupply (toEnum 0) succ

{-# SPECIALIZE newDupableNumSupply :: IO (Supply Int) #-}
-- | A supply of values that are in the 'Num' class.
-- The initial value is 0, new values are generated by adding 1.
-- WARNING: See comment on 'newDupableSupply'
newDupableNumSupply  :: (Num a) => IO (Supply a)
newDupableNumSupply   = newDupableSupply 0 (1+)



