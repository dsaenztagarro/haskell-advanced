{-# LANGUAGE InstanceSigs, RankNTypes #-}

import Control.Monad

data Target = Monster
            | NPC
            deriving (Eq, Show)

swingAxeBack value = print ("SwingAxeBack " ++ (show value))

isTargetValid :: Target -> IO Bool
isTargetValid target = return $ target == Monster

sayUhOh :: IO ()
sayUhOh = print "sayUhOh!"

handler :: Target -> IO ()
handler target = if target == Monster
                   then print "Monster attacked!"
                   else print "NPC attacked!"

-- Example 1: ContT as Monad transformer

unitAttack :: Target -> (Target -> IO ()) -> IO ()
unitAttack target todo = do
    swingAxeBack 60
    valid <- isTargetValid target
    if valid
    then todo target
    else sayUhOh

-- ghci> unitAttack Monster handler

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

unitAttack' :: Target -> ContT () IO Target
unitAttack' target = ContT $ \todo -> do
    swingAxeBack 60
    valid <- isTargetValid target
    if valid
    then todo target
    else sayUhOh

-- ghci> runContT (unitAttack' Monster) $ handler

-- Example 2: Variable arguments

damage :: Target -> IO ()
damage t = print ("Continue#Damage " ++ show t)

swingBack :: Int -> IO ()
swingBack n = print ("Continue#SwingBack " ++ show n)

continue :: Hole -> IO ()
continue (Swing n) = swingBack n
continue (Attack t) = damage t

data Hole = Swing Int | Attack Target

unitAttack2 :: Target -> ContT () IO Hole
unitAttack2 target = ContT $ \k -> do
    k (Swing 60)
    valid <- isTargetValid target
    if valid
    then k (Attack target)
    else sayUhOh

-- ghci> runContT (unitAttack2 Monster) $ continue

-- Example 3: Kleisli Arrows

newHandler :: Int -> IO ()
newHandler n = print ("newHandler " ++ show n)

registerUnitBeingAttacked :: IO ()
registerUnitBeingAttacked = print "halfAssedCompletion#registerUnitBeingAttacked"

halfAssedCompletion :: Target -> ContT () IO Int
halfAssedCompletion target = ContT $ \todo -> do
    registerUnitBeingAttacked
    todo 40

instance Monad (ContT r m) where
    return a = ContT ($ a)

    -- (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
    -- a >>= f = ContT $ runContT a . flip (runContT . f)
    m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)


-- unitAttack' >=> halfAssedCompletion :: Target -> ContT () IO Int