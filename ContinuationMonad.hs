import Control.Monad

data Target = Monster
            | NPC
            deriving (Eq)

swingAxeBack value = print ("SwingAxeBack " ++ (show value))

isTargetValid :: Target -> IO Bool
isTargetValid target = return $ target == Monster

sayUhOh :: IO ()
sayUhOh = print "sayUhOh!"

attack :: Target -> IO ()
attack target = if target == Monster
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

-- ghci> unitAttack Monster attack

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

unitAttack''' :: Target -> ContT () IO Target
unitAttack''' target = ContT $ \todo -> do
    swingAxeBack 60
    valid <- isTargetValid target
    if valid
    then todo target
    else sayUhOh

-- ghci> runContT (unitAttack Monster) $ attack

-- Example 2: Variable arguments

data Hole = Swing Int | Attack Target

unitAttack2 :: Target -> ContT () IO Hole
unitAttack2 target = ContT $ \k -> do
    k (Swing 60)
    valid <- isTargetValid target
    if valid
    then k (Attack target)
    else sayUhOh