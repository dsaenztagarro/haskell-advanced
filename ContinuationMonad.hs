import Control.Monad

data Target = Monster
            | NPC
            deriving (Eq)

swingAxeBack value = print ("SwingAxeBack " ++ (show value))

isTargetValid :: Target -> Bool
isTargetValid target = target == Monster

sayUhOh :: IO ()
sayUhOh = print "sayUhOh!"

attack :: Target -> IO ()
attack target = if target == Monster
                   then print "Monster attacked!"
                   else print "NPC attacked!"


newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

unitAttack :: Target -> ContT () IO Target
unitAttack target = ContT $ \todo -> do
    swingAxeBack 60
    valid <- return $ isTargetValid target
    if valid
    then todo target
    else sayUhOh

-- runConT (unitAttack Monster) $ attack