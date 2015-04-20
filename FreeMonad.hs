import Control.Monad

data Toy b next = Output b next
                | Bell next
                | Done

-- *Main> :type Output 'A' Done
-- Output 'A' Done :: Toy Char (Toy b next)
-- *Main> :type Bell (Output 'A' Done)
-- Bell (Output 'A' Done) :: Toy b (Toy Char (Toy b1 next))

-- data Fix f = Fix (f (Fix f))

-- *Main> :type Fix (Output 'A' (Fix Done))
-- Fix (Output 'A' (Fix Done)) :: Fix (Toy Char)
-- *Main> :type Fix (Bell (Fix (Output 'A' (Fix Done))))
-- Fix (Bell (Fix (Output 'A' (Fix Done)))) :: Fix (Toy Char)

data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (flip catch f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell next) = Bell (f next)
    fmap f Done = Done

data IncompleteException = IncompleteException

-- subroutine = Fix (Output 'A' (Throw IncompleteException))

-- program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)))

-- FixE already exists and it is called the Free Monad

data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Monad (Free f) where
    return = Pure
    -- (Free x) >>= f = Free (fmap (\m -> m >>= f) x)
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r

-- output :: a -> Free (Toy a) ()
-- output x = Free (Output x (Pure ()))
--
-- bell :: Free (Toy a) ()
-- bell = Free (Bell (Pure ()))
--
-- done :: Free (Toy a) r
-- done = Free Done

liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

output x = liftF (Output x ())
bell = liftF (Bell ())
done = liftF Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
    subroutine
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) = "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) = "bell\n" ++ showProgram x
showProgram (Free Done) = "done\n"
showProgram (Pure r) = "return " ++ show r ++ "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showProgram

-- >>> pretty (output 'A')
-- output 'A'
-- return ()
--
-- >>> pretty (return 'A' >>= output)
-- output 'A'
-- return ()
--
-- >>> pretty (output 'A' >>= return)
-- output 'A'
-- return ()
--
-- >>> pretty ((output 'A' >> done) >> output 'C')
-- output 'A'
-- done
--
-- >>> pretty (output 'A' >> (done >> output 'C'))
-- output 'A'
-- done

-- Concurrency

data Thread m r = Atomic (m (Thread m r)) | Return r

atomic :: (Monad m) => m a -> Thread m a
atomic m = Atomic $ liftM Return m

instance (Monad m) => Monad (Thread m) where
    return = Return
    (Atomic m) >>= f = Atomic (liftM (>>= f) m)
    (Return r) >>= f = f r

thread1 :: Thread IO ()
thread1 = do
    atomic $ print 1
    atomic $ print 2

thread2 :: Thread IO ()
thread2 = do
    str <- atomic $ getLine
    atomic $ putStrLn str

interleave :: (Monad m) => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
    next1 <- atomic m1
    next2 <- atomic m2
    interleave next1 next2
interleave t1 (Return _) = t1
interleave (Return _) t2 = t2

runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r

-- runThread (interleave thread1 thread2)