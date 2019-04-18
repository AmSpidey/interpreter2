import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe)


{-
MonadState s m => MonadState s (ReaderT r m)
MonadReader r m => MonadReader r (StateT s m)

dlatego monady SS i RR umieją odpowiadać na get, put, ask, local
NIEZALEZNIE od kolejności zagnieżdżenia monad i BEZ zadnych liftów
-}



{-
newtype Reader env a = Reader {runReader :: env -> a}

instance Monad (Reader env) where
   return x = Reader (\_ -> x)
   (Reader f) >>= g = Reader $ \x -> runReader (g (f x)) x

ask = Reader $ \x -> x

local f (Reader g) = Reader $ \x -> runReader g (f x)

runReader :: Reader env a -> env -> a
-}

{-
newtype State s a = State{runState :: s -> (a, s)}

instance Monad (State s) where
   return a = State $ \s -> (a,s)
   (State x) >>= f = State $ \s -> let (v,s') = x s in
                             runState (f v) s'

evalState :: State s a -> s -> a
execState :: State s a -> s -> s

gets :: (s -> a) -> State s a
modify :: (s -> s) -> State s ()

-}


{-

runStateT :: StateT s m a -> s -> m (a, s)

evalStateT :: Monad m => StateT s m a -> s -> m a

execStateT :: Monad m => StateT s m a -> s -> m s


-}

--
-- A syntax tree type for simple math, with variables
--
data Exp = IntE Int
         | OpE  Op Exp Exp
         | VarE String
         | LetE String Exp Exp

data Decl = VarD String Exp -- var x=e

data Stmt = S               -- skip
        | AS String Exp     -- x:= e
        | SeqS Stmt Stmt    -- S1; S2
        | IfS Exp Stmt Stmt -- if b then S1 else S2
        | WhileS Exp Stmt   -- while b do S
        | Block [Decl] Stmt -- begin [D] S end


type Op = Int -> Int -> Int

type Loc = Int

-- środowisko
type Env = M.Map String Loc

-- stan i kolejna wolna lokacja
type Mem = M.Map Loc Int
type Store = (Mem, Loc)

type SS a = StateT Store (Reader Env) a

--
-- The interpreter
--

-- rezerwacja nowej lokacji
newloc :: SS Loc
newloc = do
  (st,l) <- get
  put (st,l+1)
  return l

-- funkcja pomocnicza do zmiany pamięci
-- takie modify, które zmienia tylko część stanu
modifyMem :: (Mem -> Mem) -> SS ()
modifyMem f =
  modify (\(st,l) -> (f st,l))


-- interpretacja wyrażeń

eval :: Exp -> SS Int

eval (IntE n)       = return n

eval (OpE op e1 e2) = do
  x1 <- eval e1
  x2 <- eval e2
  return (op x1 x2)
  -- liftM2 op (eval e1) (eval e2)

eval (VarE x)       = do
  env <- ask
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  (st,_)  <- get
  return $ fromMaybe (error "undefined location") (M.lookup l st)

eval (LetE x e1 e2) = do
  v <- eval e1
  l <- newloc
  modifyMem (M.insert l v)
  local (M.insert x l) (eval e2)


---
---Exec statement
---

interpret :: Stmt -> SS ()
interpret S = return ()

-- zmienna x juz byla zadeklarowana i ma przypisaną lokację; updatujemy stan
interpret (AS x e)  = do
  env <- ask
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  w <- eval e
  modifyMem $ M.insert l w


interpret (SeqS s1 s2) = do {interpret s1;interpret s2}
                      -- interpret s1 >> interpret s2

interpret (IfS e s1 s2) = do
  w <- eval e
  if w==0 then interpret s2 else interpret s1

interpret (WhileS e s1) = do
  w <- eval e
  if w==0 then interpret S else do {interpret s1; interpret (WhileS e s1)}

interpret (Block [] s) =  interpret s

interpret (Block (VarD x e : ds) s) = do
  w <- eval e
  l <- newloc
  modifyMem (M.insert l w)
  local (M.insert x l) (interpret (Block ds s))

--
-- Run the interpreter
--
interpStmt :: Stmt -> IO ()
interpStmt s = mapM_ wypiszPare $ M.toList $ fst $
    runReader (execStateT  (interpret s) (M.empty,0)) M.empty
  where
    wypiszPare :: (Loc, Int) -> IO ()
    wypiszPare (l, i) = do {putStr $ (show l)++", "; putStrLn $ show i}


--type SS a = StateT Store (Reader Env) a

--
-- Evaluate an expression
--

evalExp :: Exp -> IO ()
evalExp e = print $ runReader (evalStateT (eval e) (M.empty,0)) M.empty

evalE = evalExp testE

--
-- A simple text expression:
--
--      let x =
--          let y = 5 + 6
--          in y / 5
--      in x * 3
--
-- ==>  6
--
testE = LetE "x" (LetE "y" (OpE (+) (IntE 5) (IntE 6))
                      (OpE div y (IntE 5)))
                (OpE (*) x (IntE 3))
    where x = VarE "x"
          y = VarE "y"


-- begin var x=3; x:=4 end
testSB1 = Block [VarD "x" (IntE 3)] (AS "x" (IntE 4))

-- begin var x=3, var y=7 end
testSB2 = Block [VarD "x" (IntE 3), VarD "y" (IntE 7)] S

-- begin var x=3; x:=testE; while x do x:=x-1 done end
testSB = Block [VarD "x" (IntE 3)] (SeqS (AS "x" testE) (WhileS (VarE "x") (AS "x" (OpE (-) (VarE "x") (IntE 1)))))

-- x:=testE; while x do x:=x-1 done
testS = (SeqS (AS "x" testE) (WhileS (VarE "x") (AS "x" (OpE (-) (VarE "x") (IntE 1)))))

-- begin var x=63; var y=17;
--       begin var x=3; x:=x+1; y:=y+x end;
--       x:=x-y
-- end
testSS = Block [VarD "x" (IntE 63), VarD "y" (IntE 17)] $
           SeqS (Block [VarD "x" (IntE 3)] $
                   SeqS (AS "x" (OpE (+) (VarE "x") (IntE 1)))
                        (AS "y" (OpE (+) (VarE "y") (VarE "x"))))
                (AS "x" (OpE (-) (VarE "x") (VarE "y")))

