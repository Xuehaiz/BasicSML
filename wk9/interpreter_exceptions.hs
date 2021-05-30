import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

instance Functor Exception where
  fmap = liftM

instance Applicative Exception where
  pure  = Prelude.return
  (<*>) = ap


data Exception a = Ok a| Error
                   deriving Show

data E = Lit Int | Div (E,E)
                   deriving Show

eval (Lit n)        = Ok n
eval (Div (e1, e2)) =
     case  (eval e1) of
     (Ok n1) ->  (case (eval e2) of
                    Ok n2 -> if n2 == 0 then Error else (Ok (n1 `div` n2))
                    Error -> Error)
     Error   ->  Error

test =  case eval (Div (Lit 1 , Lit 0)) of
             Ok x  -> x
             Error -> 0 ;
test0 = eval (Div( Div(Lit(48),Lit(2)) , Div(Lit 16, Lit 4) )) ;
test1 = eval (Div( Div(Lit(48),Lit(2)) , Div(Lit 16, Lit 0) )) ;
-----------------------------------------------------------------------------
-- Rewrite the interpreter 

return x = Ok x

bind e k = case e of
             Ok n1 -> k n1
             Error -> Error

eval_1 (Lit n)        = Main.return n 
eval_1 (Div (e1, e2)) =
       (eval_M e1) `bind`
           (\v1 -> (eval_M e2)  `bind`
                         (\v2 -> if v2 == 0 then Error else (Main.return (v1 `div` v2))))

test2 = eval_1 (Div( Div(Lit(48),Lit(2)) , Div(Lit 16, Lit 4) ))
test3 = eval_1 (Div( Div(Lit(48),Lit(0)) , Div(Lit 16, Lit 4) ))

-----------------------------------------------------------------------------
-- Interpreter in monadic style
--
-- Exception is a Monad

-- class Monad m where
--    return :: a -> m a     return :: Int -> Exception Int
--    (>>=)  :: m a -> (a -> m b) -> m b
--            (>>=) :: Exception Int -> (Int -> Exception Int) -> Exception Int
--
-- 

instance Monad Exception where
   return x = Ok x
   (>>=)  e k  = case e of
                  Ok n1 -> k n1
                  Error -> Error
--
--

eval_M (Lit n) = Prelude.return n 
eval_M (Div (e1, e2)) =
       (eval_M e1) >>=
           (\v1 -> (eval_M e2)  >>=
                         (\v2 -> if v2 == 0 then Error else (Prelude.return (v1 `div` v2))))

eval_nice (Lit n) = Prelude.return n
eval_nice (Div (e1, e2)) = do
                             v1 <- eval_nice e1 ;
                             v2 <- eval_nice e2;
                             if v2 == 0 then Error else Prelude.return (v1 `div` v2)


test4= eval_nice (Div( Div(Lit(48),Lit(2)) , Div(Lit 16, Lit 4) ))
test5 = eval_nice (Div( Div(Lit(48),Lit(0)) , Div(Lit 16, Lit 4) ))

