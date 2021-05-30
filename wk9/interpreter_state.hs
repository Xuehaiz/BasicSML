
type State a  = Int -> (a,Int)


data E = Lit Int | Div (E,E)
                   deriving Show

eval' :: E -> State Int
eval' (Lit n) c = (n, c)
eval' (Div (e1, e2)) c =  let
                             (v1,c1) = eval' e1 c
                             (v2,c2) = eval' e2 c1
                          in (v1 `div` v2, c2+1)

test1 = eval'(Div(Lit 2, Lit 1)) 0
test2 = eval' (Div( Div(Lit(48),Lit(2)) , Div(Lit 16, Lit 4) ))  0



eval_mstate :: E -> State Int
eval_mstate (Lit n) = return n
eval_mstate (Div (e1, e2)) = do
                             v1 <- eval_mstate e1 ;
                             v2 <- eval_mstate e2;
                             update (+1);
                             return (v1 `div` v2)


