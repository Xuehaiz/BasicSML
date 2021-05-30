abstract class exp {};        /* A datatype declaration */
  
class num extends exp {       /* num is a variant of exp */
  int n;
  num(int n){this.n = n;}
}

class add extends exp{           /* add is a variant of exp */
   exp l, r;
  add(exp l, exp r){ this.l = l;this.r=r;}
}

class evaluator {

static int eval(exp e){
  if  (e instanceof num) return (((num) e).n);
  else
   if (e instanceof add) return ((eval (((add) e).l) + eval (((add) e).r)));
  else return 0;
}
}

class use_evaluator {

public static void main(String[] args){          /* arrays of strings objects*/
exp test1 = new add (new num(10),new num(10));   /* 10+10 */
int result = evaluator.eval (test1);
System.out.println(result);                      /* it prints 20 */
}
                                                                                            
