/* To solve the problem we had with the previous
   interpreter we move each clause of the interpreter
   in the appropiate class.
*/
abstract class exp {
        abstract int eval();   /* All abstract classes must have this
                                  method
                               */
                  };

class num extends exp {
  int n;
  num(int n){this.n = n;}
  int eval(){return n;}
}

class add extends exp{
   exp l, r;
   add(exp l, exp r){ this.l = l;this.r=r;}
   int eval(){return (l.eval()+r.eval());}
}


class numadd {
public static void main(String[] args){
exp test1 = new add (new num(2),new num(3));
int result = test1.eval();
System.out.println(result);
}
}

/*  We can now add a new variant without changing
    what we have done so far
*/

class mul extends exp{
   exp l, r;
   mul(exp l, exp r){ this.l = l;this.r=r;}
   int eval(){return (l.eval()* r.eval());}
}


/* This solutions take care of the problem of adding new
   variants.  However, the problem now becomes adding new
   operations, such as, a type checker.
*/
