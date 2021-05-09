val x = ref 10;

val w = 9; 
val w = 10 ;

val z = !x ;

val w = x:= !x +1; 

let 
fun mk_counter (init : int) =   
	 let  val count = ref init 
          fun counter(inc:int) = (count := !count + inc; !count)
	 in
	      counter
	 end
val c = mk_counter(1)
val xx = c(2)
val yy = c(2)
val final_count = yy + xx
in final_count
end ;


datatype Message = GetBalance | Deposit of int | Withdraw of int ;


fun opening_account init_amt  = let
                                  val amount = ref init_amt
                                in
                                  fn msg => (case msg of
                                    Deposit amt => (amount := !amount + amt;
                                                 !amount)
                                |   Withdraw amt => (amount := !amount - amt;
                                                         !amount)
                                |  GetBalance => !amount
                                )
                         end;

val checking_account =  opening_account 100;

val t1 = checking_account (Withdraw 100);
val t2 = checking_account (Deposit 50);
val t3 = checking_account (GetBalance);
