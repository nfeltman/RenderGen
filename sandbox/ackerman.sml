(** This is an example from Davies and Pfenning 2001. *)
fun acker m = 
  if m = 0 then 
    (fn n => n + 1)
  else
    (fn n => 
       if n = 0 then 
         acker (m-1) 1 
       else 
         acker (m-1) (acker m (n-1)))


(* A modal version of the function above.
 * This function will not terminate because of the recursive call to 
 * acker m when calculating ackerman m. 
 *)   
fun acker m = 
  if m = 0 then 
    box (fn n => n + 1)
  else
    box (fn n => 
           if n = 0 then 
             (unbox (acker (m-1))) 1 
           else (unbox (acker (m-1))) ((unbox (acker m)) (n-1))) 

