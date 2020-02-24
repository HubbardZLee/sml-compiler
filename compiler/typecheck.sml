signature TYPECHECK =
sig
  val tc : Absyn.prog -> unit
  (* if there are errors, these are reported through ErrorMsg.error *)

  val sub: Absyn.tp * Absyn.tp -> bool
  val join: (string->unit) -> Absyn.tp * Absyn.tp -> Absyn.tp
end

structure TypeCheck :> TYPECHECK =
struct
open List

 structure A = Absyn
 structure S = Symbol
     
 fun list2string nil = ""
   | list2string [t] = t
   | list2string (h::t) = h ^ "," ^ list2string t

 fun tp2string A.Inttp = "int"
   | tp2string (A.Tupletp tps) = "<" ^ (list2string (map tp2string tps)) ^ ">"
   | tp2string (A.Arrowtp (tp1, tp2)) = tp2string tp1 ^ " -> " ^ tp2string tp2
   | tp2string (A.Reftp tp) = tp2string tp ^ " ref"
   | tp2string (_) = "Unknown type"

 type ctxt = A.tp Symbol.table

 exception UNIMPLEMENTED
 exception NO_MAIN
 exception IMPROPER_MAIN of string * string 
 exception DOUBLE_FUNCTION of string * ErrorMsg.pos2
 exception IMPROPER_RETURN_VALUE of string * string * string * string * ErrorMsg.pos2
 exception UNDEFINED_VARIABLE of string * ErrorMsg.pos2
 exception INDEX_ERROR of ErrorMsg.pos2
 exception NOT_ARRAY of ErrorMsg.pos2
 exception TYPE_ERROR of string * string * ErrorMsg.pos2
 exception NOT_FUNCTION of ErrorMsg.pos2
 exception NOT_BOOLEAN of ErrorMsg.pos2
 exception NOT_INTEGER of ErrorMsg.pos2
 exception NOT_REF of ErrorMsg.pos2
 exception CONSTRAINT_FAILED of ErrorMsg.pos2
 exception TOO_MANY_ARGUMENTS of ErrorMsg.pos2
 exception TOO_FEW_ARGUMENTS of ErrorMsg.pos2
 
(* subtyping *)
 fun sub (t': A.tp, t: A.tp): bool = 
   case (t', t) of
     (A.Inttp, A.Inttp) => true 
   | (A.Tupletp tplist', A.Tupletp tplist) => 
       if length(tplist) > length(tplist') 
       then false
       else typeSafe(tplist, tplist')
   | (A.Arrowtp (f1', f2'), A.Arrowtp (f1, f2)) => 
       if sub (f1, f1') 
       then if sub (f2', f2)
            then true
            else false
       else false
   | (A.Reftp (reftp'), A.Reftp (reftp)) => sub (reftp', reftp)
   | (_ , _) => false

 and typeSafe(list1: A.tp list, list2: A.tp list): bool =
   case (list1, list2) of
     ([], []) => true
   | ([], h::t) => true
   | (h::t, []) => false
   | (h::t, h2::t2) => if h = h2 then typeSafe(t, t2) else false
 
 fun check_sub pos (tp1, tp2) = 
   if sub (tp1, tp2) then ()
   else ErrorMsg.error (pos, tp2string(tp1) ^ "is not a sub type of " ^ tp2string(tp2))

(* subtype join *)
 fun join complain (t1,t2) : A.tp = 
   case (t1, t2) of
     (A.Inttp, A.Inttp) => A.Inttp
   | (A.Tupletp tplist1, A.Tupletp tplist2) => A.Tupletp (joinTuple complain (tplist1, tplist2))
   | (A.Arrowtp (tp1in, tp1out), A.Arrowtp (tp2in, tp2out)) =>
       A.Arrowtp (meet complain (tp1in, tp2in), join complain (tp1out, tp2out))
   | (A.Reftp rftp1, A.Reftp rftp2) =>
       A.Reftp (join complain (rftp1, rftp2))
   | (_, _) => A.Tupletp ([])
 
 and meet complain (t1, t2) : A.tp =
   case (t1, t2) of
     (A.Inttp, A.Inttp) => A.Inttp
   | (A.Tupletp tplist1, A.Tupletp tplist2) => raise UNIMPLEMENTED
   | (A.Arrowtp (tp1in, tp1out), A.Arrowtp (tp2in, tp2out)) => 
       A.Arrowtp (join complain (tp1in, tp2in), meet complain (tp1out, tp2out))
   | (A.Reftp (rftp1), A.Reftp (rftp2)) => 
       A.Reftp (meet complain (rftp1, rftp2))
   | (_, _) => A.Tupletp ([])
 
 and joinTuple complain (t1list: A.tp list, t2list: A.tp list) : A.tp list = 
   case (t1list, t2list) of
     ([], []) => []
   | ([], h2::t2) => []
   | (h::t, []) => []
   | (h::t, h2::t2) => 
       let
         val tail = (joinTuple complain (t, t2))
         val head = (join complain (h, h2))
       in head::tail
       end
       
 and meetTuple complain (t1list: A.tp list, t2list: A.tp list) : A.tp list =
   case (t1list, t2list) of
     ([], []) => []
   | ([], t2list') => t2list'
   | (t1list', []) => t1list'
   | (h::t, h2::t2) =>
       let
         val tail = (meetTuple complain (t, t2))
         val head = (meet complain (h, h2))
       in head::tail
       end
   
   
(* checks list to ensure all integers *)
 fun IsInt(tp: A.tp) = if tp = A.Inttp then true else false

(* expression typing *)
 fun tc_exp (rho: ctxt, pos: ErrorMsg.pos2, exp: A.exp) : A.tp =  
   case exp of
     A.Int n => A.Inttp
   | A.Op (oper, elist) => 
       let 
         val tlist = processElist(rho, pos, elist)
         val len = length(tlist)
         val () = if (len > 2) 
                  then raise TOO_MANY_ARGUMENTS pos
                  else if (len > 0) 
                  then ()
                  else raise TOO_FEW_ARGUMENTS pos
       in if len = 1
          then case oper of
                 A.Ref => let val h = hd tlist
                          in case h of 
                               A.Inttp => A.Reftp A.Inttp
                             | A.Tupletp tlist => A.Reftp (A.Tupletp tlist)
                             | A.Arrowtp (t1, t2) => A.Reftp (A.Arrowtp (t1, t2))
                             | A.Reftp tp => h
                             | _ => raise UNIMPLEMENTED
                          end
               | A.Get => let val h = hd tlist
                          in case h of A.Reftp tp => tp | _ => raise NOT_REF pos
                          end
               | _ => raise TOO_FEW_ARGUMENTS pos
          else case oper of
                 A.Ref => raise TOO_MANY_ARGUMENTS pos
               | A.Get => raise TOO_MANY_ARGUMENTS pos
               | A.Set => let 
                            val h = hd tlist
                            val t = hd (tl tlist)
                          in case h of 
                               A.Reftp tp => if sub (t, tp) then A.Tupletp([]) else raise TYPE_ERROR (tp2string(t), tp2string(tp), pos)
                             | _ => raise NOT_REF pos
                          end
               | _ => if all (fn x => x = A.Inttp) tlist then A.Inttp else raise NOT_INTEGER pos
       end
   | A.Tuple elist => 
       let val tlist = processElist(rho, pos, elist)
       in A.Tupletp tlist
       end
   | A.Proj(x, ex) => 
       if x < 0 then raise INDEX_ERROR pos else
       let val tp = tc_exp (rho, pos, ex)
       in
         case tp of
           A.Tupletp tplist => 
             if length(tplist) > x
             then nth (tplist, x)
             else raise INDEX_ERROR pos
         | _ => raise NOT_ARRAY pos
       end
   | A.If(ex1, ex2, ex3) => 
       let
         val tp1 = tc_exp(rho, pos, ex1)
         val tp2 = tc_exp(rho, pos, ex2)
         val tp3 = tc_exp(rho, pos, ex3)
       in if tp1 = A.Inttp
          then join pos (tp2, tp3)
          else raise NOT_BOOLEAN pos
       end
   | A.While(ex1, ex2) => 
       let 
         val ex1' = tc_exp(rho, pos, ex1)
         val ex2' = tc_exp(rho, pos, ex2)
       in if ex1' = A.Inttp
          then A.Tupletp([])
          else raise NOT_BOOLEAN pos
       end
   | A.Call(ex1, ex2) => 
       let 
         val tp1 = tc_exp (rho, pos, ex1)
         val tp2 = tc_exp (rho, pos, ex2)
       in
         case tp1 of
           A.Arrowtp (t1, t2) => 
             if sub (tp2, t1) 
             then t2
             else raise TYPE_ERROR (tp2string(tp2), tp2string(t1), pos)
         | _ => raise NOT_FUNCTION pos
       end
   | A.Let(id, ex1, ex2) =>
       tc_exp(S.enter(rho, id, tc_exp(rho, pos, ex1)), pos, ex2)
   | A.Constrain(ex, tp) => 
       if ((tc_exp (rho, pos, ex)) = tp)
       then tp
       else raise CONSTRAINT_FAILED pos
   | A.Pos(er, ex) => tc_exp(rho, pos, ex)
   | A.Id y =>
       case S.look (rho, y) of
         SOME z => z
       | NONE => raise UNDEFINED_VARIABLE (S.name y, pos)
 
 (* Converts an expression list into a type list *)
 and processElist (rho: ctxt, pos: ErrorMsg.pos2, elist: A.exp list): A.tp list =
   case elist of
     [] => []
   | h::t =>
       let 
         val tail = processElist (rho, pos, t)
         val htp = tc_exp (rho, pos, h)
       in htp::tail
       end 
         

 fun tc_fundec (rho: ctxt) ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
   let 
     val rho' = S.enter(rho, x, tp1)
     val tp = tc_exp (rho', (0,0), exp)
   in if sub (tp, tp2)
      then ()
      else raise IMPROPER_RETURN_VALUE (tp2string(tp), tp2string(tp2), S.name(f), tp2string(tp1), pos)
   end

 fun build_global_context (fundecs: A.prog): ctxt =
   case fundecs of
     [] => S.enter (S.empty, (S.symbol "printint"), A.Arrowtp(A.Inttp, A.Tupletp([])))
   | (pos,(f,x,tp1,tp2,exp))::t => 
       let val rho = build_global_context t
       in 
         case S.look(rho, f) of
           NONE => S.enter (rho, f, A.Arrowtp(tp1, tp2))
         | SOME z => raise DOUBLE_FUNCTION (S.name(f), pos)
       end

 fun tc (fundecs: A.prog): unit  = 
   let 
     val rho = build_global_context(fundecs) 
     val () = app (tc_fundec rho) fundecs
   in
     case S.look(rho, S.symbol "main") of
       NONE => raise NO_MAIN
     | SOME z => case z of
                   A.Arrowtp (A.Inttp, A.Inttp) => print "\nSuccesful Compilation!\n"
                 | A.Arrowtp (tp1, tp2) => raise IMPROPER_MAIN (tp2string(tp1), tp2string(tp2))
                 | _ => raise NO_MAIN
   end 
   
    handle 
      NO_MAIN => print ("\nYour program requires a main() function!\n")
    | IMPROPER_MAIN (t1, t2) => print ("\nMain should be: int -> int\n\tYour main is: " ^ t1 ^ " -> " ^ t2 ^ "\n")
    | DOUBLE_FUNCTION (f, pos) => ErrorMsg.error (pos, "\nFunction name: " ^ f ^ " already exists in this context.\n")
    | IMPROPER_RETURN_VALUE (st, ac, f, t1, pos) => ErrorMsg.error (pos, "\nReturn value of " ^ st ^ " does not match stated return value of " ^ ac ^ " in function:\n\t" ^ f ^ ": " ^ t1 ^ " -> " ^ ac ^ "\n")
    | UNDEFINED_VARIABLE (y, pos) => ErrorMsg.error (pos, "\nThere exists an undefined variable '" ^ y ^ "' in the program.\n")
    | NOT_FUNCTION pos => ErrorMsg.error (pos, "\nAn expression is being called as though it's a function. It is not.\n")
    | NOT_BOOLEAN pos => ErrorMsg.error (pos, "\nA non integer value is being used in conditional statement.\n")
    | NOT_REF pos => ErrorMsg.error (pos, "\nAttempting to access a non reference type.\n")
    | NOT_INTEGER pos => ErrorMsg.error (pos, "\nThis operation can only be performed on integer types.\n") 
    | TOO_FEW_ARGUMENTS pos => ErrorMsg.error (pos, "\nThis operation requires more arguments.\n")
    | TOO_MANY_ARGUMENTS pos => ErrorMsg.error (pos, "\nThis operation requires less arguments.\n")
    | TYPE_ERROR (t1, t2, pos) => ErrorMsg.error (pos, "\n" ^ t1 ^ " is not a subtype of " ^ t2 ^ ".\n")
    | INDEX_ERROR pos => ErrorMsg.error (pos, "\nIndex out of bounds.\n")
    | NOT_ARRAY pos => ErrorMsg.error (pos, "\nAttempting to access values of non array.\n")
    | CONSTRAINT_FAILED pos => ErrorMsg.error (pos, "\nType constraint has failed.\n")
    | UNIMPLEMENTED => print ("\nThis featue has not been implemented yet.\n")
    
end
