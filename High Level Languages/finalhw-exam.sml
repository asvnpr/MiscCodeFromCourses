(* Alejandro Salvador Vega Nogales
*   CCOM 4029
*   Prof. Koutis
*   Final Assignment *)

(*
 * 1: Sml is statically scoped. As we can see in this code example: 

 *)
  fun scope x =
let
 val t = x +5
   in
    t
 end;

 val test = t;
(* Calling t outside of the function returns this error: stdIn:17.12 Error: unbound variable or constructor: t *)
   

(*
  2.)
   - Being a strongly typed language means that the language does no conversion and rejects an argument when it doesn't
      match the expected value for a function's or procedure's argument(s) (i.e. operations expect certain arguments of certain data types 
      and will reject all arguments that don't exactly match the expected data type).
    - SML uses type inference to infer the types of the operand based on the operator/function being used. SML does this by looking at the data type
    of the operand and the expected data type of the arguments of the operator. 
    - No. Some declarations must be unambiguous and explicit (e.g. a declaration of a data type: 'datatype thingy = Stuff of int | Wtf of real;')
 *)

(* 3.) map function. Goes through the list until the list is empty. Once the list is empty it goes back 
      through the stack concatenates the empty list, goes back and applies the function to the previous element, concatenates, and so on...*)

    fun mapping ([], f)    = []
      | mapping (h::t, f) = f(h)::mapping(t, f);

    (* example output: mapping(fn x => x + 5, [1,2,3]);
val it = [6,7,8] : int list *)

(* 4.) *)
    fun xIndex ([], x) = [[x]]
      | xIndex (h::t, x) = (x::h::t)::mapping((xIndex(t,x), fn xs => h::xs));

(* 5.) *)

fun nPerms(1) = [[1]]
  | nPerms(n) = 
    let
      fun perm(nil) = [[]]
        | perm(h::t) = List.concat(mapping(perm(t), fn x => xIndex(x,h)))
      val flist = List.tabulate(n, fn y => y + 1)
  in
  perm(flist)
  end;

  (* 6.) *)

fun xList(0,l,x) = l
  |xList(n,l,x) = xList(n-1, l@[x], x);

fun onezero(one,zero) = xList(one,[],1)@xList(zero,[],0);

fun listPerm(nil) = [[]]
  |listPerm(h::t) = List.concat(mapping(listPerm(t), fn l => xIndex(l,h)));

(* for some reason I can't figure out and don't have time to. this function gives me some duplicate lists *)

fun binLists(0,l,len) = l@[xList(len,[],0)]
  | binLists(n,l,len) = if (n = len) then
                            binLists(n-1, l@[xList(len, [], 1)], len)
                        else
                            binLists(n-1, l@listPerm(onezero(n,len-n)), len);
(* bad fix/hack. functions to remove duplicates in lists. not very efficient and should be an unecessary step. oh well: *)

fun delete x nil = nil
| delete x (y::ys) = if (x = y) then (delete x ys) else (y::(delete x ys));

fun remDups nil = nil
| remDups (x::xs) = (x::(remDups(delete x xs)));

(* wrapper function to take only 1 argument *)

fun binWrap(n) = remDups(binLists(n, [], n));
