(* 'a list & 'a list -> 'a list *)
fun append (xs, ys) =
    case xs of
          [] => ys
        | x::xs' => x :: append(xs',ys)

val ok1 = append(["hi","bye"],["programming","languages"])

val ok2 = append([1,2],[4,5])

(* ''a * ''a -> string *)
fun same_thing (x,y) =
    if x=y then "yes" else "no"

(* int -> string *)
fun is_three x =
    if x=3 then "yes" else "no"