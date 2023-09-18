(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)


(* Exercice 1 *)


(*Q1*)
let rec for_all = fun p -> fun l -> 
  match l with
  | [] -> true
  | head::tail when p head -> (for_all p tail )
  | _ -> false ;;

(*Q2*)
let rec exists = fun p -> fun l -> 
  match l with
  | [] -> false 
  | head::tail when p head -> true
  | _::tail -> (exists p tail ) ;;

(*Q3*)
let rec for_all_contiguous = fun p -> fun l -> 
  match l with 
  | [] -> true 
  | [_] -> true 
  | fst::tail when p fst (List.hd tail) -> (for_all_contiguous p tail )
  | _ -> false ;;

(*Q4*)
let is_sorted = fun l -> for_all_contiguous (fun x y -> y >= x) l ;; 



(* Exercice 2 *)


(*Q1*)
let rec insert_increasing = fun e -> fun l -> 
  match l with
  |  [] -> e::[]
  |  head::tail -> if e < head then e :: l
      else head :: insert_increasing e tail;;

(*Q2*)
let rec sort_increasing = fun l ->
  match l with
  |  [] -> []
  |  head::tail -> insert_increasing head (sort_increasing tail) ;;

(*Q3*)
let rec insert = fun comp -> fun e -> fun l -> 
  match l with
  |  [] -> e::[]
  |  head::tail -> if comp e head then e::l
      else head::insert comp e tail;;

let rec sort comp = function
  |  [] -> []
  |  head::tail -> insert comp head (sort comp tail) ;;

let tests = [ insert (>) 1 []; insert (>) 1 [2];
              insert (>) 3 [2]; insert (>) 1 [3;2];
              insert (>) 1 [2;0]; insert (>) 3 [2;1]
            ] = [ [1]; [2;1]; [3; 2]; [3;2;1]; [2;1;0]; [3;2;1] ];;
sort (>=) [1;2;3;4];;
sort (fun (x,y) (x',y') -> y < y') [(1,6);(0,4);(3,6);(2,0)];;


























