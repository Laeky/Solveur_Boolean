type eb = 
  |V of int 
  |Vrai 
  |Faux 
  |AND of eb * eb 
  |OR of eb * eb 
  |XOR of eb * eb 
  |NAND of eb * eb 
  |NOT of eb;;

(*let rec solveur exp =
   match exp with
   |Vrai -> Vrai
   |Faux -> Faux
   |NOT(a)->NOT(solveur a)
   |AND(a,b)->AND((solveur a),(solveur b)) 
   |NAND(a,b)->NAND((solveur a),(solveur b)) 
   |OR(a,b)->OR((solveur a),(solveur b))
   |XOR(a,b)->XOR((solveur a),(solveur b)) 
(* not finished*) *)
               
let rec ens exp =
  match exp with 
  |Vrai -> []
  |Faux -> []
  |V(i)-> [i]
  |NOT(a)->(ens a)
  |AND(a,b)->(ens a)@(ens b)
  |NAND(a,b)->(ens a)@(ens b)
  |OR(a,b)->(ens a)@(ens b)
  |XOR(a,b)->(ens a)@(ens b) 
(* problème doublons dans la liste ,définir une fonction pour éliminer les doublons*)


let rec appartient e l1 = 
  match l1 with
  |[] -> false
  |head::tail -> (e=head) || appartient e tail;;

let rec elim_doublon l1 =
  match l1 with
  |[]->[]
  |head::tail->
      if appartient head tail then
        elim_doublon tail
      else  head::elim_doublon tail ;;  
  
let rec append l1 l2 =
  match l1 with
  |[]->l2
  |head::tail -> head :: append tail l2;;


(**let rec gen_env l1 =
   match l1 with
|[]->[]
|head::tail->[(head,true),(head,false)]
|head::tail -> gen_env head append gen_env tail;;
**)
(*Fonction permettant d'évaluer Les Variables*)


let rec eval eb=
  match eb with 
  |Vrai -> Vrai
  |Faux -> Faux
  |V(i)-> V(i)
  |NOT eb -> NOT (eval eb)
  |AND (eb1,eb2) -> AND(eval eb1,eval eb2)
  |OR (eb1,eb2) -> OR(eval eb1,eval eb2)
  |XOR (eb1,eb2) -> XOR(eval eb1,eval eb2)
  |NAND (eb1,eb2) -> NAND(eval eb1,eval eb2)
;;

let append x ll = List.map ( fun l -> x::l) ll;;

let rec generateur_aux l =
  match l with
    [] -> [[]]
  | hd::tl ->let le = generateur_aux tl in
      (append (a,FALSE) le)@(append (a,TRUE) le);; 

let generation s = gene (variable2 s) ;;

(*On va définir les différents Operateurs ? *)

(*NON*) 
(*let rec NON eb =
   match eb with
   |Vrai -> Faux
   |Faux -> Vrai 
   |NOT eb ->  NON eb
   |AND(x,y)-> OR(NON x,NON y)
   |OR(x,y)-> AND(NON x, NON y)
   |XOR(x,y)->OR(AND(NON(x),NON(y)),AND(x,y))
   |NAND(x,y)->AND(x,y);; 
*)
  (*OU*) 
(*let rec OR eb1 eb2 = 
   match eb1,eb2 with
   |eb1 = Faux,eb2 = Faux -> Faux
   |eb1 = Faux,eb2 = Vrai -> Vrai
   |eb1 = Vrai,eb2 = Faux -> Vrai
   |eb1 = Vrai,eb2 = Vrai -> Vrai;; 
(*OP_XOR*)
 let XOR eb1 eb2 = 
   match eb1,eb2 with
   |eb1 = Faux,eb2 = Faux -> Faux
   |eb1 = Faux,eb2 = Vrai -> Vrai
   |eb1 = Vrai,eb2 = Faux -> Vrai
   |eb1 = Vrai,eb2 = Vrai -> Faux;;
(*ER*)
 let AND eb1 eb2 = 
   match eb1,eb2 with
   |eb1 = Faux,eb2 = Faux -> Faux
   |eb1 = Faux,eb2 = Vrai -> Faux
   |eb1 = Vrai,eb2 = Faux -> Faux
   |eb1 = Vrai,eb2 = Vrai -> Vrai;;
(*OP_NAND*)
 let NAND eb1 eb2 = 
   match eb1,eb2 with
   |eb1 = Faux,eb2 = Faux -> Vrai
   |eb1 = Faux,eb2 = Vrai -> Vrai
   |eb1 = Vrai,eb2 = Faux -> Vraip
   |eb1 = Vrai,eb2 = Vrai -> Faux;;
 
*)


(*let formule2 = [[Vrai(OR((V 1),(V 2)))];[(V 2)(XOR((V 1),(V 3)))];[VRAI(NAND (AND((V 2),(V 3))))]] *)
let formule = NOT(V 0);;
let formule3 = AND(AND((V 1), NOT (V 2)),V 1);;
elim_doublon(ens(formule3)) ;;
