type eb = 
  |V of int 
  |Vrai 
  |Faux 
  |AND of eb * eb 
  |OR of eb * eb 
  |XOR of eb * eb 
  |NAND of eb * eb 
  |NOT of eb;;
(* prend une expression booléene en entrée et renvoie une liste de tout les variables ( y compris les doublons)*)
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


(*Fonction auxilière qu'on va utliser dans la fonction élim_doublon,
permet de savoir si un element e appartient à la liste l*)
let rec appartient e l = 
  match l with
  |[] -> false
  |head::tail -> (e=head) || appartient e tail;;

(* définition d'une fonction pour éliminer les doublons, prend une liste l 
et renvoie une liste sans doublons*)
let rec elim_doublon l =
  match l with
  |[]->[]
  |head::tail->
      if appartient head tail then
        elim_doublon tail
      else  head::elim_doublon tail ;;  




(* fonction auxilière append pour notre generateur d'environnements*)
let append x ll = List.map ( fun l -> x::l) ll;;
(* à commenter*)
let rec generateur_aux l =
  match l with
    [] -> [[]]
  | hd::tl ->let le = generateur_aux tl in
      (append (hd,Faux) le)@(append (hd,Vrai) le);; 

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
  |V(i)-> V(i)  (*redef*)
  |NOT eb -> NOT (eval eb)
  |AND (eb1,eb2) -> AND(eval eb1,eval eb2)
  |OR (eb1,eb2) -> OR(eval eb1,eval eb2)
  |XOR (eb1,eb2) -> XOR(eval eb1,eval eb2)
  |NAND (eb1,eb2) -> NAND(eval eb1,eval eb2) 
;;
(*
  let rec eval lc(var,tf)=
    match lc with
      [[]]->
*)
(*On va définir les différents Operateurs ? -> inutile *)

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
(*ET*)
  
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
generateur_aux(elim_doublon(ens(formule3)));;
