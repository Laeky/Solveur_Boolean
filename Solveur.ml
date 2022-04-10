type eb = V of int | Vrai | Faux | AND of eb * eb | OR of eb * eb | XOR of eb * eb | NAND of eb * eb | NOT of eb;;

let rec solveur exp =
  match exp with
  |Vrai -> Vrai
  |Faux -> Faux
  |NOT(a)->NOT(solveur a)
  |AND(a,b)->AND((solveur a),(solveur b)) 
  |NAND(a,b)->NAND((solveur a),(solveur b)) 
  |OR(a,b)->OR((solveur a),(solveur b))
  |XOR(a,b)->XOR((solveur a),(solveur b)) 
(* not finished*)
               
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
(* problème doublons dans la liste pour not , and , nand , or ,xor * définir une fonction pour éliminer les doublons)

