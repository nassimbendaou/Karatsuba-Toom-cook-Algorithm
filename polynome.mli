(** Module permettant la manipulation de polynome *)

(**  type polynome *)
type poly = (int*float) list;;

(** 
  
    Retourne le coefficient de degrée i du polynome p
    Complexité O(n) n étant la longueur de la liste p
    recursive terminal && fonction pure
*)
val coef :  poly -> int -> float

(** 
  
    Retourne la somme des polynomes p1 et p2
    Complexité O(n) n étant la longueur de la liste p1 ou p2
    recursive terminal && fonction pure
*)
val somme : poly -> poly -> poly

(** 
 
    Retourne le polynome p multiplié par le coef c
    Complexité O(n) n étant la longueur de la liste p
*)
val multCoeff : poly -> float -> poly

(** 
   
    Retourne la soustraction du polynome p1 par le polynome p2
    Complexité O(n) n étant la longueur de la liste p1
*)
val sub : poly -> poly -> poly

(** 
  
    Retourne vrai si les polynomes p1 et p2 sont égaux
    Complexité O(n) n étant la longueur de la liste p1
*)
val equal : poly -> poly -> bool

(** 
   
    Retourne le degree du polynome p
    Complexité O(n) n étant la longueur de la liste p
*)
val degre : poly -> int

(** 
  
    Retourne le polynome p multiplié par X^i
    Complexité O(n) n étant la longueur de la liste p
*)
val multXn : poly -> int -> poly

(** 
    Retourne le polynome p scindé (p0, p1) tel que p = p0 + (x^i) p1
    Complexité O(n) n étant la longueur de la liste p
*)
val cut : poly -> int -> poly * poly

(**
    Retourne le renversé d'ordre k du polynome p
    Complexité O(n) n étant la longueur de la liste p
*)
val renverse : int -> poly -> poly

(** 
  
    Retourne le reste de la division du polynome p par le monome de degre d
    Complexité O(n) n étant la longueur de la liste p
*)
val modulo : poly -> int -> poly

(** 
  
    Affiche le polynome sur la sortie standard
    Complexité O(n) n étant la longueur de la liste p
*)
val print_poly : poly -> unit

(** 
    
    Retourne l'évaluation du polynome p avec la valeur x
    Complexité O(n) n étant la longueur de la liste p
*)
val horner : poly -> float -> float

(** 
   
    Retourne un polynome généré de degre maximal deg et de coef max maxcoef
    Complexité O(n) n étant le degré maximal possible du polynome
*)
val random_poly : int -> float -> poly

(** 
  
    Retourne p1 * p2 par la méthode de karatsuba (Toom Cook 2)
    TODO
*)
val karatsuba : poly -> poly -> poly

(** 
   
    Retourne p1 * p2 par la méthode naive
*)
val mult_naive : poly -> poly -> poly

(** 
  
    Retourne p1 * p2 par la méthode Toom cook 3
*)
val toom_cook3 : poly -> poly -> poly

val toom_cook3 : poly -> poly -> poly

val scinde : poly -> poly * poly 