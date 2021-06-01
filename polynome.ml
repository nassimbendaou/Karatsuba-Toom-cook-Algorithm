type monome = (int*float);;
(**type polynome *)
type poly = monome list;;

(**coef p i Retourne le coefficient de degrée i 
du polynome p Complexité O(n) n étant la longueur 
de la liste p recursive terminal && fonction pure *)

let  rec coef (p:poly) (i:int) : float= match p with
  | [] -> 0.
  | (d,_)::_ when d > i -> 0.
  | (d,c)::_ when i = d -> c
  | _::lst -> (coef [@tailcall]) lst i;;

(**somme p1  p2 Retourne la somme des polynomes p1 et p2
 Complexité O(n) n étant la longueur 
 de la liste p1 ou p2 recursive terminal && fonction pure *)

let rec somme (p1:poly) (p2:poly) : poly = 
  let rec aux p1 p2 (r:poly) = match p1,p2 with
    | [],[] -> (List.rev r)
    | lst1, [] -> List.rev_append r lst1
    | [], lst2 -> List.rev_append r lst2
    | (d1,c1)::lst1, (d2,_)::_ when d1 < d2 -> aux lst1 p2 ((d1,c1)::r)
    | (d1,c1)::lst1, (d2,c2)::lst2 when d1 = d2 -> 
        (aux [@tailcall]) lst1 lst2 (
          begin 
            let p = c1+.c2 in
            if (p = 0.) then r else ((d1,p)::r)
          end)
    | _, (d2,c2)::lst2 -> (aux [@tailcall]) p1 lst2 ((d2,c2)::r)
  in aux p1 p2 []
;;


(**multCoeff p c Retourne le polynome p multiplié par le coef c
 Complexité O(n) n étant la longueur de la liste p *)
let multCoeff (p:poly) (a:float) : poly = match a with
  | 0. -> []
  | _ -> List.map (fun (d,c) ->(d, c*.a)) p
;;



(**sub p1 p2 Retourne la soustraction du polynome p1 par le polynome p2
 Complexité O(n) n étant la longueur de la liste p1*)
let sub (p1:poly) (p2:poly) : poly = 
  somme p1  (multCoeff p2  (-1.))
;;

(**equal p1  p2 
Retourne vrai si les polynomes p1 et p2 sont égaux Complexité O(n)
 n étant la longueur de la liste p1*)

let rec equal p1 p2 =
  match p1, p2 with
  | [],[] -> true
  | (a1, k1)::q1, (a2, k2)::q2 -> k1 = k2 && a1 = a2 && equal q1 q2
  | _ -> false;;

(**degre p Retourne le degree du polynome p Complexité O(n) n étant la longueur de la liste p*)

let degre (p:poly) : int = match p with
  | [] -> (-1)
  | _ -> List.rev p |> List.hd |> fst
;;
(**multXn p i Retourne le polynome p multiplié par X^i Complexité O(n) n étant la longueur de la liste p*)

let multXn (p:poly) (deg:int) : poly =
  List.map (fun (d,c) -> (d+deg,c)) p
;;


(**cut p i Retourne le polynome p scindé (p0, p1) tel que p = p0 + (x^i) p1 Complexité O(n) n étant la longueur de la liste p*)

let cut (p:poly) (deg:int) = 
  match List.partition (fun (d,_) -> d < deg) p with
  | p0,p1 -> p0 ,multXn p1 (-deg)
;;
(**renverse k p Retourne le renversé d'ordre k du polynome p Complexité O(n) n étant la longueur de la liste p*)
let renverse (order:int) (p:poly) = 
  if (degre p) <= order then 
    List.rev_map (fun (d,c) -> (order-d,c)) p
  else 
    failwith "(deg p) > order"
;;
(*modulo p d Retourne le reste de la division du polynome p par le monome de degre d Complexité O(n) n étant la longueur de la liste p*)
let modulo (p:poly) (deg:int) : poly =
  List.map (fun (d,c) -> (d-deg,c)) p
;;

let mono_to_string (m:monome) : string = match m with
  | (d,c) -> (if (c >= 0.) then ("+") else "")^
             (string_of_int (int_of_float c)) ^ 
             (if (d = 0) then "" else "x^" ^ (string_of_int d) ^ " ")
;;
(**print_poly p Affiche le polynome sur la sortie standard Complexité O(n) n étant la longueur de la liste p *)
let print_poly (p:poly) : unit = 
  let rec aux (p:poly) (s:string) = match p with
    | [] -> (print_string (s^"\n"))
    | (d,c)::lst -> aux lst ((mono_to_string (d,c))^s)
  in aux p ""
;;
(**horner p x Retourne l'évaluation du polynome p avec la valeur x Complexité O(n) n étant la longueur de la liste p *)
let horner (p:poly) (x:float) : float = 
  let rec aux (acc:float) (pos:int) (p:poly) = match p with 
    | [] -> acc
    | (d,c)::lst when d = pos -> c +. (x *. (aux acc (pos+1) lst))
    | _  -> (coef p pos) +. (x *. (aux acc (pos+1) p))
  in aux 0. 0 p
;;
(**random_poly deg coefmax Retourne un polynome généré de degre maximal deg et de coef max maxcoef Complexité O(n) n étant le degré maximal possible du polynome *)
let random_poly (deg:int) (maxcoef:float) : poly = 
  let rec aux (r:poly) (pos:int) = match pos with
    | _ when pos = deg -> (List.rev r)
    | _ -> 
        let has_coef = Random.bool () in
        let coef = Random.float maxcoef in
        if (has_coef) then 
          aux ((pos, coef)::r) (pos + 1)
        else 
          aux r (pos + 1)
  in aux [] 0
;;
(**mult_naive p1 p2 Retourne p1 * p2 par la méthode naive *)
let rec mult_naive (p1:poly) (p2:poly) : poly = match p1 with
  | [] -> []
  | (d,c)::[] -> List.map (fun (a,b) -> (a+d,b*.c) ) p2
  | e1::lst -> somme (mult_naive [e1] p2)  (mult_naive lst p2)
;;
(**karatsuba p1 p2 Retourne p1 * p2 par la méthode de karatsuba *)
let karatsuba = 
  let rec karatsuba_aux (p1:poly) (p2:poly) : poly =  match p1, p2 with
    |([], _) | (_, []) -> []
    |([(0, 0.)], _) | (_ ,[(0, 0.)]) -> [(0, 0.)]
    |([(0, b)], p)  |(p, [(0, b)]) -> multCoeff p  b
    | _ -> 
        let k = (max (degre p1) (degre p2)) in
        let k = k + (k mod 2) in
        let mk = (k/2) in
        let p1_0,p1_1 = cut p1 mk in
        let p2_0,p2_1 = cut p2 mk in
        let c0 = karatsuba_aux p1_0  p2_0 in
        let c2 =karatsuba_aux  p1_1  p2_1 in
        let sum_I= somme p1_0  p1_1 in
        let sum_II= somme p2_0  p2_1 in
        let u = karatsuba_aux sum_I  sum_II in
        let sub_u_c0 = sub u  c0 in
        let c1 =  sub sub_u_c0  c2 in
        let mul_c1_mk = multXn c1  mk in
        let mul_c2_k = multXn c2  k in
        let sum_1 = somme mul_c1_mk mul_c2_k in
        somme c0  sum_1
  in karatsuba_aux 
;;
(**toom_cook  *)

let rec toom_cook (p1:poly) (p2:poly) (alpha:float) = match p1, p2 with
  |([], _) | (_, []) -> []
  |([(0, 0.)], _) | (_ ,[(0, 0.)]) -> [(0, 0.)]
  |([(0, b)], p)  | (p, [(0, b)]) -> (multCoeff p  b)
  | _ ->
      let k = (max (degre p1) (degre p2)) in
      let k = k + 3 - (k mod 3) in
      let mk = (k/3) in
      let p1_0, p_temp = cut p1 mk in
      let p1_1, p1_2 = cut p_temp mk in
      let p2_0, q_temp = cut p2 mk in
      let p2_1, p2_2 = cut q_temp mk in
      let n = degre p1_0 + 1 in
      let r0 = toom_cook p1_0 p2_0 alpha in 
      let sum_r0_1 = somme p1_0  p1_1 in
      let sum_r0_2 = somme p2_0 p2_1 in
      let sum_r0_3 = somme sum_r0_1 p1_2 in
      let sum_r0_4 = somme sum_r0_2 p2_2 in 
      let r1 = toom_cook sum_r0_3 sum_r0_4  alpha in
      let sum_r1_1 = somme p1_0 p1_2  in
      let mul_r1_1 = multCoeff p1_1  (-1.) in
      let sum_r1_2 = somme sum_r1_1 mul_r1_1 in
      let sum_r1_3 = somme p2_0  p2_2  in
      let mul_r1_2 = multCoeff p2_1 (-1.) in
      let sum_r1_4 = somme sum_r1_3  mul_r1_2  in
      let r2 = toom_cook sum_r1_2 sum_r1_4 alpha in
      let mul_r2_1 = multCoeff p1_1  alpha in
      let sum_r2_1 = somme p1_0 mul_r2_1 in
      let mul_r2_2 = multCoeff p1_2  (alpha*.alpha) in
      let sum_r2_2 = somme sum_r2_1 mul_r2_2 in
      let mul_r2_3 = multCoeff p2_1  alpha in
      let sum_r2_3 = somme p2_0 mul_r2_3 in
      let mul_r2_4 = multCoeff p2_2 (alpha*.alpha)in
      let sum_r2_4 = somme sum_r2_3  mul_r2_4 in
      let r3 = toom_cook sum_r2_2 sum_r2_4 alpha in
      let r4 = toom_cook p1_2 p2_2 alpha in
      let res0 = r0 in
      let sum1 = somme (multCoeff (multCoeff r0 (-1.)) (1./.alpha)) (multCoeff (multCoeff r1 (1./.2.))  (alpha /. (alpha -. 1.))) in
      let sum2 = somme (multCoeff r4 alpha) (multCoeff(multCoeff (multCoeff r2  (1./.2.)) (alpha /. (alpha +. 1.)))  (-1.)) in
      let sum3 = somme sum1 sum2 in
      let res1 = somme sum3 (multCoeff(multCoeff r3  (1. /. (alpha *. ((alpha *. alpha) -. 1.)))) (-1.)) in
      let sum1_1 = somme (multCoeff r0  (-1.))  (multCoeff r4 (-1.)) in
      let sum_res2_1 = somme r1  r2 in
      let mul_res2_1 = multCoeff sum_res2_1 (1./.2.) in
      let res2 = somme sum1_1  mul_res2_1 in
      let mul_res3_1 = multCoeff r0 (1./.alpha) in 
      let mul_res3_2 = multCoeff(multCoeff r1  (1./.(2. *. (alpha -. 1.))))  (-1.) in 
      let sum3_1 = somme mul_res3_1 mul_res3_2 in 
      let sum3_2 = somme (multCoeff(multCoeff r4  alpha) (-1.))  (multCoeff(multCoeff r2 (1./.(2. *. (alpha +. 1.)))) (-1.)) in
      let sum3_3 = somme sum3_1 sum3_2 in
      let res3 =  somme sum3_3 (multCoeff r3 (1. /. (alpha *. ((alpha *. alpha) -. 1.)))) in
      let res4 = r4 in
      let mul1 = multXn res1  n in
      let mul2 = multXn res2 (2*n) in 
      let mul3 = multXn res3 (3*n) in 
      let mul4 = multXn res4 (4*n) in
      let sumR1R2 = somme mul1 mul2 in
      let sumR3R4 = somme mul3 mul4 in 
      let sumR1R2R3R4 = somme sumR1R2 sumR3R4 in
      somme res0 sumR1R2R3R4
;;

(**toom_cook p1 p2 Retourne p1 * p2 par la méthode Toom cook 3 *)
let toom_cook3 p1 p2 = toom_cook p1 p2 (1.);;
(**scinde p Retourne p1  p2 par la méthode scinde*)
let rec scinde p =  
  match p with
  | [] -> [],[]
  | x::[] -> [x],[] (*Car la taille de la première liste est toujours >= celle de la seconde, on n'arrivera jamais au cas []::y *)
  | x::y::q ->
      let (a, b) = scinde q in
      x::a, y::b;;

          
scinde[(0,0.1); (1,1.1); (2,2.2)] ;;