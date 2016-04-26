(* * Polynomials
 * Use [Var] and [Ring] types to define [MakePoly] functor.
 * Also define [IntRing]. *)

(* ** Imports *)
open PolyInterfaces
open Big_int

(* ** Utility functions
 * ----------------------------------------------------------------------- *)

let pp_string fmt s = Format.fprintf fmt "%s" s

let rec pp_list sep pp_elt f l =
  match l with
  | [] -> ()
  | [e] -> pp_elt f e
  | e::l -> Format.fprintf f "%a%(%)%a" pp_elt e sep (pp_list sep pp_elt) l

let equal_pair eq1 eq2 (x1,x2) (y1,y2) =
  eq1 x1 y1 && eq2 x2 y2

let compare_pair cmp1 cmp2 (x1,x2) (y1,y2) =
  let r1 = cmp1 x1 y1 in
  if r1 <> 0 then r1
  else cmp2 x2 y2

let equal_list eq xs0 ys0 =
  let rec go xs ys =
    match xs,ys with
    | [], []       -> true
    | x::xs, y::ys -> eq x y && go xs ys
    | _            -> assert false
  in
  (List.length xs0 = List.length ys0) && go xs0 ys0

let compare_list cmp xs0 ys0 =
  let rec go xs ys =
    match xs, ys with
    | [], []       -> 0
    | x::xs, y::ys ->
      let d = cmp x y in
      if d <> 0 then d
      else go xs ys
    | _            -> assert false
  in
  let d = List.length xs0 - List.length ys0 in
  if d > 0 then 1
  else if d < 0 then -1
  else go xs0 ys0

let conc_map f xs =
  List.concat (List.map f xs)

let cat_Some l =
  let rec go acc = function
    | Some(x)::xs  -> go (x::acc) xs
    | None::xs     -> go acc      xs
    | []           -> List.rev acc
  in
  go [] l

(* ** [Ring] instance for [int]
 * ----------------------------------------------------------------------- *)

module IntRing = struct
  type t = big_int
  let pp fmt i = Format.fprintf fmt "%s" (string_of_big_int i)

  let add  = add_big_int
  let opp  = minus_big_int
  let mult = mult_big_int
  let one  = unit_big_int
  let zero = zero_big_int
  let rec ring_exp m n =
    if n > 0 then mult m (ring_exp m (n-1))
    else if n = 0 then one
    else failwith "Negative exponent in IntRing"
  let ladd cs = List.fold_left (fun acc c -> add c acc) zero cs
  let from_int i = big_int_of_int i
  let equal = eq_big_int
  let compare = compare_big_int
  let use_parens = false
end

(* ** Functor for Polynomials
 * ----------------------------------------------------------------------- *)

module MakePoly (V : Var) (C : Ring) = struct
  type coeff = C.t
  type var   = V.t

  (** We represent polynomials as assoc lists from
      monomials to coefficents. See [norm] for invariants
      that we maintain. *)
  type monom = (V.t * int) list

  type term = monom * C.t

  type t = term list

(* *** Equality and comparison
 * ----------------------------------------------------------------------- *)

  let vexp_equal = equal_pair V.equal (=)

  let vexp_compare = compare_pair V.compare compare

  let mon_equal = equal_list vexp_equal

  let mon_compare = compare_list vexp_compare

  let equal =
    equal_list (fun (m1,c1) (m2,c2) -> C.equal c1 c2 && mon_equal m1 m2)

  let term_compare (m1,c1) (m2,c2) =
    let cmp = C.compare c1 c2 in
    if cmp <> 0 then - cmp else mon_compare m1 m2

  let compare = compare_list term_compare

(* *** Pretty printing
 * ----------------------------------------------------------------------- *)

  let pp_vpow fmt (v,e) =
    if e = 1 then V.pp fmt v
    else Format.fprintf fmt "%a^%i" V.pp v e

  let pp_monom fmt m =
    match m with
    | [] -> Format.fprintf fmt "1"
    | _  -> Format.fprintf fmt "%a" (pp_list "*" pp_vpow) m

  let pp_term fmt (m,c) =
    if m = [] then Format.fprintf fmt "%a" C.pp c
    else if C.equal c C.one then pp_monom fmt m
    else if C.use_parens then Format.fprintf fmt "[%a]*%a" C.pp c pp_monom m
    else Format.fprintf fmt "%a*%a" C.pp c pp_monom m

  let pp_ break fmt f =
    let f = List.sort term_compare f in
    let rec go fmt ts = match ts with
      | [] -> Format.fprintf fmt ""
      | (m,c)::ts when C.compare c C.zero < 0->
        Format.fprintf fmt " %s- %a%a" (if break then "\n" else "") pp_term (m,C.opp c) go ts
      | t::ts ->
        Format.fprintf fmt " %s+ %a%a" (if break then "\n" else "") pp_term t go ts
    in
    match f with
    | []     -> Format.fprintf fmt "0"
    | t::ts  ->
      Format.fprintf fmt "%a%a" pp_term t go ts

  let pp = pp_ false

  let pp_break = pp_ true

  let pp_coeff = C.pp

(* *** Internal functions
 * ----------------------------------------------------------------------- *)

  let norm_monom (ves : (V.t * int) list) =
    let cmp_var (v1,_) (v2,_) = V.compare v1 v2 in
    let equal_var (v1,_) (v2,_) = V.equal v1 v2 in
    List.sort cmp_var ves
    |> BatList.group_consecutive equal_var
    |> List.map (fun ves -> (fst (List.hd ves), BatList.sum (List.map snd ves)))
    |> List.filter (fun (_,e) -> e <> 0)
    |> List.sort vexp_compare

  (** The [norm] function ensures that:
      \begin{itemize}
      \item Vexp entries
      \item Each monomial is sorted.
      \item Each monomial with non-zero coefficient has exactly one entry.
      \item The list is sorted by the monomials (keys).
      \end{itemize} *)
  let norm (f : t) =
    f |> List.map (fun (m,c) -> (norm_monom m,c))
      |> List.sort (fun (m1,_) (m2,_) -> mon_compare m1 m2)
      |> BatList.group_consecutive  (fun (m1,_) (m2,_) -> mon_equal m1 m2)
      |> List.map (fun ys -> (fst (List.hd ys), C.ladd (List.map snd ys)))
      |> List.filter (fun (_,c) -> not (C.equal c C.zero))

  let mult_term_poly_int (m,c) f =
    List.map (fun (m',c') -> (m @ m', C.mult c c')) f

(* *** Ring operations on polynomials
 * ----------------------------------------------------------------------- *)

  let one  = [([], C.one)]

  let add f g = norm (f @ g)

  (** No [norm] required since the keys (monomials) are unchanged. *)
  let opp f = List.map (fun (m,c) -> (m,C.opp c)) f

  let mult f g =
    if equal f one then g else if equal g one then f
    else f |> conc_map (fun t -> mult_term_poly_int t g) |> norm

  let minus f g = add f (opp g)

  let zero : t = []

  let var_exp v n = [([(v,n)],C.one)]

  let rec ring_exp f n =
    if n > 0 then mult f (ring_exp f (n-1))
    else if n = 0 then one
    else failwith "Negative exponent in polynomial"

  let var v = [([(v,1)],C.one)]

  let const c = if (C.equal c C.zero) then [] else [([],c)]

  let from_int i = const (C.from_int i)

  let lmult = List.fold_left (fun acc f -> mult acc f) one

  let ladd  = List.fold_left (fun acc f -> add acc f) zero

  let vars f =
    List.sort_uniq V.compare
      (conc_map (fun (m,_) -> List.sort_uniq V.compare (List.map fst m)) f)

  let partition p f =
    let (t1s, t2s) = List.partition p f in
    (norm t1s, norm t2s)

  let inst_var env (v,e) =
    match e < 0, env v with
    | true, _ ->
      failwith "impossible: variables with negative exponent"
    | false, f ->
      ring_exp f e

  let eval env f =
    let eval_monom m = lmult (List.map (inst_var env) m) in
    let eval_term (m,c) = mult (const c) (eval_monom m) in
    ladd (List.map eval_term f)

  let eval_generic cconv vconv terms =
    let vars_to_poly ves = lmult (List.map (inst_var vconv) ves) in
    ladd (List.map (fun (ves, c) ->  mult (vars_to_poly ves) (cconv c)) terms)

  let to_terms f = f

  let lc f = match f with (* FIXME: fix name tc (tail coeff) *)
    | [] -> C.zero
    | x::_ -> snd x (* (Util.last f) *)

  let from_terms f = norm f

  let from_mon m = from_terms [(m, C.one)]

  let is_const = function ([([],_)] | []) -> true | _ -> false

  let is_var = function [([_x],c)] when C.equal c C.one -> true | _ -> false

  let mons (f : t) = List.sort_uniq (compare_list vexp_compare) (List.map fst f)
  let coeff f m = try List.assoc m f with Not_found -> C.zero

  let map_coeffs cf f =
    cat_Some
      (List.map (fun (m,c) -> let c = cf c in if C.equal c C.zero then None else Some (m,c)) f)

  let ( *@) = mult
  let ( +@) = add
  let ( -@) = minus

end


(* ** Module of polynomials with integer coefficients and string variables *)
module SP = MakePoly(
  struct
    type t = string
    let pp = pp_string
    let equal = (=)
    let compare = compare
  end) (IntRing)

(* ** Module of polynomials with integer coefficients and integer variables. *)
module IP = MakePoly(
  struct
    type t = int
    let pp fmt i =Format.fprintf fmt "v_%i" i
    let equal = (=)
    let compare = compare
  end) (IntRing)

type ipoly = IP.t
