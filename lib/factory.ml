open Ctypes
open Poly

module FB = Factory_bindings.Bindings(Factory_generated)
module FT = Factory_bindings.Types(Factory_generated_types)

module US = Unsigned.Size_t
module UL = Unsigned.ULong
module L  = List
module F  = Format


(* ** Conversions
 * ------------------------------------------------------------------------ *)

let _print_cpoly (maxvar,nterms,cexpvecs,ccoeffs) =
  FB.c_print_cpoly maxvar nterms cexpvecs ccoeffs

(** The order of coefficients / exponent vectors does not matter,
    but has to be consistent in both.
    The order in the exponent vector e is
    e[0]: exponent of $v_1$
    ... 
*)
let ipoly_to_cpoly ip =
  if IP.equal ip IP.zero then
    (0,0,from_voidp (ptr long) null, from_voidp long null)
  else (
    let maxvar =
      match L.sort (fun a b -> - (compare a b)) (IP.vars ip) with
      | []   -> 1 (* we treat this case like 1 variable *)
      | x::_ ->
        if x < 1 then
          failwith (F.sprintf "only variables >= 1 supported, got %i" x)
        else
          x
    in
    let terms = IP.to_terms ip in
    let nterms = L.length terms in
    let ccoeffs = FB.c_new_coeffs nterms in
    let cexpvecs = FB.c_new_expvecs maxvar nterms in
    L.iteri (fun i (evec,coeff) ->
      let v = Big_int.int64_of_big_int coeff in (* should raise Failure on overflow *)
      ccoeffs +@ i <-@ Signed.Long.of_int64 v;  (* FIXME: are these types always convertible? *)
      let cexpvec = !@ (cexpvecs +@ i) in
      for j = 0 to maxvar - 1 do
        let e = try L.assoc (j+1) evec with Not_found -> 0 in
        cexpvec +@ j <-@ Signed.Long.of_int e
      done)
      terms;
    (maxvar, nterms, cexpvecs, ccoeffs)
  )

let cpoly_to_ipoly (maxvar, nterms, cexpvecs, ccoeffs) =
  if nterms = 0 then IP.zero
  else (
    assert (maxvar > 0);
    let res = ref [] in
    for i = 0 to nterms - 1 do
      let c = !@ (ccoeffs +@ i) in
      let cexpvec = !@ (cexpvecs +@ i) in
      let vs = ref [] in
      for j = 0 to maxvar - 1 do
        let e = !@ (cexpvec +@ j) in
        vs := (j+1,Signed.Long.to_int e)::!vs;
      done;
      let e = Signed.Long.to_int64 c in (* FIXME: are these types always convertible? *)
      res := (!vs,Big_int.big_int_of_int64 e)::!res;
    done;
    IP.from_terms !res
  )

let free_cpoly (_maxvar, nt, cevs, cos) =
  if nt > 0 then (
    FB.c_free_coeffs cos;
    FB.c_free_expvecs cevs nt
  )

let wrap c_f verbose p1 p2 =
  if verbose then F.printf "p1 = %a, p2 = %a\n%!" IP.pp p1 IP.pp p2;
  let (maxvar1, nt1, cevs1, cos1) = ipoly_to_cpoly p1 in
  let (maxvar2, nt2, cevs2, cos2) = ipoly_to_cpoly p2 in
  let res = c_f maxvar1 nt1 cevs1 cos1 maxvar2 nt2 cevs2 cos2 in
  let maxvar_res = getf res FB.dp_maxvar in
  let nt_res = getf res FB.dp_nterms in
  let cevs_res = getf res FB.dp_expvecs in
  let cos_res = getf res FB.dp_coeffs in
  let pres = cpoly_to_ipoly (maxvar_res,nt_res,cevs_res,cos_res) in
  free_cpoly (maxvar_res, nt_res, cevs_res, cos_res);
  free_cpoly (maxvar1, nt1, cevs1, cos1);
  free_cpoly (maxvar2, nt2, cevs2, cos2);
  pres

let gcd p1 p2 =
  if IP.equal IP.one p1 || IP.equal IP.one p2 then IP.one else wrap FB.c_gcd false p1 p2

let reduce = wrap FB.c_reduce false

let div p1 p2 =
  if IP.equal p2 IP.one then p1 else wrap FB.c_div false p1 p2

let reduce_zero p1 p2 =
  let (maxvar1, nt1, cevs1, cos1) = ipoly_to_cpoly p1 in
  let (maxvar2, nt2, cevs2, cos2) = ipoly_to_cpoly p2 in
  let res = FB.c_reduce_zero maxvar1 nt1 cevs1 cos1 maxvar2 nt2 cevs2 cos2 in
  free_cpoly (maxvar1, nt1, cevs1, cos1);
  free_cpoly (maxvar2, nt2, cevs2, cos2);
  (* log_ig (lazy (Format.fsprintf "reduce %a %a %i" IP.pp p1 IP.pp p2 res)); *)
  (res = 1)

let gcd_div p1 p2 =
  if IP.equal IP.one p1 || IP.equal IP.one p2 then (IP.one,p1,p2) else
  let get_item li =
    let head = !@ (getf li FB.dpl_head) in
    let tail = match getf li FB.dpl_tail with None -> None | Some p -> Some (!@ p) in
    let maxvar = getf head FB.dp_maxvar in
    let nt     = getf head FB.dp_nterms in
    let cevs   = getf head FB.dp_expvecs in
    let cos    = getf head FB.dp_coeffs in
    (tail,cpoly_to_ipoly (maxvar,nt,cevs,cos))
  in
  let (maxvar1, nt1, cevs1, cos1) = ipoly_to_cpoly p1 in
  let (maxvar2, nt2, cevs2, cos2) = ipoly_to_cpoly p2 in
  let res = FB.c_gcd_div maxvar1 nt1 cevs1 cos1 maxvar2 nt2 cevs2 cos2 in
  let (oli,h) = get_item (!@ res) in
  match oli with
  | None -> assert false
  | Some li ->
    let (oli,p1) = get_item li in
    begin match oli with
    | None -> assert false
    | Some li ->
      let (oli,p2) = get_item li in
      assert (oli = None);
      FB.c_free_dpoly_list res;
      free_cpoly (maxvar1, nt1, cevs1, cos1);
      free_cpoly (maxvar2, nt2, cevs2, cos2);
      (h,p1,p2)
    end

let factor p =
  let rec get_factors li =
    let head = !@ (getf li FB.dpl_head) in
    let exp = getf li FB.dpl_head_aux in
    let tail = getf li FB.dpl_tail in
    let maxvar = getf head FB.dp_maxvar in
    let nt     = getf head FB.dp_nterms in
    let cevs   = getf head FB.dp_expvecs in
    let cos    = getf head FB.dp_coeffs in
    let others =
      match tail with
      | None   -> []
      | Some p -> get_factors (!@ p)
    in
    (cpoly_to_ipoly (maxvar,nt,cevs,cos), exp)::others
  in
  let (maxvar, nt, cevs, cos) = ipoly_to_cpoly p in
  let res = FB.c_factor maxvar nt cevs cos in
  let facs = get_factors (!@ res) in
  FB.c_free_dpoly_list res;
  free_cpoly (maxvar, nt, cevs, cos);
  if L.length facs > 0 then
    L.filter (fun (f,_) -> not (IP.equal f IP.one)) facs
  else
    facs
