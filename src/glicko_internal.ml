(*
 * Glicko2 library. Implements the Glicko2 algorithm.
 * Copyright (C) 2018-present Bruce Ricard
 *
 * This file is part of glicko2.
 *
 * Glicko2 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * Glicko2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Glicko2. If not, see <http://www.gnu.org/licenses/>.
 *)


module type INTERNAL_CONFIG =
  sig
    val tau : float
    val epsilon : float
    val max_volatility_iterations : int
  end

module Make(Config : INTERNAL_CONFIG) =
  struct
    (* Constants *)
    let pi = 2. *. acos 0.
    let epsilon = Config.epsilon
    let tau = Config.tau
    let scale_factor = 173.7178

    (* Types *)
    type player =
      { r : float;
	rd : float;
	sigma : float }

    type personal_game_outcome =
      Win | Lost | Draw

    type opponent =
      { rj : float;
	rdj : float;
	sj : personal_game_outcome }

    let outcome_to_float = function
        Win -> 1.
      | Lost -> 0.
      | Draw -> 0.5

    let print_opponent (muj, phij, gphij, emmp, sj) =
      Printf.printf "Muj: %f; PhiJ: %f; GPhij: %f; EMMP: %f; Sj: %f\n" muj phij gphij emmp sj

    let printf_opponents opps = List.iter print_opponent opps

    (* Helper functions *)
    let square x = x *. x
    let sum xs = List.fold_left (+.) 0.0 xs

    let g phi =
      1.0 /. sqrt (1.0 +. 3.0 *. phi *. phi /. (pi *. pi))

    let e mu muj phij =
      1.0 /. (1.0 +. (exp (-. g(phij) *. (mu -. muj))))

    (* Step 2 *)
    let scale r rd =
      let mu = (r -. 1500.0) /. scale_factor in
      let phi = rd /. scale_factor in
      (mu, phi)

    let scale_opponents mu xs =
      List.map
        (function {rj; rdj; sj} ->
                  let (muj, phij) = scale rj rdj in
                  (muj, phij, g phij, e mu muj phij, sj)) xs

    (* Step 3 *)
    let update_rating os =
      1.0 /. sum ( List.map (function (_, _, gphij, emmp, _) -> square gphij *. emmp *. (1.0 -. emmp)) os )

    (* Step 4: Delta computation *)
    let compute_delta v opps =
      let f (_, _, gphij, emmp, sj) = gphij *. (outcome_to_float sj -. emmp) in
      v *. sum (List.map f opps)

    (* Step 5: Volatility computation *)
    let rec vol_k k f a tau =
      let const = a -. (float_of_int k) *. sqrt (tau *. tau) in
      if (f const) < 0.0 then
	vol_k (k + 1) f a tau
      else
	const

    let sign x = float_of_int (compare x 0.0)

    exception Exceeded_Iterations

    let i_compute_volatility sigma phi v delta tau =
      let a = log (sigma *. sigma) in
      let phi2 = phi *. phi in
      let f = function x ->
	               let ex = exp x in
	               let d2 = delta *. delta in
	               let a2 = phi2 +. v +. ex in
	               let p2 = (x -. a) /. (tau *. tau) in
	               let p1 = (ex *. (d2 -. phi2 -. v -. ex)) /. (2.0 *. a2 *. a2) in
	               p1 -. p2 in
      let b = if delta *. delta > phi *. phi +. v then
		log (delta *. delta -. phi *. phi -. v)
	      else
		vol_k 1 f a tau
      in
      let fa = f a in
      let fb = f b in
      let rec loop a b f fa fb = function
	| 0 -> raise Exceeded_Iterations
	| k ->
	   if abs_float(b -. a) <= epsilon then
	     exp (a /. 2.0)
	   else
	     let c = (a +. b) *. 0.5 in
	     let fc = f c in
	     let d = c +. (c -. a) *. (sign(fa -. fb) *. fc) /. sqrt(fc *. fc -. fa *. fb) in
	     let fd = f d in
	     if (sign fd) != (sign fc) then
	       loop c d f fc fd (k-1)
	     else
	       if (sign fd) != (sign fa) then
	         loop a d f fa fd (k-1)
	       else
	         loop d b f fd fb (k-1) in
      loop a b f fa fb Config.max_volatility_iterations

    (* Step 6 *)
    let phi_star sp phi = sqrt ( square phi +. square sp )

    (* Step 7 *)
    let new_rating phistar mu v opps =
      let phip = 1.0 /. sqrt ( (1.0 /. square phistar) +. (1.0 /. v) ) in
      let l = List.map (function (_, _, gphij, emmp, sj) ->
                                 gphij *. (outcome_to_float sj -. emmp)) opps in
      let mup = mu +. square phip *. sum l in
      (mup, phip)

    (* Step 8 *)
    let unscale mup phip =
      let rp = scale_factor *. mup +. 1500.0 in
      let rdp = scale_factor *. phip in
      (rp, rdp)

    let rate {r; rd; sigma} opps =
      let (mu, phi) = scale r rd in
      let sopps = scale_opponents mu opps in
      let v = update_rating sopps in
      let delta = compute_delta v sopps in
      let sigmap = i_compute_volatility sigma phi v delta tau in
      let phistar = phi_star sigmap phi in
      let (mup, phip) = new_rating phistar mu v sopps in
      let (r1, rd1) = unscale mup phip in
      {r = r1; rd = rd1; sigma = sigmap}

    let update_after_not_playing_in_rating_period {r; rd; sigma} =
      let rs, rds = scale r rd in
      let new_rds = sqrt ((square rds) +. (square sigma)) in
      let _rp, rdp = unscale rs new_rds in
      {r; rd = rdp; sigma}
  end
