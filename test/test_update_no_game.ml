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

open Glicko2.Default
open SingleGame

module Utils = Test_utils.MakeSingleGame(Glicko2.Default.SingleGame)
let player_return = Utils.player_return
let default_player =
  Utils.default_player Glicko2.Default.SingleGame.default_player

let update =
  update_player_after_not_player_in_rating_period

let test_update_only_deviation () =
  let p = default_player () in
  let new_p = update p in
  match new_p with
  | `Ok {rating; rating_deviation; volatility} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating shouldn't change"
         1500.
         rating;
       Alcotest.check
         (Alcotest.float 1e-2)
         "volaility shouldn't change"
         0.06
         volatility
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"

let test_update_deviation () =
  let p = default_player () in
  let new_p = update p in
  match new_p with
  | `Ok {rating_deviation} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating deviation should be updated"
         350.155
         rating_deviation;
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"


let test_update_deviation2 () =
  let p = default_player ~rating_deviation:5 () in
  let new_p = update p in
  match new_p with
  | `Ok {rating_deviation} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating deviation should be updated"
         11.56
         rating_deviation;
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"

let test_update_deviation_with_high_volatility () =
  let p = {rating = 1500.; rating_deviation = 5.; volatility = 1.} in
  let new_p = update p in
  match new_p with
  | `Ok {rating_deviation} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating deviation should be updated"
         173.79
         rating_deviation;
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"

let update_suite = [
    "update only touches deviation", `Quick, test_update_only_deviation;
    "update updates deviation", `Quick, test_update_deviation;
    "update updates deviation", `Quick, test_update_deviation2;
    "volatility increases deviation", `Quick, test_update_deviation_with_high_volatility;
  ]
