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

module Glicko2 = Glicko2.Default.LowLevel
open Glicko2

module Utils = Test_utils.MakePlayer(Glicko2)

let default_player = Utils.default_player Glicko2.default_player
let player = Utils.player

let test_rate_one_game () =
  let game_results =
    {
      player = default_player ();
      games = {
          opponent = player_to_opponent (default_player ());
          result = `Win
        },[];
    } in
  let expected_new_player =
    Glicko2.{
      rating = 1662.31;
      rating_deviation = 290.32;
      volatility = 0.06;
    }
  in
  let open Glicko2 in
  match rate game_results with
  | `Ok new_player ->
     Alcotest.check
       player
       "Player should have higher rating"
       expected_new_player
       new_player
  | _ ->  Alcotest.fail "shouldn't internal error"

let test_rate_two_games () =
  let game1 =
    {
      opponent = player_to_opponent (default_player ());
      result = `Win
    } in
  let game_results =
    {
      player = default_player ();
      games = game1,[game1];
    } in
  let expected_new_player =
    Glicko2.{
      rating = 1747.31;
      rating_deviation = 253.40;
      volatility = 0.06;
    }
  in
  let open Glicko2 in
  match rate game_results with
  | `Ok new_player ->
     Alcotest.check
       player
       "Player should have higher rating"
       expected_new_player
       new_player
  | _ ->  Alcotest.fail "shouldn't internal error"


let rate_suite =  [
    "one game played", `Quick, test_rate_one_game;
    "two games played", `Quick, test_rate_two_games;
  ]
