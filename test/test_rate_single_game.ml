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


open Glicko2.Default.SingleGame

module Utils = Test_utils.MakeSingleGame(Glicko2.Default.SingleGame)
let player_return = Utils.player_return
let default_player =
  Utils.default_player Glicko2.Default.SingleGame.default_player

let test_simple_rate () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = `Draw;
    } in
  match rate game_result with
  | `Ok _ -> ()
  | _ -> Alcotest.fail "should return new ratings"

let test_simple_rate2 () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = `Draw;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-10)
       "ratings shouldn't move from 1500"
       1500.
       p1rating;
     Alcotest.check
       (Alcotest.float 1e-10)
       "ratings shouldn't move from 1500"
       1500.
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_win_changes_rating () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-3)
       "ratings should be larger"
       1662.31
       p1rating;
     Alcotest.check
       (Alcotest.float 1e-3)
       "rating should be lower"
       1337.69
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_two_wins_change_rating () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({new_player1;}) ->
     begin
       let game2_result =
         {
           player1 = new_player1;
           player2 = default_player ();
           game_outcome = `Player1Win;
         } in
       match rate game2_result with
       | `Ok(
           {
             new_player1 = {
               rating;
               rating_deviation;
             };
         }) ->
          Alcotest.check
            (Alcotest.float 1e-2)
            "rating should be larger"
            1750.54
            rating;
          Alcotest.check
            (Alcotest.float 1e-2)
            "deviation should be smaller"
            256.35
            rating_deviation
       | _ -> Alcotest.fail "should return new ratings"
     end
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_matters () =
  let game_result =
    {
      player1 = {(default_player ()) with rating_deviation = 100.};
      player2 = {(default_player ()) with rating_deviation = 10.};
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       1526.84
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       1499.43
       p2rating;

  | _ -> Alcotest.fail "should return new ratings"

let test_volatility_matters () =
  let game_result =
    {
      player1 = {(default_player ()) with volatility = 1.};
      player2 = {(default_player ()) with volatility = 10.};
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       1687.65
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       1025.55
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       290.32
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       290.32
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated2 () =
  let game_result =
    {
      player1 = {(default_player ()) with rating = 2500.};
      player2 = default_player ();
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       343.84
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       343.84
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated3 () =
  let game_result =
    {
      player1 = {(default_player ()) with rating = 2500.};
      player2 = default_player ();
      game_outcome = `Player2Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       343.84
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       343.84
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated4 () =
  let game_result =
    {
      player1 = {(default_player ())
                with volatility = 0.0001};
      player2 = default_player ();
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       290.23
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       290.32
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_rating_with_volatility1 () =
  let game_result =
    {
      player1 = {(default_player ())
                with volatility = 0.0001};
      player2 = {(default_player ())
                with volatility = 1.};
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1662.21
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1312.35
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_rating_with_volatility2 () =
  let game_result =
    {
      player1 = {(default_player ())
                with volatility = 0.0001;
                     rating_deviation = 10.};
      player2 = {(default_player ())
                with volatility = 1.;
                     rating_deviation = 10.};
      game_outcome = `Player1Win;
    } in
  match rate game_result with
  | `Ok({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1500.29
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1430.62
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let rate_single_game_suite = [
    "rate rates", `Quick, test_simple_rate;
    "draw rates correctly", `Quick, test_simple_rate2;
    "win changes rating", `Quick, test_win_changes_rating;
    "2 wings change rating", `Quick, test_two_wins_change_rating;
    "rating deviation affects new ratings", `Quick, test_deviation_matters;
    "volatility affects new ratings", `Quick, test_volatility_matters;
    "rating deviation is updated", `Quick, test_deviation_is_updated;
    "irrelevant result doesn't change deviation much",
    `Quick, test_deviation_is_updated2;
    "big upset doesn't change deviation much",
    `Quick, test_deviation_is_updated3;
    "volatility affects deviation",
    `Quick, test_deviation_is_updated4;
    "volatility affects rating", `Quick, test_rating_with_volatility1;
    "volatility affects rating", `Quick, test_rating_with_volatility2;
  ]
