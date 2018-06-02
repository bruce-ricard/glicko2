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


open Test_utils
module Glicko2 = Glicko2.Default
open Glicko2
open SingleGame

module Utils = Test_utils.MakeSingleGame(Glicko2.SingleGame)
let player_return = Utils.player_return

let test_default_player () =
  Alcotest.check
    player_return
    "default player should be correct"
    (`Ok
       {
         rating = 1500.;
         rating_deviation = 350.;
         volatility = 0.06;
       }
    )
    (default_player ())

let test_default_player_sets_rating () =
  Alcotest.check
    player_return
    "default player should be correct"
    (`Ok
       {
         rating = 1000.;
         rating_deviation = 350.;
         volatility = 0.06;
       }
    )
    (default_player ~rating:1000 ())

let test_default_player_sets_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (`Ok
       {
         rating = 1500.;
         rating_deviation = 150.;
         volatility = 0.06;
       }
    )
    (default_player ~rating_deviation:150 ())

let test_default_player_low_rating () =
  Alcotest.check
    player_return
    "default player should be correct"
    (`Error (
         `InvalidArgument
          "rating cannot be lower than 100"
    ))
    (default_player ~rating:99 ())

let test_default_player_low_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (`Error
       (`InvalidArgument
       "rating_deviation cannot be negative")
    )
    (default_player ~rating_deviation:(-1) ())

let test_default_player_high_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (`Error
       (`InvalidArgument
       "rating_deviation cannot be greater than 350")
    )
    (default_player ~rating_deviation:(351) ())

let default_player_suite = [
    "creation", `Quick, test_default_player;
    "correct rating", `Quick, test_default_player_sets_rating;
    "correct deviation", `Quick, test_default_player_sets_deviation;
    "low rating", `Quick, test_default_player_low_rating;
    "low rating deviation", `Quick, test_default_player_low_deviation;
    "high rating deviation", `Quick, test_default_player_high_deviation;
  ]
