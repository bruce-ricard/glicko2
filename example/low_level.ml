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

let alice =
  match Glicko2.default_player () with
  | `Ok p -> p
  | `Error _ -> failwith "error creating player"

let bob =
  match Glicko2.default_player ~rating:1800 () with
  | `Ok p -> p
  | `Error _ -> failwith "error creating player"

let charlie =
  match Glicko2.default_player () with
  | `Ok p -> p
  | `Error _ -> failwith "error creating player"

let bob_opponent = Glicko2.player_to_opponent bob
let alice_game_results =
  Glicko2.{
    player = alice;
    games = {opponent = bob_opponent; result = `Win},
            [{opponent = bob_opponent; result = `Lose}]
  }

let new_alice =
  match Glicko2.rate alice_game_results with
  | `Ok a -> a
  | `Error _ -> failwith "deal with error"

let alice_opponent = Glicko2.player_to_opponent alice
let bob_game_results =
  Glicko2.{
    player = bob;
    games = {opponent = alice_opponent; result = `Lose},
            [{opponent = alice_opponent; result = `Win}]
  }

let new_bob =
  match Glicko2.rate bob_game_results with
  | `Ok b -> b
  | `Error _ -> failwith "deal with error"

let new_charlie =
  match
    Glicko2.update_player_after_not_player_in_rating_period charlie
  with
  | `Ok p -> p
  | `Error _ -> failwith "deal with error"

(* Those are the players you have at the end *)
let new_players = [new_alice; new_bob; new_charlie]

let new_ratings = List.map (fun (p : Glicko2.player) -> p.Glicko2.rating) new_players
