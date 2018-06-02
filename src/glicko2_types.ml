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


module type GLICKO2_CONFIG =
  sig
    val tau : [ `Default | `High | `Low | `Manual of float ]
    val epsilon : [ `Default | `Manual of float ]
    val max_volatility_iterations : [ `Default | `Manual of int ]

    val default_rating : [ `Default | `Manual of int ]
    val default_deviation : [ `Default | `Manual of int ]
    val default_volatility :  [ `Default | `Manual of float ]
  end

type unknown_error = [
    `UnknownError of string
  ]

type rate_error =
  [
  | `InvalidVolatility
  | `ExceededIterations
  | unknown_error
  ]

type player_error = [
  | `InvalidArgument of string
  | unknown_error
  ]

type ('a, 'b) result =
  [
  | `Ok of 'a
  | `Error of 'b
  ]

type 'a rate_result = ('a, rate_error) result

module type PLAYER =
  sig
    (**
The type containing the data need for the Glicko2 algorithm.
You can create this structure yourself, but we recommend you
use the "default_player" function defined in this module.
     *)
    type player =
      {
        rating: float;
        rating_deviation: float;
        volatility: float;
      }

    type player_result = (player, player_error) result

  end

module type LOW_LEVEL =
  sig
    include PLAYER
    type personal_result = [ `Win | `Lose | `Draw ]

    type opponent =
      {
        rating: float;
        rating_deviation: float;
      }

    val player_to_opponent : player -> opponent

    type game_outcome = {
        opponent: opponent;
        result: personal_result;
      }

    type game_results =
      {
        player : player;
        games : game_outcome * game_outcome list
      }

    val default_player:
      ?rating:int -> ?rating_deviation:int -> unit
      -> player_result
    val rate: game_results -> player rate_result
    val update_player_after_not_player_in_rating_period:
      player -> player_result
  end

module type SINGLE_GAME =
  sig
    include PLAYER

    type game_outcome =
      [ `Player1Win | `Player2Win | `Draw ]

    type new_ratings =
      {
        new_player1: player;
        new_player2: player;
      }

    (**
    The input type to the Glicko2 rating functions.
    The two players, and the outcome of the game.
     *)
    type game_result =
      {
        player1: player;
        player2: player;
        game_outcome: game_outcome;
      }

    (**
    The recommended way of creating a new player.
    It will have a rating of 1500 (unless overwritten),
    a rating deviation of 350 (unless overwritten), and
    a volatility of 0.06.
     *)
    val default_player:
      ?rating:int -> ?rating_deviation:int -> unit
      -> player_result

    val rate: game_result -> new_ratings rate_result
    val update_player_after_not_player_in_rating_period:
      player -> player_result
  end


module type GLICKO2 =
  sig
    module LowLevel : LOW_LEVEL
    module SingleGame : SINGLE_GAME
  end
