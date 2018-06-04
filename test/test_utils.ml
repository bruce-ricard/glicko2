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


open Glicko2_types

module MakePlayer(Player : PLAYER) =
  struct
    open Player
    let player_to_string {rating; rating_deviation; volatility} =
      Printf.sprintf
        "{rating = %f; rating_deviation = %f; volatlity = %f}"
        rating
        rating_deviation
        volatility

    let compare_players (=) p1 p2 =
      p1.rating = p2.rating
      &&
        p1.rating_deviation = p2.rating_deviation
      &&
        p1.volatility = p2.volatility

    let player =
      Alcotest.testable
        (Fmt.of_to_string player_to_string)
        (compare_players (fun x y -> abs_float (x -. y) < 1e-2))

    type err = [ `ExceededIterations
               | `InvalidArgument of string
               | `InvalidVolatility
               | `UnknownError of string ]

    let (error_to_string : err -> string) = function
      | `InvalidVolatility -> "`InvalidVolatility"
      | `ExceededIterations -> "`ExceededIterations"
      | `UnknownError s -> Printf.sprintf "`Unkown`Error(%s)" s
      | `InvalidArgument s -> Printf.sprintf "`InvalidArgument(%s)" s

    let (player_return_to_string : player_result -> string) =
      let open Printf in
      function
      | `Ok p -> sprintf "Player(%s)" (player_to_string p)
      | `Error e -> sprintf "`Error(%s)" (error_to_string (e :> err))

    let player_return =
      Alcotest.testable (Fmt.of_to_string player_return_to_string)
                        (=)

    (* This function probably should be refactored.
       It takes a "default_player" function and makes it
       return a player, while failing on errors
     *)
    let default_player dp ?rating ?rating_deviation () =
      match dp ?rating ?rating_deviation () with
      | `Ok p -> p
      | _ -> Alcotest.fail "default player should be created"

  end

module MakeSingleGame(SingleGame : SINGLE_GAME) =
  struct
    module Player = MakePlayer(SingleGame)
    include Player
    open SingleGame

    let ratings_to_string {new_player1; new_player2} =
      Printf.sprintf
        "{new_player1: %s; new_player2: %s}"
        (player_to_string new_player1)
        (player_to_string new_player2)

    let rate_result_to_string = function
      | `Ok(ratings) ->
         Printf.sprintf
           "NewRatings(%s)"
           (ratings_to_string ratings)
      | `Error s -> Printf.sprintf "`Error(%s)" s

  end
