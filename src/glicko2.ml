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


module Logs = InternalLogs

let log_source = Logs.src

module Make (Config : Glicko2_types.GLICKO2_CONFIG) =
  struct
    module InternalConf =
      struct
        let tau = Default_values.tau Config.tau
        let epsilon = Default_values.epsilon Config.epsilon
        let max_volatility_iterations =
          Default_values.max_iterations Config.max_volatility_iterations
      end
    module Glicko_internal = Glicko_internal.Make(InternalConf)

    type unknown_error = [
        `UnknownError of string
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

    module Player =
      struct

        type player =
          {
            rating: float;
            rating_deviation: float;
            volatility: float;
          }

        type player_result = (player, player_error) result

        let default_rating =
          Default_values.rating Config.default_rating
        and default_deviation =
          Default_values.deviation Config.default_deviation
        and default_volatility =
          Default_values.volatility Config.default_volatility

        let default_player
              ?(rating=default_rating)
              ?(rating_deviation=default_deviation)
              () =
          if rating < 100 then
            `Error (`InvalidArgument "rating cannot be lower than 100")
          else if rating_deviation < 0 then
            `Error (`InvalidArgument "rating_deviation cannot be negative")
          else if rating_deviation > 350 then
            `Error (`InvalidArgument
                    "rating_deviation cannot be greater than 350")
          else
            `Ok
              {
                rating = float_of_int rating;
                rating_deviation = float_of_int rating_deviation;
                volatility = default_volatility;
              }

        let player_to_string player =
          Printf.sprintf
            "{rating = %f; rating_deviation = %f; volatility = %f}"
            player.rating
            player.rating_deviation
            player.volatility

        let internal_player player =
          let open Glicko_internal in
          {
            r = player.rating;
            rd = player.rating_deviation;
            sigma = player.volatility;
          }

        let internal_opponent player game_result =
          let open Glicko_internal in
          { rj = player.rating;
            rdj = player.rating_deviation;
            sj = game_result;
          }

        let player_from_internal internal =
          let open Glicko_internal in
          {
            rating = internal.r;
            rating_deviation = internal.rd;
            volatility = internal.sigma;
          }

        let update_player_after_not_player_in_rating_period player =
          let internal_p = internal_player player in
          let updated_internal =
            Glicko_internal.update_after_not_playing_in_rating_period
          internal_p in
          `Ok (player_from_internal updated_internal)

      end

    let internal_outcome =
      let open Glicko_internal in
      function
      | `Win -> Win
      | `Lose -> Lost
      | `Draw -> Draw

    let is_too_small volatility = volatility < 1e-10

    module LowLevel =
      struct
        include Player
        type personal_result = [ `Win | `Lose | `Draw ]

        type opponent =
          {
            rating: float;
            rating_deviation: float;
          }

        let player_to_opponent ({rating; rating_deviation; _} : player) =
          {rating; rating_deviation;}

        let opponent_to_internal opponent game_result =
          let open Glicko_internal in
          { rj = opponent.rating;
            rdj = opponent.rating_deviation;
            sj = game_result }

        type game_outcome = {
            opponent: opponent;
            result: personal_result;
          }

        type game_results =
          {
            player: player;
            games : game_outcome * game_outcome list
          }

        let rate_unsafe game_results =
          let player = internal_player game_results.player in
          let game_outcomes =
            let g,gs = game_results.games in
            g :: gs  in

          let new_player =
            Glicko_internal.rate
              player
              (List.map
                 (function game_outcome ->
                              opponent_to_internal
                                game_outcome.opponent
                                (internal_outcome game_outcome.result)
                    )
                 game_outcomes
              )
          in
          player_from_internal new_player

        let rate game_results =
          if is_too_small game_results.player.volatility then
            begin
              Logs.info (fun m ->
                  m
                    "Invalid volatlity on input: %s"
                    (player_to_string game_results.player)
                );
              `Error `InvalidVolatility
            end
          else
            try
              `Ok (rate_unsafe game_results)
            with
            | Glicko_internal.Exceeded_Iterations ->
               Logs.err (fun m ->
                   m "Glicko2 Exceeded iterations"
                 ); `Error `ExceededIterations
            | e ->
               Logs.err (fun m ->
                   m "Glicko2 unknown error %s"
                     (Printexc.to_string e)
                 ); `Error (`UnknownError (Printexc.to_string e))

        let update_player_after_not_player_in_rating_period =
          update_player_after_not_player_in_rating_period
      end

    module SingleGame =
      struct
        include Player

        type game_outcome =
          [ `Player1Win | `Player2Win | `Draw ]

        type game_result =
          {
            player1: player;
            player2: player;
            game_outcome: game_outcome
          }

        type players = P1 | P2

        type new_ratings =
          {
            new_player1: player;
            new_player2: player;
          }

        let outcome_to_string = function
          | `Player1Win -> "Player1Win"
          | `Player2Win -> "Player2Win"
          | `Draw -> "Draw"

        let game_result_to_string result =
          Printf.sprintf
            "{player1 = %s; player2 = %s; game_outcome = %s}"
            (player_to_string result.player1)
            (player_to_string result.player2)
            (outcome_to_string result.game_outcome)

        let personal_game_outcome game_outcome player =
          let open Glicko_internal in
          match game_outcome,player with
          | `Player1Win, P1 | `Player2Win, P2 -> Win
          | `Draw, _ -> Draw
          | `Player1Win, P2 | `Player2Win, P1 -> Lost

        let rate_unsafe game_result =
          let p1 = internal_player game_result.player1
          and p2 = internal_player game_result.player2 in

          let newp1 = Glicko_internal.rate
                        p1
                        [internal_opponent
                           game_result.player2
                           (personal_game_outcome game_result.game_outcome P1)
                        ]
          and newp2 = Glicko_internal.rate
                        p2
                        [internal_opponent
                           game_result.player1
                           (personal_game_outcome game_result.game_outcome P2)
                        ]
          in
          {
            new_player1 = player_from_internal newp1;
            new_player2 = player_from_internal newp2
          }

        let rate game_result =
          if is_too_small game_result.player1.volatility
             || is_too_small game_result.player2.volatility then
            begin
              Logs.info (fun m ->
                  m
                    "Invalid volatlity on input: %s"
                    (game_result_to_string game_result)
                );
              `Error `InvalidVolatility
            end
          else
            try
              `Ok (rate_unsafe game_result)
            with
            | Glicko_internal.Exceeded_Iterations ->
               Logs.err (fun m ->
                   m
                     "Glicko2 Exceeded iterations"
                 ); `Error `ExceededIterations
            | e ->
               Logs.err (fun m ->
                   m
                     "Glicko2 unknown error: %s"
                     (Printexc.to_string e)
                 ); `Error (`UnknownError (Printexc.to_string e))

        let update_player_after_not_player_in_rating_period =
          update_player_after_not_player_in_rating_period
      end
  end

module DefaultConfig =
  struct
    let tau = `Default
    let epsilon = `Default
    let max_volatility_iterations = `Default

    let default_rating = `Default
    let default_deviation = `Default
    let default_volatility = `Default
  end

module Default = Make(DefaultConfig)
