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

module Config =
  struct
    (* To update those 3 values, you should read the Glicko2
       paper: http://www.glicko.net/glicko/glicko2.pdf *)

    let tau = `High

    (* Unless you have strong performance requirements, you
       probably should leave this to `Default *)
    let epsilon = `Manual 1e-4

    (* Unless you have strong performance requirements, you
       probably should leave this to `Default *)
    let max_volatility_iterations = `Manual 50


    (* These 3 last values let you set the
       default player's settings *)

    let default_rating = `Manual 1200
    let default_deviation = `Manual 215

    (* You should read the Glicko2 paper before updating
        this value *)
    let default_volatility = `Default
  end

module M = Glicko2.Make(Config)
module Glicko2 = M.SingleGame

(* Now you can use the Glicko2 module like you would use it
in single_game.ml or low_level.ml *)
