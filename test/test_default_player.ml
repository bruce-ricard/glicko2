open Test_utils
open Glicko2
open SingleGame

let test_default_player () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Player
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
    (Glicko2.Player
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
    (Glicko2.Player
       {
         rating = 1500.;
         rating_deviation = 150.2;
         volatility = 0.06;
       }
    )
    (default_player ~rating_deviation:150.2 ())

let test_default_player_low_rating () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Error
       "rating cannot be lower than 100"
    )
    (default_player ~rating:99 ())

let test_default_player_low_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Error
       "rating_deviation cannot be negative"
    )
    (default_player ~rating_deviation:(-1e-5) ())

let test_default_player_high_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Error
       "rating_deviation cannot be greater than 350"
    )
    (default_player ~rating_deviation:(350.01) ())

let default_player_suite = [
    "creation", `Quick, test_default_player;
    "correct rating", `Quick, test_default_player_sets_rating;
    "correct deviation", `Quick, test_default_player_sets_deviation;
    "low rating", `Quick, test_default_player_low_rating;
    "low rating deviation", `Quick, test_default_player_low_deviation;
    "high rating deviation", `Quick, test_default_player_high_deviation;
  ]