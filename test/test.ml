open Lambdaledge
open Map
open Player
open OUnit2

let init_test (name : string) (input_x : int) (input_y : int) expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_x (init input_x input_y), get_y (init input_x input_y))

let map_from_data_test (name : string) (input : string)
    (expected_output : int Array.t Array.t) : test =
  name >:: fun _ -> assert_equal expected_output (get_coords (make_map input))

let idle_test (name : string) (input : key_pressed) (expected_output : bool) :
    test =
  name >:: fun _ -> assert_equal expected_output (idle input)

let sprite_test (name : string) (input : Player.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (sprite input)

let dir_test (name : string) (input_p : Player.t)
    (input_kp : key_pressed) (expected_output : dir) : test =
  name >:: fun _ -> assert_equal expected_output (dir input_p input_kp)

let player_tests =
  [
    init_test "0 0" 0 0 (0, 0);
    init_test "1 0" 1 0 (1, 0);
    init_test "0 1" 0 1 (0, 1);
    init_test "1 1" 2 2 (2, 2);
    init_test "5 5" 5 5 (5, 5);
    idle_test "1 idle" { l = 0; r = 0; u = 0; d = 0; x = 0; c = 0 } true;
    idle_test "1 idle" { l = 1; r = 0; u = 0; d = 0; x = 0; c = 0 } false;
    idle_test "1 idle" { l = 0; r = 1; u = 0; d = 0; x = 0; c = 0 } false;
    idle_test "1 idle" { l = 0; r = 0; u = 0; d = 0; x = 0; c = 1 } false;
    idle_test "1 idle" { l = 1; r = 1; u = 0; d = 0; x = 0; c = 0 } false;
    idle_test "1 idle" { l = 1; r = 0; u = 0; d = 0; x = 0; c = 1 } false;
    idle_test "1 idle" { l = 1; r = 1; u = 0; d = 0; x = 0; c = 1 } false;
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let ans = 6 in
     sprite_test "Sprite 1" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let ans = 0 in
     sprite_test "Sprite 2" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = false;
         can_dash = true;
         dir = RU;
       }
     in
     let ans = 0 in
     sprite_test "Sprite 2" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let ans = 1 in
     sprite_test "Sprite 3" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = false;
         can_dash = true;
         dir = LU;
       }
     in
     let ans = 1 in
     sprite_test "Sprite 3" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let ans = 2 in
     sprite_test "Sprite 4" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = false;
         can_dash = true;
         dir = RD;
       }
     in
     let ans = 2 in
     sprite_test "Sprite 4" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let ans = 3 in
     sprite_test "Sprite 5" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = false;
         can_dash = true;
         dir = LD;
       }
     in
     let ans = 3 in
     sprite_test "Sprite 5" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let ans = 4 in
     sprite_test "Sprite 6" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = false;
         can_dash = true;
         dir = L;
       }
     in
     let ans = 5 in
     sprite_test "Sprite 7" inp ans);
    (let inp =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = false;
         can_dash = true;
         dir = R;
       }
     in
     let ans = 7 in
     sprite_test "Sprite 8" inp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 0; x = 0; c = 0 } in
     let ans = R in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 0; x = 0; c = 0 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 0; x = 0; c = 0 } in
     let ans = R in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 0; x = 0; c = 0 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 1; x = 0; c = 0 } in
     let ans = R in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 1; x = 0; c = 0 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 1; x = 0; c = 0 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 0; x = 0; c = 1 } in
     let ans = R in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 0; x = 0; c = 1 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 0; x = 0; c = 1 } in
     let ans = R in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 0; x = 0; c = 1 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 1; x = 0; c = 1 } in
     let ans = R in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 1; x = 0; c = 1 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 1; x = 0; c = 1 } in
     let ans = L in
     dir_test "Dir 1" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 0 } in
     let ans = RU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 0 } in
     let ans = RU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 1 } in
     let ans = RU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 1 } in
     let ans = RU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 0 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 0 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 1 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 1 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 0 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 0 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 1 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 1 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 0 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 0 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 1 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 1 } in
     let ans = RU in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 0 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 0 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 1 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 1 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 0 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 0 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 1; d = 0; x = 0; c = 1 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 1; d = 0; x = 0; c = 1 } in
     let ans = LU in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = RD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = RD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 1 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 1 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LD;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 1 } in
     let ans = RD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 1 } in
     let ans = RD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = RD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = R;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = RD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 1 } in
     let ans = RD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 1 } in
     let ans = RD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = RD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = RU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = RD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 1 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 1 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = LD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = LD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 1 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 1 } in
     let ans = LD in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 1; r = 1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = LD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 0; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = LD in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 0; r = 1; u = 0; d = 1; x = 0; c = 1 } in
     let ans = R in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = -1; r = 0; u = 0; d = 1; x = 0; c = 1 } in
     let ans = R in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = -1; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = R in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = L;
       }
     in
     let inp_kp = { l = 0; r = 1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = R in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 1; r = 0; u = 0; d = 1; x = 0; c = 1 } in
     let ans = L in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 0; r = -1; u = 0; d = 1; x = 0; c = 1 } in
     let ans = L in
     dir_test "Dir 2" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 1; r = 0; u = 0; d = 1; x = 0; c = 0 } in
     let ans = L in
     dir_test "Dir 3" inp_p inp_kp ans);
    (let inp_p =
       {
         x = 0;
         y = 0;
         v_y = 0;
         v_x = 0;
         on_ground = true;
         idle = true;
         can_dash = true;
         dir = LU;
       }
     in
     let inp_kp = { l = 0; r = -1; u = 0; d = 1; x = 0; c = 0 } in
     let ans = L in
     dir_test "Dir 3" inp_p inp_kp ans);
  ]

let _map_tests =
  [
    map_from_data_test "map" "maps/map.txt"
      [|
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 2; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 2; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 2; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      |];
    map_from_data_test "map" "maps/map1.txt"
      [|
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 |];
      |];
    map_from_data_test "map2" "maps/map.txt"
      [|
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      |];
    map_from_data_test "map" "maps/map3.txt"
      [|
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
        [| 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 |];
      |];
    map_from_data_test "map" "maps/map4.txt"
      [|
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
      |];
    map_from_data_test "map" "maps/map5.txt"
      [|
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 0; 0; 0; 0; 0; 3; 3; 3; 3; 3; 3 |];
        [| 0; 0; 0; 3; 3; 3; 3; 3; 0; 3; 3; 0; 3; 3; 3; 0; 0; 0; 3; 3 |];
        [| 3; 3; 3; 0; 0; 0; 3; 0; 3; 0; 3; 0; 3; 0; 0; 0; 0; 0; 0; 0 |];
        [| 0; 3; 0; 3; 3; 0; 3; 0; 3; 0; 3; 0; 0; 0; 0; 0; 3; 3; 3; 0 |];
        [| 0; 0; 0; 0; 3; 3; 3; 3; 3; 3; 3; 3; 0; 0; 0; 0; 0; 0; 3; 3 |];
        [| 3; 3; 3; 3; 3; 0; 0; 0; 0; 3; 3; 3; 3; 0; 0; 0; 0; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 3; 3; 3; 1; 1; 3; 3; 3; 1; 1 |];
        [| 1; 2; 2; 2; 2; 2; 3; 3; 3; 3; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 2; 2; 2; 2; 3; 3; 3; 3; 3; 2; 2; 2; 3; 3 |];
        [| 3; 2; 2; 2; 3; 3; 3; 3; 2; 2; 2; 3; 3; 3; 3; 3; 2; 2; 2; 2 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
        [| 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 |];
      |];
  ]

let suite = "test suite" >::: List.flatten [ player_tests ]
let _ = run_test_tt_main suite
