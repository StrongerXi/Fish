module Conf = Fish.Game.Board_config
module P = Fish.Util.Position

let tests = OUnit2.(>:::) "board_config_tests" [

    OUnit2.(>::) "test_construction" (fun _ ->
        let conf = Conf.create ~width:3 ~height:4 in
        OUnit2.assert_equal 3 @@ Conf.get_width conf;
        OUnit2.assert_equal 4 @@ Conf.get_height conf;
        OUnit2.assert_equal [] @@ Conf.get_holes conf;
        OUnit2.assert_equal 0 @@ Conf.get_min_num_of_one_fish_tile conf;
        OUnit2.assert_equal 1 @@ Conf.get_default_num_of_fish conf;
      );

    OUnit2.(>::) "test_setter_getter" (fun _ ->
        (* Only 1 field changes *)
        let conf = Conf.create ~width:3 ~height:5 in

        let conf1 = Conf.set_width 10 conf in
        OUnit2.assert_equal 10 @@ Conf.get_width conf1;
        OUnit2.assert_equal 5 @@ Conf.get_height conf1;
        OUnit2.assert_equal [] @@ Conf.get_holes conf1;
        OUnit2.assert_equal 0 @@ Conf.get_min_num_of_one_fish_tile conf1;
        OUnit2.assert_equal 1 @@ Conf.get_default_num_of_fish conf1;

        let conf1 = Conf.set_height 8 conf in
        OUnit2.assert_equal 3 @@ Conf.get_width conf1;
        OUnit2.assert_equal 8 @@ Conf.get_height conf1;
        OUnit2.assert_equal [] @@ Conf.get_holes conf1;
        OUnit2.assert_equal 0 @@ Conf.get_min_num_of_one_fish_tile conf1;
        OUnit2.assert_equal 1 @@ Conf.get_default_num_of_fish conf1;

        let conf1 = Conf.set_min_num_of_one_fish_tile 7 conf in
        OUnit2.assert_equal 3 @@ Conf.get_width conf1;
        OUnit2.assert_equal 8 @@ Conf.get_height conf1;
        OUnit2.assert_equal [] @@ Conf.get_holes conf1;
        OUnit2.assert_equal 7 @@ Conf.get_min_num_of_one_fish_tile conf1;
        OUnit2.assert_equal 1 @@ Conf.get_default_num_of_fish conf1;

        let conf1 = Conf.set_default_num_of_fish 3 conf in
        OUnit2.assert_equal 3 @@ Conf.get_width conf1;
        OUnit2.assert_equal 8 @@ Conf.get_height conf1;
        OUnit2.assert_equal [] @@ Conf.get_holes conf1;
        OUnit2.assert_equal 0 @@ Conf.get_min_num_of_one_fish_tile conf1;
        OUnit2.assert_equal 3 @@ Conf.get_default_num_of_fish conf1;

        let holes = [ { P.row = 0; col = 2 };
                      { P.row = 3; col = 1 };
                      { P.row = 4; col = 5 };
                      { P.row = 1; col = 0 };
                    ]
        in
        let conf1 = Conf.set_holes holes conf in
        OUnit2.assert_equal 3 @@ Conf.get_width conf1;
        OUnit2.assert_equal 8 @@ Conf.get_height conf1;
        OUnit2.assert_equal holes @@ Conf.get_holes conf1;
        OUnit2.assert_equal 0 @@ Conf.get_min_num_of_one_fish_tile conf1;
        OUnit2.assert_equal 1 @@ Conf.get_default_num_of_fish conf1;

        (* original conf is unaffected *)
        OUnit2.assert_equal 3 @@ Conf.get_width conf;
        OUnit2.assert_equal 5 @@ Conf.get_height conf;
        OUnit2.assert_equal [] @@ Conf.get_holes conf;
        OUnit2.assert_equal 0 @@ Conf.get_min_num_of_one_fish_tile conf;
        OUnit2.assert_equal 1 @@ Conf.get_default_num_of_fish conf;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
