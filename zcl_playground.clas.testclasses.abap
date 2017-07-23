*"* use this source file for your ABAP unit test classes
*"* use this source file for your ABAP unit test classes
CLASS ltcl_playground DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      get_playground_dummy1
        RETURNING
          VALUE(ro_playground) TYPE REF TO zcl_playground,
      get_playground_tile
        RETURNING
          VALUE(ro_playground) TYPE REF TO zcl_playground,
      get_playground_blinker
        RETURNING
          VALUE(ro_playground) TYPE REF TO zcl_playground,
      test_init_dummy1 FOR TESTING RAISING cx_static_check,
      test_living_neighbour_dummy1 FOR TESTING RAISING cx_static_check,
      test_transform_dummy1 FOR TESTING RAISING cx_static_check,
      test_transform_tile FOR TESTING RAISING cx_static_check,
      test_transform_blinker FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_playground IMPLEMENTATION.

  METHOD get_playground_dummy1.

* 0 | 0 | 0
* ---------
* 0 | X | 0
* ----------
* 0 | 0 | X
*
* 0 = dead
* X = alive

    ro_playground  = NEW zcl_playground( ).

* row 3
    ro_playground->set_cell( x = 1 y = 3 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 2 y = 3 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 3 y = 3 state = zcl_cell=>dead ).

* row 2
    ro_playground->set_cell( x = 1 y = 2 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 2 y = 2 state = zcl_cell=>alive ).
    ro_playground->set_cell( x = 3 y = 2 state = zcl_cell=>dead ).

* row 1
    ro_playground->set_cell( x = 1 y = 1 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 2 y = 1 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 3 y = 1 state = zcl_cell=>alive ).

  ENDMETHOD.

  METHOD get_playground_tile.

* 0 | X | 0
* ---------
* X | 0 | X
* ----------
* 0 | X | 0
*
* 0 = dead
* X = alive

    ro_playground  = NEW zcl_playground( ).

* row 3
    ro_playground->set_cell( x = 1 y = 3 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 2 y = 3 state = zcl_cell=>alive ).
    ro_playground->set_cell( x = 3 y = 3 state = zcl_cell=>dead ).

* row 2
    ro_playground->set_cell( x = 1 y = 2 state = zcl_cell=>alive ).
    ro_playground->set_cell( x = 2 y = 2 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 3 y = 2 state = zcl_cell=>alive ).

* row 1
    ro_playground->set_cell( x = 1 y = 1 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 2 y = 1 state = zcl_cell=>alive ).
    ro_playground->set_cell( x = 3 y = 1 state = zcl_cell=>dead ).

  ENDMETHOD.

  METHOD get_playground_blinker.

* 0 | 0 | 0
* ---------
* X | X | X
* ----------
* 0 | 0 | 0
*
* 0 = dead
* X = alive

    ro_playground  = NEW zcl_playground( ).

* row 3
    ro_playground->set_cell( x = 1 y = 3 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 2 y = 3 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 3 y = 3 state = zcl_cell=>dead ).

* row 2
    ro_playground->set_cell( x = 1 y = 2 state = zcl_cell=>alive ).
    ro_playground->set_cell( x = 2 y = 2 state = zcl_cell=>alive ).
    ro_playground->set_cell( x = 3 y = 2 state = zcl_cell=>alive ).

* row 1
    ro_playground->set_cell( x = 1 y = 1 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 2 y = 1 state = zcl_cell=>dead ).
    ro_playground->set_cell( x = 3 y = 1 state = zcl_cell=>dead ).

  ENDMETHOD.

  METHOD test_init_dummy1.

* Init
    DATA(lo_playground) = get_playground_dummy1( ).

* check if the playground was correctly initialized
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 1 y = 3 )->get_state( )
            exp = zcl_cell=>dead
    ).

    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 3 y = 1 )->get_state( )
            exp = zcl_cell=>alive
    ).

  ENDMETHOD.

  METHOD test_living_neighbour_dummy1.

* Init
    DATA(lo_playground) = get_playground_dummy1( ).

* check the number of living neighbours
* assume that cells outside of the playground are dead
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_num_living_neighbours( x = 2 y = 2 )
            exp = 1
    ).

  ENDMETHOD.

  METHOD test_transform_dummy1.

    DATA(lo_playground) = get_playground_dummy1( ).

    lo_playground->transform( ).

* Expected result:
* 0 | 0 | 0
* ---------
* 0 | 0 | 0
* ----------
* 0 | 0 | 0
*
* 0 = dead
* X = alive

* row 3
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 1 y = 3 )->get_state( )
            exp = zcl_cell=>dead
    ).
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 2 y = 3 )->get_state( )
            exp = zcl_cell=>dead
    ).
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 3 y = 3 )->get_state( )
            exp = zcl_cell=>dead
    ).

* row 2
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 1 y = 2 )->get_state( )
            exp = zcl_cell=>dead
    ).
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 2 y = 2 )->get_state( )
            exp = zcl_cell=>dead
    ).
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 3 y = 2 )->get_state( )
            exp = zcl_cell=>dead
    ).

* row 1
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 1 y = 1 )->get_state( )
            exp = zcl_cell=>dead
    ).
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 2 y = 1 )->get_state( )
            exp = zcl_cell=>dead
    ).
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_cell( x = 3 y = 1 )->get_state( )
            exp = zcl_cell=>dead
    ).

  ENDMETHOD.

  METHOD test_transform_tile.

    DATA(lo_playground) = get_playground_tile( ).

    DO 10 TIMES.

      lo_playground->transform( ).

* A tile doesn't change on transformation.
*
* Expected result:
* 0 | X | 0
* ---------
* X | 0 | X
* ----------
* 0 | X | 0
*
* 0 = dead
* X = alive

* row 3
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 1 y = 3 )->get_state( )
              exp = zcl_cell=>dead
      ).
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 2 y = 3 )->get_state( )
              exp = zcl_cell=>alive
      ).
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 3 y = 3 )->get_state( )
              exp = zcl_cell=>dead
      ).

* row 2
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 1 y = 2 )->get_state( )
              exp = zcl_cell=>alive
      ).
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 2 y = 2 )->get_state( )
              exp = zcl_cell=>dead
      ).
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 3 y = 2 )->get_state( )
              exp = zcl_cell=>alive
      ).

* row 1
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 1 y = 1 )->get_state( )
              exp = zcl_cell=>dead
      ).
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 2 y = 1 )->get_state( )
              exp = zcl_cell=>alive
      ).
      cl_abap_unit_assert=>assert_equals(
          EXPORTING
              act = lo_playground->get_cell( x = 3 y = 1 )->get_state( )
              exp = zcl_cell=>dead
      ).

    ENDDO.

  ENDMETHOD.

  METHOD test_transform_blinker.

    DATA(lo_playground) = get_playground_blinker( ).

    DO 50 TIMES.

      lo_playground->transform( ).

* This blinker changes it state from a line (y=2) to a column (x=2)
* and then back again.
*
* 1st/2nd/3rd... state
* 0 | 0 | 0
* ---------
* X | X | X
* ----------
* 0 | 0 | 0
*
* 2nd/4th/... state
* 0 | X | 0
* ---------
* 0 | X | 0
* ----------
* 0 | X | 0
*
* 0 = dead
* X = alive

      CASE sy-index MOD 2.
        WHEN 0.

* row 3
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 1 y = 3 )->get_state( )
                  exp = zcl_cell=>dead
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 2 y = 3 )->get_state( )
                  exp = zcl_cell=>dead
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 3 y = 3 )->get_state( )
                  exp = zcl_cell=>dead
          ).

* row 2
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 1 y = 2 )->get_state( )
                  exp = zcl_cell=>alive
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 2 y = 2 )->get_state( )
                  exp = zcl_cell=>alive
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 3 y = 2 )->get_state( )
                  exp = zcl_cell=>alive
          ).

* row 1
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 1 y = 1 )->get_state( )
                  exp = zcl_cell=>dead
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 2 y = 1 )->get_state( )
                  exp = zcl_cell=>dead
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 3 y = 1 )->get_state( )
                  exp = zcl_cell=>dead
          ).

        WHEN OTHERS.

* row 3
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 1 y = 3 )->get_state( )
                  exp = zcl_cell=>dead
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 2 y = 3 )->get_state( )
                  exp = zcl_cell=>alive
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 3 y = 3 )->get_state( )
                  exp = zcl_cell=>dead
          ).

* row 2
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 1 y = 2 )->get_state( )
                  exp = zcl_cell=>dead
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 2 y = 2 )->get_state( )
                  exp = zcl_cell=>alive
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 3 y = 2 )->get_state( )
                  exp = zcl_cell=>dead
          ).

* row 1
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 1 y = 1 )->get_state( )
                  exp = zcl_cell=>dead
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 2 y = 1 )->get_state( )
                  exp = zcl_cell=>alive
          ).
          cl_abap_unit_assert=>assert_equals(
              EXPORTING
                  act = lo_playground->get_cell( x = 3 y = 1 )->get_state( )
                  exp = zcl_cell=>dead
          ).

      ENDCASE.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
