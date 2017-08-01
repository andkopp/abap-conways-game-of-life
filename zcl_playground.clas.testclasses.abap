*"* use this source file for your ABAP unit test classes
*"* use this source file for your ABAP unit test classes
CLASS ltcl_playground DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_init_dummy1 FOR TESTING RAISING cx_static_check,
      test_size_dummy1 FOR TESTING RAISING cx_static_check,
      test_living_neighbour_dummy1 FOR TESTING RAISING cx_static_check,
      test_transform_dummy1 FOR TESTING RAISING cx_static_check,
      test_transform_tile FOR TESTING RAISING cx_static_check,
      test_transform_blinker FOR TESTING RAISING cx_static_check,
      test_alv_table_blinker FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_playground IMPLEMENTATION.

  METHOD test_init_dummy1.

* Init
    DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_dummy1( ).

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

  METHOD test_size_dummy1.

* Init
    DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_dummy1( ).

    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_width( )
            exp = 3
    ).

    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_height( )
            exp = 3
    ).

  ENDMETHOD.

  METHOD test_living_neighbour_dummy1.

* Init
    DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_dummy1( ).

* check the number of living neighbours
* assume that cells outside of the playground are dead
    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lo_playground->get_num_living_neighbours( x = 2 y = 2 )
            exp = 1
    ).

  ENDMETHOD.

  METHOD test_transform_dummy1.

    DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_dummy1( ).

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

    DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_tile( ).

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

    DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_blinker( ).

    DO 50 TIMES.

      lo_playground->transform( ).

* This blinker changes it state from a line (y=2) to a column (x=2)
* and then back again.
*
* 1st/3rd/5th... state
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


  METHOD test_alv_table_blinker.

    FIELD-SYMBOLS: <dyn_table> TYPE STANDARD TABLE.

    DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_blinker( ).

    DATA(lo_table) = lo_playground->get_as_alv_table( ).

    cl_abap_unit_assert=>assert_bound( lo_table ).

    ASSIGN lo_table->* TO <dyn_table>.

* Check line dimension
    cl_abap_unit_assert=>assert_equals(
        act = lines( <dyn_table> )
        exp = 3
    ).

* Check column dimension, and check if values are set correctly...
    LOOP AT <dyn_table> ASSIGNING FIELD-SYMBOL(<line>).     "y-axis
      DATA(lv_line_idx) = sy-tabix.
      DO 3 TIMES.                                           "x-axis
        DATA(lv_col_idx) = sy-index.
        ASSIGN COMPONENT |X{ lv_col_idx }| OF STRUCTURE <line> TO FIELD-SYMBOL(<comp>).
        cl_abap_unit_assert=>assert_subrc(
            exp = 0
            act = sy-subrc
        ).
        cl_abap_unit_assert=>assert_true(
            COND abap_bool(
                WHEN lv_line_idx = 1 AND lv_col_idx = 1 AND <comp> = zcl_cell=>dead  THEN abap_true
                WHEN lv_line_idx = 1 AND lv_col_idx = 2 AND <comp> = zcl_cell=>dead  THEN abap_true
                WHEN lv_line_idx = 1 AND lv_col_idx = 3 AND <comp> = zcl_cell=>dead  THEN abap_true
                WHEN lv_line_idx = 2 AND lv_col_idx = 1 AND <comp> = zcl_cell=>alive THEN abap_true
                WHEN lv_line_idx = 2 AND lv_col_idx = 2 AND <comp> = zcl_cell=>alive THEN abap_true
                WHEN lv_line_idx = 2 AND lv_col_idx = 3 AND <comp> = zcl_cell=>alive THEN abap_true
                WHEN lv_line_idx = 3 AND lv_col_idx = 1 AND <comp> = zcl_cell=>dead  THEN abap_true
                WHEN lv_line_idx = 3 AND lv_col_idx = 2 AND <comp> = zcl_cell=>dead  THEN abap_true
                WHEN lv_line_idx = 3 AND lv_col_idx = 3 AND <comp> = zcl_cell=>dead  THEN abap_true
                ELSE abap_false
            )
        ).
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
