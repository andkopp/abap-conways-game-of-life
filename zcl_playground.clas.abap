CLASS zcl_playground DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF t_field,
             x    TYPE i,                   "bottom left is x = 1
             y    TYPE i,                   "bottom left is y = 1
             cell TYPE REF TO zcl_cell,
           END OF t_field.
    TYPES: field_table TYPE SORTED TABLE OF t_field WITH UNIQUE KEY x y.

    METHODS set_cell
      IMPORTING
        x     TYPE i
        y     TYPE i
        state TYPE zcl_cell=>t_state.

    METHODS transform
      RAISING
        zcx_unexpected_cell_state.

    METHODS get_cell
      IMPORTING
        x              TYPE i
        y              TYPE i
      RETURNING
        VALUE(ro_cell) TYPE REF TO zcl_cell.

    METHODS get_num_living_neighbours
      IMPORTING
        x          TYPE i
        y          TYPE i
      RETURNING
        VALUE(num) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: fields TYPE field_table.

    METHODS get_neighbours
      IMPORTING
        x                TYPE i
        y                TYPE i
      RETURNING
        VALUE(rt_fields) TYPE field_table.
ENDCLASS.



CLASS zcl_playground IMPLEMENTATION.


  METHOD set_cell.

* Create instance with NEW() and insert it into table fields
    fields = VALUE #( BASE fields ( x = x y = y cell = NEW zcl_cell( iv_state = state ) ) ).

  ENDMETHOD.


  METHOD get_cell.

* Old syntax: READ TABLE fields INTO DATA(ls_field) WITH KEY x = x y = y. IF sy-subrc = 0....
    TRY.
        ro_cell = fields[ x = x y = y ]-cell.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD get_neighbours.

* Each cell can have 8 neighbours with the following relative coordinates:
* (x-1/y+1) | x/(y+1) | (x+1)/(y+1)
* ---------------------------------
* (x-1/y)   | x/y     | (x+1)/y
* ---------------------------------
* (x-1/y-1) | x/(y-1) | (x+1)/(y-1)

* Path (number = sy-index of following do..enddo loop):
* 3 | 4 | 5
* ---------
* 2 |   | 6
* ----------
* 1 | 8 | 7

    DATA(lv_y) = y - 1.
    DATA(lv_x) = x - 1.

    DO 8 TIMES.
      lv_y = SWITCH #( sy-index
        WHEN 2 THEN ( lv_y + 1 )    "go up
        WHEN 3 THEN ( lv_y + 1 )    "go up
        WHEN 6 THEN ( lv_y - 1 )    "go down
        WHEN 7 THEN ( lv_y - 1 )    "go down
        ELSE lv_y
      ).

      lv_x = SWITCH #( sy-index
        WHEN 4 THEN ( lv_x + 1 )    "go right
        WHEN 5 THEN ( lv_x + 1 )    "go right
        WHEN 8 THEN ( lv_x - 1 )    "go left
        ELSE lv_x
      ).

* Check if line exists in internal table and insert into returning table
      CHECK line_exists( me->fields[ x = lv_x  y = lv_y ] ).
      rt_fields = VALUE #( BASE rt_fields ( me->fields[ x = lv_x  y = lv_y ] ) ).

    ENDDO.

  ENDMETHOD.


  METHOD get_num_living_neighbours.

    DATA(neighbours) = me->get_neighbours( x = x y = y ).

* With table expression REDUCE()
    num = REDUCE i( INIT cnt = 0 FOR line IN neighbours NEXT cnt = COND #(
                      WHEN line-cell->get_state( ) = zcl_cell=>alive
                      THEN cnt + 1
                      ELSE cnt ) ).

* Old Syntax
*    LOOP AT neighbours INTO DATA(neighbour).
*      IF neighbour-cell->get_state( ) = zcl_cell=>alive.
*        num = num + 1.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.


  METHOD transform.

    DATA: lt_new_fields LIKE me->fields.

    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).

* Calculate living neighbours
      DATA(lv_num_living_neighbours) = me->get_num_living_neighbours(
          x = <field>-x
          y = <field>-y
      ).

* Solution with COND()
      DATA(lv_new_state) = COND zcl_cell=>t_state(
        WHEN <field>-cell->get_state( ) = zcl_cell=>alive AND lv_num_living_neighbours = 0 THEN zcl_cell=>dead
        WHEN <field>-cell->get_state( ) = zcl_cell=>alive AND lv_num_living_neighbours = 1 THEN zcl_cell=>dead
        WHEN <field>-cell->get_state( ) = zcl_cell=>alive AND lv_num_living_neighbours = 2 THEN zcl_cell=>alive
        WHEN <field>-cell->get_state( ) = zcl_cell=>alive AND lv_num_living_neighbours = 3 THEN zcl_cell=>alive
        WHEN <field>-cell->get_state( ) = zcl_cell=>alive AND lv_num_living_neighbours > 3 THEN zcl_cell=>dead
        WHEN <field>-cell->get_state( ) = zcl_cell=>dead AND lv_num_living_neighbours < 3 THEN zcl_cell=>dead
        WHEN <field>-cell->get_state( ) = zcl_cell=>dead AND lv_num_living_neighbours = 3 THEN zcl_cell=>alive
        WHEN <field>-cell->get_state( ) = zcl_cell=>dead AND lv_num_living_neighbours > 3 THEN zcl_cell=>dead
        ELSE THROW zcx_unexpected_cell_state( )
      ).

** Solution with nested(2) SWITCH()
** COND() seems to be a better solution because you can use GT and LT
*      DATA(lv_new_state) = SWITCH zcl_cell=>t_state( <field>-cell->get_state( )
*        WHEN zcl_cell=>dead THEN SWITCH zcl_cell=>t_state( lv_num_living_neighbours
*          WHEN 1 OR 2 THEN zcl_cell=>dead
*          WHEN 3 THEN zcl_cell=>alive
*          WHEN 4 OR 5 OR 6 OR 7 OR 8 THEN zcl_cell=>dead
*        )
*        WHEN zcl_cell=>alive THEN SWITCH zcl_cell=>t_state( lv_num_living_neighbours
*          WHEN 0 OR 1 THEN zcl_cell=>dead
*          WHEN 2 OR 3 THEN zcl_cell=>alive
*          WHEN 4 OR 5 OR 6 OR 7 OR 8 THEN zcl_cell=>dead
*          ELSE THROW zcx_unexpected_cell_state( )
*        )
*      ).

* Compare state - and create new cell if state changed
      DATA(lo_new_cell) = COND #(
        WHEN <field>-cell->get_state( ) = lv_new_state THEN <field>-cell
        WHEN <field>-cell->get_state( ) <> lv_new_state THEN NEW zcl_cell( iv_state = lv_new_state )
        ELSE THROW zcx_unexpected_cell_state( )
      ).

      INSERT VALUE #( cell = lo_new_cell ) INTO TABLE lt_new_fields.
* Or: lt_new_fields = value #( base lt_new_fields ( cell = lo_new_cell ) ).

    ENDLOOP.

* Replace old fields
    me->fields = lt_new_fields.

  ENDMETHOD.

ENDCLASS.
