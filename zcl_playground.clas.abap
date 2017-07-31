CLASS zcl_playground DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF t_field,
             x    TYPE i,
             y    TYPE i,
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

    METHODS get_width
      RETURNING
        VALUE(rv_width) TYPE i.

    METHODS get_height
      RETURNING
        VALUE(rv_height) TYPE i.

    METHODS get_as_alv_table
      RETURNING
        VALUE(rt_playground) TYPE REF TO data.

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

    DATA(lo_cell) = NEW zcl_cell( iv_state = state ).
    INSERT VALUE t_field( x = x y = y cell = lo_cell ) INTO TABLE fields.

  ENDMETHOD.


  METHOD get_cell.

    READ TABLE fields
      INTO DATA(ls_field)
      WITH KEY x = x
               y = y.
    IF sy-subrc = 0.
      ro_cell = ls_field-cell.
    ENDIF.

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

      READ TABLE me->fields
              INTO DATA(ls_field)
              WITH KEY x = lv_x
                       y = lv_y.
      IF sy-subrc = 0.
        INSERT ls_field INTO TABLE rt_fields.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD get_num_living_neighbours.

    DATA(neighbours) = me->get_neighbours( x = x y = y ).

* how to do this with "FOR"?
    LOOP AT neighbours INTO DATA(neighbour).
      IF neighbour-cell->get_state( ) = zcl_cell=>alive.
        num = num + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD transform.

    DATA: lt_new_fields LIKE me->fields.

    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).

* Calculate living neighbours
      DATA(lv_num_living_neighbours) = me->get_num_living_neighbours(
          x = <field>-x
          y = <field>-y
      ).

* Derive new state
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


* Nested switch possible?
*      data(lv_state) = switch zcl_cell=>t_state( <cell>-cell->get_state( )
*        when zcl_cell=>dead then abap_true
*        when zcl_cell=>alive then ( SWITCH zcl_cell=>t_state( lv_num_living_neighbours
*        WHEN 0 OR 1 THEN zcl_cell=>dead
*        WHEN 2 OR 3 THEN zcl_cell=>alive
*        WHEN 4 OR 5 OR 6 OR 7 OR 8 THEN zcl_cell=>dead
*        ELSE THROW zcx_unexpected_cell_state( )
*      ) )
*      ).
*
* Single switch works...
*      DATA(lv_new_state) = SWITCH zcl_cell=>t_state( lv_num_living_neighbours
*        WHEN 0 OR 1 THEN zcl_cell=>dead
*        WHEN 2 OR 3 THEN zcl_cell=>alive
*        WHEN 4 OR 5 OR 6 OR 7 OR 8 THEN zcl_cell=>dead
*        ELSE THROW zcx_unexpected_cell_state( )
*      ).

* Compare state - and create new cell if state changed
      DATA(lo_new_cell) = COND #(
        WHEN <field>-cell->get_state( ) = lv_new_state THEN <field>-cell
        WHEN <field>-cell->get_state( ) <> lv_new_state THEN NEW zcl_cell( iv_state = lv_new_state )
        ELSE THROW zcx_unexpected_cell_state( )
      ).

      DATA(ls_new_field) = <field>.
      ls_new_field-cell = lo_new_cell.

      INSERT ls_new_field INTO TABLE lt_new_fields.

    ENDLOOP.

* Replace old fields
    me->fields = lt_new_fields.

  ENDMETHOD.

  METHOD get_width.

* Get distinct columns (x)
    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>)
        GROUP BY ( x = <field>-x ) ASCENDING
        WITHOUT MEMBERS ASSIGNING FIELD-SYMBOL(<unique_field>).
      CHECK <unique_field>-x > rv_width.
      rv_width = <unique_field>-x.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_height.
    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).
      CHECK <field>-y > rv_height.
      rv_height = <field>-y.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_as_alv_table.

    FIELD-SYMBOLS: <dyn_table> TYPE STANDARD TABLE.
    DATA: gr_line TYPE REF TO data.

* Generate field catalog with dynamic dimensions
    DATA(lt_fcat) = VALUE lvc_t_fcat(
     FOR i = 1 THEN i + 1 UNTIL i = me->get_width( ) "= max(x)
      (
        fieldname = |X{ i }|      "Field Name
        outputlen = i + 4         "Output Length
        tabname   = 'GT_DATA'     "Internal Table Name
        coltext   = |x = { i }|   "Header Text for the Column
        col_pos   = i             "Column position
        key       = abap_false    "Flag Key field
      )
    ).

* Create table
    cl_alv_table_create=>create_dynamic_table(
        EXPORTING
            it_fieldcatalog         = lt_fcat
            i_style_table           = abap_true
        IMPORTING
          ep_table                  = rt_playground
        EXCEPTIONS
          generate_subpool_dir_full = 1
          OTHERS                    = 2
    ).

* TODO: Populate table...
    IF sy-subrc = 0.
      ASSIGN rt_playground->* TO <dyn_table>.
      CREATE DATA gr_line LIKE LINE OF <dyn_table>.
      ASSIGN gr_line->* TO FIELD-SYMBOL(<dyn_line>).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
