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

    METHODS get_width
      RETURNING
        VALUE(rv_width) TYPE i.

    METHODS get_height
      RETURNING
        VALUE(rv_height) TYPE i.

    METHODS get_table_as_reference
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



CLASS ZCL_PLAYGROUND IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PLAYGROUND->GET_CELL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X                              TYPE        I
* | [--->] Y                              TYPE        I
* | [<-()] RO_CELL                        TYPE REF TO ZCL_CELL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cell.

* Old syntax: READ TABLE fields INTO DATA(ls_field) WITH KEY x = x y = y. IF sy-subrc = 0....
    TRY.
        ro_cell = fields[ x = x y = y ]-cell.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PLAYGROUND->GET_HEIGHT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_HEIGHT                      TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_height.
    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).
      CHECK <field>-y > rv_height.
      rv_height = <field>-y.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PLAYGROUND->GET_NEIGHBOURS
* +-------------------------------------------------------------------------------------------------+
* | [--->] X                              TYPE        I
* | [--->] Y                              TYPE        I
* | [<-()] RT_FIELDS                      TYPE        FIELD_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PLAYGROUND->GET_NUM_LIVING_NEIGHBOURS
* +-------------------------------------------------------------------------------------------------+
* | [--->] X                              TYPE        I
* | [--->] Y                              TYPE        I
* | [<-()] NUM                            TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PLAYGROUND->GET_TABLE_AS_REFERENCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_PLAYGROUND                  TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_table_as_reference.

* x-axis: one column per x
* y-axis: y = sy-tabix (reversed)
    FIELD-SYMBOLS: <dyn_table> TYPE STANDARD TABLE.
    DATA: gr_line TYPE REF TO data.

* Generate field catalog with dynamic dimensions
    DATA(lt_fcat) = VALUE lvc_t_fcat(
     FOR i = 1 THEN i + 1 UNTIL i > me->get_width( ) "= max(x)
      (
        fieldname = |X{ i }|      "Field Name
        outputlen = 8             "Output Length
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

* Populate table
    IF sy-subrc = 0.
      ASSIGN rt_playground->* TO <dyn_table>.
      CREATE DATA gr_line LIKE LINE OF <dyn_table>.
      ASSIGN gr_line->* TO FIELD-SYMBOL(<dyn_line>).

* We need a different order...
* y descending (highest y-value is the lowest sy-tabix)
* x ascending
      DATA(lv_y) = me->get_height( ).    "= max(y) = number of rows

      WHILE lv_y >= 1.
        DATA(lv_x) = 1.

        WHILE lv_x <= me->get_width( ).
          READ TABLE me->fields
            ASSIGNING FIELD-SYMBOL(<field>)
            WITH TABLE KEY x = lv_x
                           y = lv_y.
          IF sy-subrc = 0.
            ASSIGN COMPONENT |X{ lv_x }| OF STRUCTURE <dyn_line> TO FIELD-SYMBOL(<comp>).
            IF sy-subrc = 0.
              <comp> = <field>-cell->get_state( ).
              UNASSIGN <comp>.
            ENDIF.
          ENDIF.
          lv_x = lv_x + 1.
        ENDWHILE.

        APPEND <dyn_line> TO <dyn_table>.
        CLEAR <dyn_line>.
        lv_y = lv_y - 1.
      ENDWHILE.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PLAYGROUND->GET_WIDTH
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_WIDTH                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_width.

* Get distinct columns (x)
    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>)
        GROUP BY ( x = <field>-x ) ASCENDING
        WITHOUT MEMBERS ASSIGNING FIELD-SYMBOL(<unique_field>).
      CHECK <unique_field>-x > rv_width.
      rv_width = <unique_field>-x.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PLAYGROUND->SET_CELL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X                              TYPE        I
* | [--->] Y                              TYPE        I
* | [--->] STATE                          TYPE        ZCL_CELL=>T_STATE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_cell.

* Create instance with NEW() and insert it into table fields
    fields = VALUE #( BASE fields ( x = x y = y cell = NEW zcl_cell( iv_state = state ) ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PLAYGROUND->TRANSFORM
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_UNEXPECTED_CELL_STATE
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
