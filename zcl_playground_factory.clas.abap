CLASS zcl_playground_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_playground_factory.
    METHODS:
      get_playground_dummy1
        RETURNING
          VALUE(ro_playground) TYPE REF TO zcl_playground,
      get_playground_tile
        RETURNING
          VALUE(ro_playground) TYPE REF TO zcl_playground,
      get_playground_blinker
        RETURNING
          VALUE(ro_playground) TYPE REF TO zcl_playground.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_playground_factory IMPLEMENTATION.

  METHOD get_instance.
    ro_instance = NEW zcl_playground_factory( ).
  ENDMETHOD.

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

ENDCLASS.
