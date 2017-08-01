CLASS zcl_cell DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: t_state TYPE string.

* TODO Replace by Enum with NetWeaver 7.51 so the environment checks value
* -> no text needed then
    CONSTANTS: alive TYPE t_state VALUE 'alive',
               dead  TYPE t_state VALUE 'dead'.

    METHODS constructor
      IMPORTING
        iv_state TYPE t_state.

    METHODS get_state
      RETURNING
        VALUE(rv_state) TYPE t_state.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA state TYPE t_state.

ENDCLASS.



CLASS zcl_cell IMPLEMENTATION.

  METHOD constructor.
    me->state = iv_state.
  ENDMETHOD.

  METHOD get_state.
    rv_state = me->state.
  ENDMETHOD.
ENDCLASS.
