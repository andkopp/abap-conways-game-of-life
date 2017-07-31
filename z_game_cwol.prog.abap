REPORT z_game_cwol.

START-OF-SELECTION.

* TODO as input parameter
  DATA(lo_playground) = zcl_playground_factory=>get_instance( )->get_playground_dummy1( ).

  DO 5 TIMES.
    lo_playground->transform( ).

  ENDDO.
