*&---------------------------------------------------------------------*
*& Report zabapgit_standalone_generate
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_standalone_generate.

START-OF-SELECTION.
  PERFORM run.

FORM run.
  TRY.
      NEW zcl_abapgit_standalone_gen( )->generate( ).
    CATCH zcx_abapgit_standalone_gen INTO DATA(exception).
      MESSAGE exception TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.
