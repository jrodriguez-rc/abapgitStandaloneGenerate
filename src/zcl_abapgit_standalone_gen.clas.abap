CLASS zcl_abapgit_standalone_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    METHODS generate
      RAISING
        zcx_abapgit_standalone_gen.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_standalone_gen IMPLEMENTATION.


  METHOD generate.

    CONSTANTS:
      lc_url_program_content     TYPE string
        VALUE 'https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap' ##NO_TEXT,
      lc_program_id              TYPE pgmid     VALUE 'R3TR' ##NO_TEXT,
      lc_object_type             TYPE trobjtype VALUE 'PROG' ##NO_TEXT,
      lc_object_name             TYPE sobj_name VALUE 'ZABAPGIT_STANDALONE' ##NO_TEXT,
      lc_default_master_language TYPE langu     VALUE 'E' ##NO_TEXT.

    DATA:
      source_abap TYPE abaptxt255_tab.

    cl_http_client=>create_by_url(
     EXPORTING
       url    = lc_url_program_content
     IMPORTING
       client = DATA(http_client)
     EXCEPTIONS
       OTHERS = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_standalone_gen=>raise_text( 'Cannot get source'(001) ).
    ENDIF.

    http_client->request->set_method( 'GET' ).

    http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_standalone_gen=>raise_text( 'Cannot get source'(001) ).
    ENDIF.

    http_client->receive(
      EXCEPTIONS
        OTHERS = 0 ).

    http_client->response->get_status(
      IMPORTING
        code   = DATA(code)
        reason = DATA(reason) ).

    IF code <> '200'.
      zcx_abapgit_standalone_gen=>raise_text( |{ 'Cannot get source'(001) }: { reason }| ).
    ENDIF.

    DATA(source_response) = http_client->response->get_cdata( ).

    SPLIT source_response AT zif_abapgit_definitions=>c_newline
        INTO TABLE source_abap.

    SELECT COUNT( * )
      FROM reposrc
      WHERE progname = lc_object_name.
    DATA(exists) = xsdbool( sy-subrc = 0 ).

    SELECT SINGLE masterlang
      INTO @DATA(master_language)
      FROM tadir
      WHERE pgmid    = @lc_program_id
        AND object   = @lc_object_type
        AND obj_name = @lc_object_name.
    DATA(language) = COND #( WHEN sy-subrc = 0 AND master_language IS NOT INITIAL
                                 THEN master_language
                                 ELSE lc_default_master_language ).

    TRY.

        zcl_abapgit_language=>set_current_language( language ).

        IF exists = abap_true.

          CALL FUNCTION 'RPY_PROGRAM_UPDATE'
            EXPORTING
              program_name     = lc_object_name
              title_string     = 'abapGit Standalone'
            TABLES
              source_extended  = source_abap
            EXCEPTIONS
              cancelled        = 1
              permission_error = 2
              not_found        = 3
              OTHERS           = 4.
          IF sy-subrc <> 0.

            IF sy-msgid = 'EU' AND sy-msgno = '510'.
              zcx_abapgit_standalone_gen=>raise_text( 'User is currently editing program'(002) ).
            ELSEIF sy-msgid = 'EU' AND sy-msgno = '522'.
              zcx_abapgit_standalone_gen=>raise_text( |{ 'Delete program and try again,'(003) } lc_object_name (EU522)| ).
            ELSE.
              zcx_abapgit_standalone_gen=>raise_system( ).
            ENDIF.

          ENDIF.

        ELSE.

          CALL FUNCTION 'RPY_PROGRAM_INSERT'
            EXPORTING
              development_class = '$TMP'
              program_name      = lc_object_name
              title_string      = 'abapGit Standalone'
              suppress_dialog   = abap_true
            TABLES
              source            = source_abap
            EXCEPTIONS
              already_exists    = 1
              cancelled         = 2
              name_not_allowed  = 3
              permission_error  = 4
              OTHERS            = 5.
          IF sy-subrc <> 0.
            zcx_abapgit_standalone_gen=>raise_system( ).
          ENDIF.

*          CALL FUNCTION 'RS_CORR_INSERT'
*            EXPORTING
*              object              = lc_object_name
*              object_class        = 'ABAP'
*              devclass            = '$TMP'
*              master_language     = language
*              mode                = 'I'
*              suppress_dialog     = abap_true
*            EXCEPTIONS
*              cancelled           = 1
*              permission_failure  = 2
*              unknown_objectclass = 3
*              OTHERS              = 4.
*          IF sy-subrc <> 0.
*            zcx_abapgit_standalone_gen=>raise_system( ).
*          ENDIF.
*
*          INSERT REPORT lc_object_name
*            FROM source_abap
*            STATE 'I'
*            PROGRAM TYPE '1'.
*          IF sy-subrc <> 0.
*            zcx_abapgit_standalone_gen=>raise_text( 'error from INSERT REPORT' ).
*          ENDIF.

        ENDIF.

      CLEANUP.
        zcl_abapgit_language=>restore_login_language( ).
    ENDTRY.

    zcl_abapgit_language=>restore_login_language( ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    TRY.
        NEW zcl_abapgit_standalone_gen( )->generate( ).
      CATCH zcx_abapgit_standalone_gen INTO DATA(exception).
        out->write( exception->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
