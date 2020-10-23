CLASS zcx_abapgit_standalone_gen DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_t100_message.

    TYPES:
      BEGIN OF ts_message_vars,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF ts_message_vars.

    DATA:
      text1 TYPE string READ-ONLY,
      text2 TYPE string READ-ONLY,
      text3 TYPE string READ-ONLY,
      text4 TYPE string READ-ONLY.

    CLASS-METHODS raise_system
      RAISING
        zcx_abapgit_standalone_gen.

    CLASS-METHODS raise_text
      IMPORTING
        text TYPE csequence
      RAISING
        zcx_abapgit_standalone_gen.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !text1    LIKE text1 OPTIONAL
        !text2    LIKE text2 OPTIONAL
        !text3    LIKE text3 OPTIONAL
        !text4    LIKE text4 OPTIONAL
        !previous LIKE previous OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS split_text
      IMPORTING
        text          TYPE csequence
      RETURNING
        VALUE(result) TYPE ts_message_vars.

ENDCLASS.



CLASS zcx_abapgit_standalone_gen IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->text1 = text1.
    me->text2 = text2.
    me->text3 = text3.
    me->text4 = text4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_system.

    RAISE EXCEPTION TYPE zcx_abapgit_standalone_gen
      EXPORTING
        textid = VALUE scx_t100key( msgid = sy-msgid
                                    msgno = sy-msgno
                                    attr1 = 'TEXT1'
                                    attr2 = 'TEXT2'
                                    attr3 = 'TEXT3'
                                    attr4 = 'TEXT4' )
        text1  = CONV #( sy-msgv1 )
        text2  = CONV #( sy-msgv2 )
        text3  = CONV #( sy-msgv3 )
        text4  = CONV #( sy-msgv4 ).

  ENDMETHOD.


  METHOD raise_text.

    DATA(text_in_vars) = split_text( text ).

    RAISE EXCEPTION TYPE zcx_abapgit_standalone_gen
      EXPORTING
        textid = VALUE scx_t100key( msgid = 'OO'
                                    msgno = '000'
                                    attr1 = 'TEXT1'
                                    attr2 = 'TEXT2'
                                    attr3 = 'TEXT3'
                                    attr4 = 'TEXT4' )
        text1  = CONV #( text_in_vars-msgv1 )
        text2  = CONV #( text_in_vars-msgv2 )
        text3  = CONV #( text_in_vars-msgv3 )
        text4  = CONV #( text_in_vars-msgv4 ).

  ENDMETHOD.


  METHOD split_text.
    result = text.
  ENDMETHOD.


ENDCLASS.
