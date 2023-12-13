*&---------------------------------------------------------------------*
*&  Include           ZAA23DT_DYNPROS_COT_DYN
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  SET PF-STATUS 'STATUS_9000'.
  SET TITLEBAR 'TITLE_9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CASE sy-ucomm.

    WHEN 'CREAR'.

      PERFORM crear_9000.

    WHEN 'MODIF'.

      PERFORM modif_9000.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_ENTREGA_9000  INPUT
*&---------------------------------------------------------------------*
MODULE VALIDAR_ENTREGA_9000 INPUT.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input         = s_9000-entrega
*   IMPORTING
*      OUTPUT        = s_9000-entrega.


  IF s_9000-entrega IS INITIAL.

    MESSAGE 'La Entrega no puede ser vacía' TYPE 'E'.

  ELSE.

    SELECT COUNT(*)
      FROM likp
     WHERE vbeln EQ s_9000-entrega.

    IF sy-subrc NE 0.
      MESSAGE 'La Entrega no existe' TYPE 'E'.
    ENDIF.

  ENDIF.

ENDMODULE.                 " VALIDAR_ENTREGA_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT_COMMAND INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.

      IF sy-dynnr EQ '9001'.

        CALL FUNCTION 'DEQUEUE_EZAAPS_COT_SH'
          EXPORTING
            vbeln = s_9001-entrega.

      ENDIF.

      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " USER_COMMAND_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  SET PF-STATUS 'STATUS_9001'.

  IF sy-ucomm EQ 'CREAR'.
    SET TITLEBAR 'TITLE_9001'.
  ELSEIF sy-ucomm EQ 'MODIF'.
    SET TITLEBAR 'TITLE_9001_M'.
  ENDIF.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  CASE sy-ucomm.

    WHEN 'SAVE'.

      PERFORM save_9001.

    WHEN 'POSICIONES'.

      PERFORM posiciones_9001.

  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_9001  OUTPUT
*&---------------------------------------------------------------------*
MODULE INIT_9001 OUTPUT.

  PERFORM init_9001.

ENDMODULE.                 " INIT_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDAR_WERKS_9001  INPUT
*&---------------------------------------------------------------------*
MODULE VALIDAR_WERKS_9001 INPUT.

  IF s_9001-werks IS INITIAL.

    MESSAGE 'El Centro no puede ser vacío' TYPE 'E'.

  ELSE.

    SELECT COUNT(*)
      FROM t001w
     WHERE werks EQ s_9001-werks.

    IF sy-subrc NE 0.
      MESSAGE 'El Centro no existe' TYPE 'E'.
    ENDIF.

  ENDIF.

ENDMODULE.                 " VALIDAR_WERKS_9001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9002 OUTPUT.

  SET PF-STATUS 'STATUS_9002'.
  SET TITLEBAR 'TITLE_9002'.

ENDMODULE.                 " STATUS_9002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_9002  OUTPUT
*&---------------------------------------------------------------------*
MODULE init_9002 OUTPUT.
  PERFORM init_9002.
ENDMODULE.                 " INIT_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  CASE sy-ucomm.

    WHEN 'SAVE'.

      PERFORM save_9002.

    WHEN 'AGREGAR'.

      PERFORM agregar_9002.

  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " USER_COMMAND_9002  INPUT