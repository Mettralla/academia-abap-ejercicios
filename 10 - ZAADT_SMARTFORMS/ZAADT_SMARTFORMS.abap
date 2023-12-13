*&---------------------------------------------------------------------*
*& Report  ZAADT_SMARTFORMS
*&---------------------------------------------------------------------*

REPORT  zaadt_smartforms.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*

DATA:
  s_cabecera   TYPE zaaps_cabecera,
  t_posiciones TYPE zaaps_posiciones_t,
  s_posiciones TYPE LINE OF zaaps_posiciones_t,
  lv_fm_name   TYPE rs38l_fnam,
  lv_mensaje   TYPE string.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

PARAMETERS:
  p_vbeln TYPE vbrk-vbeln.

SELECTION-SCREEN END OF BLOCK b01.

s_cabecera-factura = p_vbeln.
s_cabecera-original_text = 'ORIGINAL'.

s_cabecera-razon_social      = 'DL Consultores SA'.
s_cabecera-direccion_empresa = 'Av Caseros'.
s_cabecera-condicion_iva     = 'Responsable Inscripto'.

s_posiciones-material = '123'.
s_posiciones-cantidad = '10'.
s_posiciones-importe  = '123'.
ADD s_posiciones-importe TO s_cabecera-total.
APPEND s_posiciones TO t_posiciones.

s_posiciones-material = '456'.
s_posiciones-cantidad = '20'.
s_posiciones-importe  = '465'.
ADD s_posiciones-importe TO s_cabecera-total.
APPEND s_posiciones TO t_posiciones.

s_posiciones-material = '789'.
s_posiciones-cantidad = '30'.
s_posiciones-importe  = '798'.
ADD s_posiciones-importe TO s_cabecera-total.
APPEND s_posiciones TO t_posiciones.

" CONSIGUE EL NOMBRE DEL FORMULARIO
CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = 'ZAADT_SMARTFORMS'
  IMPORTING
    fm_name            = lv_fm_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.

IF sy-subrc EQ 0.

  " LLAMA EL FORMULARIO
  CALL FUNCTION lv_fm_name
    EXPORTING
      cabecera                   = s_cabecera
      posiciones                 = t_posiciones
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
   EXCEPTIONS
     formatting_error           = 1
     internal_error             = 2
     send_error                 = 3
     user_canceled              = 4
     OTHERS                     = 5.
*
*  IF sy-subrc NE 0.
*
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*          INTO lv_mensaje.
*
*    WRITE lv_mensaje.
*
*  ENDIF.
*
ELSE.
  MESSAGE 'Error' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.