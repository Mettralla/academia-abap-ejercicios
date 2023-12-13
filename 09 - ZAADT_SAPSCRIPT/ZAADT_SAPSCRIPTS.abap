*&---------------------------------------------------------------------*
*& Report  ZAADT_SAPSCRIPT
*&---------------------------------------------------------------------*

REPORT  ZAADT_SAPSCRIPT.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_cabecera,
    copia             TYPE c,
    direccion_empresa TYPE string,
    punto_venta       TYPE string,
    comprobante       TYPE string,
    fecha_emision     TYPE d,
    cuit              TYPE string,
    ingresos_brutos   TYPE string,
    fecha_inicio_act  TYPE string,
  END OF ty_cabecera.

TYPES:
  BEGIN OF ty_posiciones,
    material TYPE vbrp-matnr,
    cantidad TYPE vbrp-fkimg,
    importe  TYPE vbrp-netwr,
  END OF ty_posiciones,

  tyt_posiciones TYPE STANDARD TABLE OF ty_posiciones.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*
DATA:
  e_cabecera   TYPE ty_cabecera,
  t_posiciones TYPE tyt_posiciones,
  e_posiciones TYPE ty_posiciones.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

PARAMETERS:
  p_vbeln TYPE vbrk-vbeln,
  p_copia TYPE i DEFAULT 1.

SELECTION-SCREEN END OF BLOCK b01.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM get_data
    CHANGING
      e_cabecera
      t_posiciones.

  PERFORM open_form.

  PERFORM write_form.

  PERFORM close_form.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data
  CHANGING
    ch_e_cabecera   TYPE ty_cabecera
    ch_t_posiciones TYPE tyt_posiciones.

*  FIELD-SYMBOLS:
*    <lfs_posiciones> LIKE LINE OF ch_t_posiciones.

  DATA:
    le_posiciones TYPE ty_posiciones,
    lv_contador   TYPE i VALUE 1.

  ch_e_cabecera-copia             = p_copia.
  ch_e_cabecera-direccion_empresa = 'Av Caseros 1234 - CP 1234'.
  ch_e_cabecera-punto_venta       = '00150'.
  ch_e_cabecera-comprobante       = '00123456'.
  ch_e_cabecera-fecha_emision     = '20210101'.
  ch_e_cabecera-cuit              = '2034270514'.
  ch_e_cabecera-ingresos_brutos   = '123456789-1'.
  ch_e_cabecera-fecha_inicio_act  = '01/01/1990'.

  SELECT matnr fkimg netwr
    FROM vbrp
    INTO TABLE ch_t_posiciones
   WHERE vbeln EQ p_vbeln.

  READ TABLE ch_t_posiciones INTO le_posiciones INDEX 1. " Leer por indice

  DO 10 TIMES.

    ADD 1 TO lv_contador.

    MULTIPLY le_posiciones-cantidad BY lv_contador.
    MULTIPLY le_posiciones-importe BY lv_contador.

    APPEND le_posiciones TO ch_t_posiciones.

  ENDDO.

*  " Para mostrar como funciona la MAIN pongo muchas líneas forzadas
*  READ TABLE ch_t_posiciones INTO le_posiciones INDEX 1.
*
*  DO 40 TIMES.
*    APPEND le_posiciones TO ch_t_posiciones.
*  ENDDO.

ENDFORM.                    "get_data

*&---------------------------------------------------------------------*
*&      Form  OPEN_FORM
*&---------------------------------------------------------------------*
FORM open_form.

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      form                        = 'ZAADT_SAPSCRIPT'
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      OPTIONS                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      codepage                    = 11
      OTHERS                      = 12.

  CALL FUNCTION 'START_FORM'
    EXCEPTIONS
      form        = 1
      format      = 2
      unended     = 3
      unopened    = 4
      unused      = 5
      spool_error = 6
      codepage    = 7
      OTHERS      = 8.

ENDFORM.                    " OPEN_FORM

*&---------------------------------------------------------------------*
*&      Form  WRITE_FORM
*&---------------------------------------------------------------------*
FORM write_form.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      window                   = 'ORIGINAL'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.

  LOOP AT t_posiciones INTO e_posiciones.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = 'IT' " Este elemento es el que usara el form para mostrar los items
        window                   = 'MAIN'
      EXCEPTIONS
        element                  = 1
        function                 = 2
        type                     = 3
        unopened                 = 4
        unstarted                = 5
        window                   = 6
        bad_pageformat_for_print = 7
        spool_error              = 8
        codepage                 = 9
        OTHERS                   = 10.

  ENDLOOP.

ENDFORM.                    " WRITE_FORM

*&---------------------------------------------------------------------*
*&      Form  CLOSE_FORM
*&---------------------------------------------------------------------*
FORM close_form.

  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      codepage                 = 5
      OTHERS                   = 6.

ENDFORM.                    " CLOSE_FORM

**&---------------------------------------------------------------------*
**&      Form  get_material_desc
**&---------------------------------------------------------------------*
*form get_material_desc
*  tables
*    itab structure itcsy
*    otab structure itcsy.
*
*  data:
*    le_itab       like line of itab,
*    lv_matnr      type vbrp-matnr,
*    lv_makt_maktx type makt-maktx.
*
*  field-symbols:
*    <lfs_itab> type itcsy.
*
*  read table itab into le_itab
*    with key name = 'E_POSICIONES-MATERIAL'.
*
*  if sy-subrc eq 0.
*    lv_matnr = le_itab-value.
*  endif.
*
*  call function 'CONVERSION_EXIT_MATN1_INPUT'
*    exporting
*      input        = lv_matnr
*    importing
*      output       = lv_matnr
*    exceptions
*      length_error = 1
*      others       = 2.
*
*  select single maktx
*    from makt
*    into lv_makt_maktx
*   where matnr eq lv_matnr
*     and spras eq sy-langu.
*
*  unassign <lfs_itab>.
*  read table otab assigning <lfs_itab>
*    with key name = 'V_MATERIAL_DESC'.
*
*  if <lfs_itab> is assigned.
*    <lfs_itab>-value = lv_makt_maktx.
*  endif.
*
*endform.                    "get_material_desc