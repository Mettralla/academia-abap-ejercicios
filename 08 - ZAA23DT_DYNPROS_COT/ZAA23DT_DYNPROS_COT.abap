*&---------------------------------------------------------------------*
*& Modulpool         ZAA23DT_DYNPROS_COT
*&---------------------------------------------------------------------*

PROGRAM  ZAA23DT_DYNPROS_COT.

*&---------------------------------------------------------------------*
*&     INCLUDE
*&---------------------------------------------------------------------*

INCLUDE:
    ZAA23DT_DYNPROS_COT_TOP,
    ZAA23DT_DYNPROS_COT_DYN,
    ZAA23DT_DYNPROS_COT_TCW.

*&---------------------------------------------------------------------*
*&      Form  CREAR_9000
*&---------------------------------------------------------------------*
FORM CREAR_9000 .

  DATA:
    lv_confirma TYPE abap_bool,
    lv_mensaje  TYPE string.

  IF s_9000-entrega IS NOT INITIAL.

    PERFORM bloquear_entrega
      USING
        s_9000-entrega
      CHANGING
        lv_mensaje.

    IF lv_mensaje IS NOT INITIAL.

      MESSAGE lv_mensaje TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

    SELECT COUNT(*)
      FROM zaaps_cot
     WHERE vbeln EQ s_9000-entrega.

    IF sy-subrc NE 0.

      v_init_9001 = abap_true.
      CALL SCREEN 9001.

    ELSE.

      PERFORM popup_to_confirm
        USING
          'Los datos ya existen ¿Desea Modificarlos?'
        CHANGING
          lv_confirma.

      IF lv_confirma EQ abap_true.

        sy-ucomm = 'MODIF'.

        v_init_9001 = abap_true.
        CALL SCREEN 9001.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " CREAR_9000

*&---------------------------------------------------------------------*
*&      Form  MODIF_9000
*&---------------------------------------------------------------------*
FORM MODIF_9000.

  DATA:
    lv_confirma TYPE abap_bool,
    lv_mensaje  TYPE string.

  IF s_9000-entrega IS NOT INITIAL.

    PERFORM bloquear_entrega
      USING
        s_9000-entrega
      CHANGING
        lv_mensaje.

    IF lv_mensaje IS NOT INITIAL.

      MESSAGE lv_mensaje TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

    SELECT COUNT(*)
      FROM zaaps_cot
     WHERE vbeln EQ s_9000-entrega.

    IF sy-subrc EQ 0.

      v_init_9001 = abap_true.
      CALL SCREEN 9001.

    ELSE.

      PERFORM popup_to_confirm
        USING
          'Los datos no existen ¿Desea Crearlos?'
        CHANGING
          lv_confirma.

      IF lv_confirma EQ abap_true.

        sy-ucomm = 'CREAR'.

        v_init_9001 = abap_true.
        CALL SCREEN 9001.

      ENDIF.

    ENDIF.

  ENDIF.
ENDFORM.                    " MODIF_9000
*&---------------------------------------------------------------------*
*&      Form  SAVE_9001
*&---------------------------------------------------------------------*
FORM SAVE_9001.

  DATA:
    ls_zaaps_cot TYPE zaaps_cot.

  ls_zaaps_cot-mandt   = sy-mandt.
  ls_zaaps_cot-vbeln   = s_9001-entrega.
  ls_zaaps_cot-xblnr   = s_9001-xblnr.
  ls_zaaps_cot-werks   = s_9001-werks.
  ls_zaaps_cot-nro_cot = s_9001-nro_cot.
  ls_zaaps_cot-estado  = s_9001-estado.
  ls_zaaps_cot-fecha   = s_9001-fecha.
  ls_zaaps_cot-hora    = s_9001-hora.
  ls_zaaps_cot-usuario = s_9001-usuario.

  MODIFY zaaps_cot FROM ls_zaaps_cot.

  IF sy-subrc EQ 0.

    CALL FUNCTION 'DEQUEUE_EZAAPS_COT_SH' " Desbloquea registro
      EXPORTING
        vbeln = s_9001-entrega.

    MESSAGE 'Datos guardados' TYPE 'S'.
    LEAVE TO SCREEN 0.

  ELSE.

    MESSAGE 'Error al guardar. Intente nuevamente' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.                    " SAVE_9001
*&---------------------------------------------------------------------*
*&      Form  INIT_9001
*&---------------------------------------------------------------------*
FORM INIT_9001.

  IF v_init_9001 EQ abap_true.

    v_init_9001 = abap_false.

    s_9001-entrega = s_9000-entrega.

    s_9001-usuario = sy-uname.
    s_9001-fecha   = sy-datum.
    s_9001-hora    = sy-uzeit.

    PERFORM list_box_estado_9001.

    IF sy-ucomm EQ 'MODIF'.

      SELECT SINGLE xblnr werks nro_cot estado
        FROM zaaps_cot
        INTO (s_9001-xblnr, s_9001-werks, s_9001-nro_cot, s_9001-estado)
       WHERE vbeln EQ s_9001-entrega.

    ELSE.

      CLEAR:
        s_9001-xblnr,
        s_9001-werks,
        s_9001-werks_desc,
        s_9001-nro_cot,
        s_9001-estado.

    ENDIF.

  ENDIF.

  IF s_9001-werks IS NOT INITIAL.

    SELECT SINGLE name1
      FROM t001w
      INTO s_9001-werks_desc
     WHERE werks EQ s_9001-werks.

  ENDIF.

  CLEAR sy-ucomm.

ENDFORM.                    " INIT_9001
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_ESTADO_9001
*&---------------------------------------------------------------------*
FORM LIST_BOX_ESTADO_9001.

  DATA:
    lt_values    TYPE vrm_values,
    ls_values    LIKE LINE OF lt_values,
    lt_dd07v_tab TYPE STANDARD TABLE OF dd07v,
    ls_dd07v_tab TYPE dd07v.

  CALL FUNCTION 'DD_DOMVALUES_GET' " Recibe el nombre del dominio y crea una tabla con los valores
    EXPORTING
      domname        = 'ZAAPS_ESTADO' " Nombre del dominio
      text           = 'X' " Devuelve clave y texto
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_dd07v_tab
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  LOOP AT lt_dd07v_tab INTO ls_dd07v_tab.

    CLEAR ls_values.
    ls_values-key  = ls_dd07v_tab-domvalue_l.
    ls_values-text = ls_dd07v_tab-ddtext.
    APPEND ls_values TO lt_values.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES' " Funcion cargar elementos en listbox
    EXPORTING
      id              = 'S_9001-ESTADO'
      values          = lt_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


ENDFORM.                    " LIST_BOX_ESTADO_9001

*&---------------------------------------------------------------------*
*&      FORM popup_to_confirm
*&---------------------------------------------------------------------*
FORM popup_to_confirm
  USING
    pv_text_question TYPE any
  CHANGING
    pv_si            TYPE abap_bool.

  DATA:
    lv_answer TYPE c.

  pv_si = abap_false.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = pv_text_question
      text_button_1         = 'Sí'
      text_button_2         = 'No'
      default_button        = '2'
      display_cancel_button = abap_false
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF sy-subrc EQ 0.

    IF lv_answer EQ '1'. " Sí

      pv_si = abap_true.

    ENDIF.

  ENDIF.

ENDFORM. "popup_to_confirm

*&---------------------------------------------------------------------*
*&      Form  BLOQUEAR_ENTREGA
*&      Evita que pueda abrirse en dos lugares al mismo tiempo
*&---------------------------------------------------------------------*
FORM bloquear_entrega
  USING
    us_v_entrega
  CHANGING
    ch_v_mensaje.

  CLEAR ch_v_mensaje.

  CALL FUNCTION 'ENQUEUE_EZAAPS_COT_SH' " Solicita bloqueo enqueue para objeto &
    EXPORTING
      vbeln          = us_v_entrega
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO ch_v_mensaje.

  ENDIF.

ENDFORM.                    " BLOQUEAR_ENTREGA
*&---------------------------------------------------------------------*
*&      Form  INIT_9002
*&---------------------------------------------------------------------*
FORM INIT_9002.

  DATA:
    ls_9002_table    TYPE ty_9002_table,
    lt_zaaps_cot_pos TYPE STANDARD TABLE OF zaaps_cot_pos,
    ls_zaaps_cot_pos TYPE zaaps_cot_pos.

  IF v_init_9002 EQ abap_true.

    v_init_9002 = abap_false.

    REFRESH t_9002.

    s_9002-entrega = s_9001-entrega.

    SELECT *
      FROM zaaps_cot_pos
      INTO TABLE lt_zaaps_cot_pos
     WHERE vbeln EQ s_9002-entrega.

    IF sy-subrc EQ 0. " Si me trajo datos

      LOOP AT lt_zaaps_cot_pos INTO ls_zaaps_cot_pos.

        CLEAR ls_9002_table.

        ls_9002_table-posnr        = ls_zaaps_cot_pos-posnr.
        ls_9002_table-nro_cot      = ls_zaaps_cot_pos-nro_cot.
        ls_9002_table-material_cot = ls_zaaps_cot_pos-material_cot.
        ls_9002_table-unidad_cot   = ls_zaaps_cot_pos-unidad_cot.

        APPEND ls_9002_table TO t_9002.

      ENDLOOP.

    ELSE.

      CLEAR ls_9002_table.
      APPEND ls_9002_table TO t_9002.

    ENDIF.

  ENDIF.

ENDFORM.                    " INIT_9002
*&---------------------------------------------------------------------*
*&      Form  SAVE_9002
*&---------------------------------------------------------------------*
FORM SAVE_9002.

  DATA:
    lt_zaaps_cot_pos TYPE STANDARD TABLE OF zaaps_cot_pos,
    ls_zaaps_cot_pos TYPE zaaps_cot_pos,
    lv_posicion      TYPE zaaps_cot_pos-posnr.

  LOOP AT t_9002 INTO s_9002_table.

    IF s_9002_table IS INITIAL. " Si la linea es vacia no lo guarda
      CONTINUE.
    ENDIF.

    CLEAR ls_zaaps_cot_pos.

    ADD 10 TO lv_posicion.

    ls_zaaps_cot_pos-vbeln        = s_9002-entrega.
    ls_zaaps_cot_pos-nro_cot      = s_9002_table-nro_cot.
    ls_zaaps_cot_pos-material_cot = s_9002_table-material_cot.
    ls_zaaps_cot_pos-unidad_cot   = s_9002_table-unidad_cot.

    IF s_9002_table-posnr IS NOT INITIAL.
      ls_zaaps_cot_pos-posnr = s_9002_table-posnr.
    ELSE.
      ls_zaaps_cot_pos-posnr = lv_posicion.
    ENDIF.

    APPEND ls_zaaps_cot_pos TO lt_zaaps_cot_pos.

  ENDLOOP.

  CHECK lt_zaaps_cot_pos[] IS NOT INITIAL.

  MODIFY zaaps_cot_pos FROM TABLE lt_zaaps_cot_pos.

  IF sy-subrc EQ 0.

    MESSAGE 'Datos guardados' TYPE 'S'.
    LEAVE TO SCREEN 0.

  ELSE.

    MESSAGE 'Error al guardar. Intente nuevamente' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.                    " SAVE_9002
*&---------------------------------------------------------------------*
*&      Form  POSICIONES_9001
*&---------------------------------------------------------------------*
FORM POSICIONES_9001.

  v_init_9002 = abap_true.

  CALL SCREEN 9002.

ENDFORM.                    " POSICIONES_9001
*&---------------------------------------------------------------------*
*&      Form  AGREGAR_9002
*&---------------------------------------------------------------------*
FORM AGREGAR_9002.

  DATA:
    ls_9002_table TYPE ty_9002_table.

  CLEAR ls_9002_table.
  APPEND ls_9002_table TO t_9002.

ENDFORM.                    " AGREGAR_9002

ANA PEREZ