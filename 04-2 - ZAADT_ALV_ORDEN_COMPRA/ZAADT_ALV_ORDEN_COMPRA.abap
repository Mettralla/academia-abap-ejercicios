*&---------------------------------------------------------------------*
*& Report  ZAADT_ALV_ORDEN_COMPRA
*&---------------------------------------------------------------------*

REPORT  zaadt_alv_orden_compra.

*&---------------------------------------------------------------------*
*& INCLUDE
*&---------------------------------------------------------------------*
INCLUDE:
  zaadt_alv_orden_compra_top.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

SELECT-OPTIONS:
   s_ebeln FOR ekko-ebeln,
   s_bukrs FOR ekko-bukrs NO INTERVALS,
   s_bsart FOR ekko-bsart NO INTERVALS,
   s_bedat FOR ekko-bedat.

SELECTION-SCREEN END OF BLOCK b01.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*

END-OF-SELECTION.

  PERFORM get_data
    CHANGING
      t_alv[].

  IF t_alv[] IS NOT INITIAL.

    PERFORM show_report
      USING
        t_alv[].

  ELSE.

    MESSAGE 'No hay datos para los parámetros de selección' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Carga la tabla ALV usando las tablas locales.
*----------------------------------------------------------------------*
*       <-- CH_T_ALV
*----------------------------------------------------------------------*
FORM get_data
  CHANGING
    ch_t_alv TYPE tyt_alv.

  DATA:
    lt_ekko  TYPE tyt_ekko,
    lt_t161t TYPE tyt_t161t,
    lt_lfa1  TYPE tyt_lfa1,
    lt_ekpo  TYPE tyt_ekpo,
    lt_makt  TYPE tyt_makt,
    lt_eket  TYPE tyt_eket,
    lt_konv  TYPE tyt_konv.

  PERFORM get_data_db
    CHANGING
      lt_ekko[]
      lt_t161t[]
      lt_lfa1[]
      lt_ekpo[]
      lt_makt[]
      lt_eket[]
      lt_konv[].

  PERFORM process_data
    USING
      lt_ekko[]
      lt_t161t[]
      lt_lfa1[]
      lt_ekpo[]
      lt_makt[]
      lt_eket[]
      lt_konv[]
    CHANGING
      ch_t_alv[].

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EKKO  text
*----------------------------------------------------------------------*
FORM get_data_db
  CHANGING
    ch_t_ekko  TYPE tyt_ekko
    ch_t_t161t TYPE tyt_t161t
    ch_t_lfa1  TYPE tyt_lfa1
    ch_t_ekpo  TYPE tyt_ekpo
    ch_t_makt  TYPE tyt_makt
    ch_t_eket  TYPE tyt_eket
    ch_t_konv  TYPE tyt_konv.

  DATA:
    le_ekko     TYPE LINE OF tyt_ekko,
    le_ekpo     TYPE LINE OF tyt_ekpo,
    le_makt_aux TYPE LINE OF tyt_makt,
    le_konv_aux TYPE LINE OF tyt_konv,
    le_aux      TYPE LINE OF tyt_konv,
    lt_konv_aux TYPE tyt_konv.

  SELECT ebeln bukrs bsart ernam lifnr knumv bedat
    FROM ekko
    INTO TABLE ch_t_ekko
   WHERE ebeln IN s_ebeln
     AND bukrs IN s_bukrs
     AND bsart IN s_bsart
     AND bedat IN s_bedat.

  IF ch_t_ekko[] IS NOT INITIAL.

    SELECT bsart batxt
      FROM t161t
      INTO TABLE ch_t_t161t
       FOR ALL ENTRIES IN ch_t_ekko
     WHERE spras EQ sy-langu
       AND bsart EQ ch_t_ekko-bsart.

    SORT ch_t_t161t BY bsart.

    SELECT lifnr name1
      FROM lfa1
      INTO TABLE ch_t_lfa1
       FOR ALL ENTRIES IN ch_t_ekko
     WHERE lifnr EQ ch_t_ekko-lifnr.

    SORT ch_t_lfa1 BY lifnr.

    SELECT ebeln ebelp werks lgort matnr txz01 meins
      FROM ekpo
      INTO TABLE ch_t_ekpo
       FOR ALL ENTRIES IN ch_t_ekko
     WHERE ebeln EQ ch_t_ekko-ebeln.

    IF ch_t_ekpo[] IS NOT INITIAL.

      SELECT ebeln ebelp etenr eindt menge
        FROM eket
        INTO TABLE ch_t_eket
         FOR ALL ENTRIES IN ch_t_ekpo
       WHERE ebeln EQ ch_t_ekpo-ebeln
         AND ebelp EQ ch_t_ekpo-ebelp.

      SELECT knumv kposn kbetr kschl
        FROM konv
        INTO TABLE lt_konv_aux
         FOR ALL ENTRIES IN ch_t_ekko
       WHERE knumv EQ ch_t_ekko-knumv.

      LOOP AT ch_t_ekpo INTO le_ekpo.

        IF le_ekpo-matnr IS NOT INITIAL.

          CLEAR le_makt_aux.

          SELECT SINGLE matnr maktx
            FROM makt
            INTO le_makt_aux
           WHERE spras EQ sy-langu
             AND matnr EQ le_ekpo-matnr.

          APPEND le_makt_aux TO ch_t_makt.

        ENDIF.

        LOOP AT lt_konv_aux INTO le_konv_aux.

          SELECT SINGLE knumv kposn kbetr kschl
          FROM konv
          INTO le_aux
         WHERE knumv EQ le_konv_aux-knumv
           AND kposn EQ le_ekpo-ebelp
           AND kschl EQ 'PBXX'.

          APPEND le_aux TO ch_t_konv.

        ENDLOOP.

      ENDLOOP.

    ENDIF.


  ENDIF.

ENDFORM.                    " GET_DATA_DB


*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_EKKO[]  text
*      <--P_CH_T_ALV[]  text
*----------------------------------------------------------------------*
FORM process_data
  USING
    us_t_ekko  TYPE tyt_ekko
    us_t_t161t TYPE tyt_t161t
    us_t_lfa1  TYPE tyt_lfa1
    us_t_ekpo  TYPE tyt_ekpo
    us_t_makt  TYPE tyt_makt
    us_t_eket  TYPE tyt_eket
    us_t_konv  TYPE tyt_konv
  CHANGING
    ch_t_alv  TYPE tyt_alv.

  DATA:
    le_ekko  TYPE LINE OF tyt_ekko,
    le_t161t TYPE LINE OF tyt_t161t,
    le_lfa1  TYPE LINE OF tyt_lfa1,
    le_ekpo  TYPE LINE OF tyt_ekpo,
    le_makt  TYPE LINE OF tyt_makt,
    le_eket  TYPE LINE OF tyt_eket,
    le_konv  TYPE LINE OF tyt_konv,
    le_alv   TYPE LINE OF tyt_alv.

  IF us_t_ekko[] IS NOT INITIAL.

    LOOP AT us_t_ekko INTO le_ekko.

      CLEAR le_t161t.

      READ TABLE us_t_t161t INTO le_t161t BINARY SEARCH
        WITH KEY bsart = le_ekko-bsart.

      CLEAR le_lfa1.

      READ TABLE us_t_lfa1 INTO le_lfa1 BINARY SEARCH
        WITH KEY lifnr = le_ekko-lifnr.

      LOOP AT us_t_ekpo INTO le_ekpo
        WHERE ebeln = le_ekko-ebeln.

        CLEAR le_konv.

        READ TABLE us_t_konv INTO le_konv
          WITH KEY knumv = le_ekko-knumv
                   kposn = le_ekpo-ebelp
                   kschl = 'PBXX'.

        LOOP AT us_t_eket INTO le_eket
          WHERE ebeln = le_ekpo-ebeln
            AND ebelp = le_ekpo-ebelp.

          CLEAR le_makt.
          CLEAR le_alv.

          IF le_ekpo-matnr IS NOT INITIAL.

            READ TABLE us_t_makt INTO le_makt
              WITH KEY matnr = le_ekpo-matnr.

            le_alv-matnr_desc = le_makt-maktx.

          ELSE.

            le_alv-matnr_desc = le_ekpo-txz01.

          ENDIF.

          le_alv-ebeln       = le_ekko-ebeln.
          le_alv-ebelp       = le_ekpo-ebelp.
          le_alv-etenr       = le_eket-etenr.
          le_alv-eindt       = le_eket-eindt.
          le_alv-bukrs       = le_ekko-bukrs.
          le_alv-bsart       = le_ekko-bsart.
          le_alv-bsart_desc  = le_t161t-batxt.
          le_alv-ernam       = le_ekko-ernam.
          le_alv-lifnr       = le_ekko-lifnr.
          le_alv-lifnr_name  = le_lfa1-name1.
          le_alv-werks       = le_ekpo-werks.
          le_alv-lgort       = le_ekpo-lgort.
          le_alv-matnr       = le_ekpo-matnr.
          le_alv-menge       = le_eket-menge.
          le_alv-meins       = le_ekpo-meins.
          le_alv-kbetr       = le_konv-kbetr.
          le_alv-kbetr_total = le_eket-menge * le_konv-kbetr.

          APPEND le_alv TO ch_t_alv.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ELSE.

    MESSAGE 'No hay datos para los parámetros de selección' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.


ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ALV[]  text
*----------------------------------------------------------------------*
FORM show_report
  USING
    us_t_alv TYPE tyt_alv.

  DATA:
    lv_callback_program TYPE sy-repid,
    le_layout           TYPE slis_layout_alv,
    lt_fieldcat         TYPE slis_t_fieldcat_alv,
    lt_sort             TYPE slis_t_sortinfo_alv,
    le_sort             TYPE LINE OF slis_t_sortinfo_alv.

  lv_callback_program = sy-repid. " titulo del programa

  PERFORM set_layout
    CHANGING
      le_layout.

  PERFORM set_fieldcat
    CHANGING
      lt_fieldcat[].

  CLEAR le_sort.

  le_sort-tabname   = 'T_ALV'. " SUBTOTALIZAR Y ORDENAR
  le_sort-fieldname = 'EBELN'.
  le_sort-up        = 'X'.
  le_sort-subtot    = 'X'.

  APPEND le_sort TO lt_sort.

  SORT us_t_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = lv_callback_program
      is_layout                = le_layout
      it_fieldcat              = lt_fieldcat
      i_callback_pf_status_set = 'SET_STATUS_ALV'
      i_callback_user_command  = 'ALV_USER_COMMAND'
      it_sort                  = lt_sort
    TABLES
      t_outtab                 = us_t_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " SHOW_REPORT

*&---------------------------------------------------------------------*
*& Form SET_STATUS_ALV
*&---------------------------------------------------------------------*
FORM set_status_alv
  USING
     us_t_extab TYPE slis_t_extab.

  SET PF-STATUS 'STATUS' EXCLUDING us_t_extab. " Nombre del status creado

ENDFORM. "set_status_alv

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       Configura la vista del alv
*----------------------------------------------------------------------*
*      <--P_LE_LAYOUT  text
*----------------------------------------------------------------------*
FORM set_layout
  CHANGING
    ch_e_layout TYPE slis_layout_alv.

  ch_e_layout-colwidth_optimize = abap_true. " Optimiza el tamaño de la celda
  ch_e_layout-zebra = abap_true. " Colorea las filas
  ch_e_layout-box_fieldname = 'SEL'. " Seleccionar linea del alv

ENDFORM.                    " SET_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
*       Devuelve el fieldcat
*----------------------------------------------------------------------*
*      <--CH_T_FIELDCAT[]  Tabla de Fieldcat
*----------------------------------------------------------------------*
FORM set_fieldcat
  CHANGING
    ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'EBELN'
       'Orden de Compra'
       abap_false
       abap_true
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'EBELP'
       'Posición'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'ETENR'
       'Reparto'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'EINDT'
       'Fecha del Reparto'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'BUKRS'
       'Sociedad'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'BSART'
       'Clase de Doc.'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'BSART_DESC'
       'Descripción'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'ERNAM'
       'Usuario'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'LIFNR'
       'Proveedor'
       abap_false
       abap_true
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'LIFNR_NAME'
       'Nombre'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'WERKS'
       'Centro'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'LGORT'
       'Almacén'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MATNR'
       'Material'
       abap_false
       abap_true
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MATNR_DESC'
       'Descripcion'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MENGE'
       'Cantidad'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MEINS'
       'Unidad'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'KBETR'
       'Precio Unitario'
       abap_false
       abap_false
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'KBETR_TOTAL'
       'Importe Total'
       abap_true
       abap_false
    CHANGING
      ch_t_fieldcat[].


ENDFORM.                    " SET_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_FIELD
*&---------------------------------------------------------------------*
*       Cargar un campo al fieldcat
*----------------------------------------------------------------------*
*      -->us_v_tabname      TABLA
*      -->us_v_field_name   CAMPO DE TABLA
*      -->us_v_text         NOMBRE
*      <--ch_t_fieldcat     FIELDCAT
*----------------------------------------------------------------------*
FORM set_fieldcat_field
  USING
     us_v_tabname
     us_v_field_name
     us_v_text
     us_v_do_sum
     us_v_hotspot
  CHANGING
     ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:
    le_fieldcat TYPE LINE OF slis_t_fieldcat_alv.

  CLEAR le_fieldcat.

  le_fieldcat-tabname         =  us_v_tabname.
  le_fieldcat-fieldname       =  us_v_field_name.
  le_fieldcat-seltext_l       =  us_v_text.
  le_fieldcat-do_sum          =  us_v_do_sum. " TOTALIZAR
  le_fieldcat-hotspot         =  us_v_hotspot. " HOTSPOT

  APPEND le_fieldcat TO ch_t_fieldcat.


ENDFORM.                    " SET_FIELDCAT_FIELD

*&---------------------------------------------------------------------*
*& Form ALV_USER_COMMAND
*&---------------------------------------------------------------------*
FORM alv_user_command
  USING
     us_v_ucomm    TYPE sy-ucomm
     us_e_selfield TYPE slis_selfield.

  FIELD-SYMBOLS:
     <lfs_alv> TYPE ty_alv.

  DATA:
     le_org_aux TYPE LINE OF tyt_ekko_org,
     le_alv TYPE ty_alv.

  IF us_v_ucomm EQ '&IC1'. " Evento click

    CASE us_e_selfield-fieldname. " Guarda el campo que se presiono.
      WHEN 'EBELN'.

        SET PARAMETER ID 'BES' FIELD us_e_selfield-value. " Paso parametro a la transaccion
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

      WHEN 'LIFNR'.

        UNASSIGN <lfs_alv>.

        READ TABLE t_alv ASSIGNING <lfs_alv> INDEX us_e_selfield-tabindex.

        IF <lfs_alv> IS ASSIGNED.

          SET PARAMETER ID 'LIF' FIELD <lfs_alv>-lifnr. " Paso parametro a la transaccion
          SET PARAMETER ID 'BUK' FIELD <lfs_alv>-bukrs.

          CLEAR le_org_aux.

          SELECT SINGLE ebeln ekorg
            FROM ekko INTO le_org_aux
           WHERE ebeln EQ <lfs_alv>-ebeln.

          SET PARAMETER ID 'EKO' FIELD le_org_aux-ekorg.

          CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN.

        ENDIF.

      WHEN 'MATNR'.

        UNASSIGN <lfs_alv>.

        READ TABLE t_alv ASSIGNING <lfs_alv> INDEX us_e_selfield-tabindex.

        IF <lfs_alv> IS ASSIGNED.

          IF <lfs_alv>-matnr IS NOT INITIAL.

            SET PARAMETER ID 'MAT' FIELD <lfs_alv>-matnr.
            CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

          ENDIF.

        ENDIF.

    ENDCASE.

  ELSEIF us_v_ucomm EQ 'MOVE'.

    PERFORM show_mov.

  ENDIF.

ENDFORM. "alv_user_command
*&---------------------------------------------------------------------*
*&      Form  SHOW_MOV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_mov.

  FIELD-SYMBOLS:
    <lfs_alv> TYPE ty_alv.

  DATA:
    lv_ebeln     TYPE ekko-ebeln,
    lv_msg       TYPE string,
    lt_mseg      TYPE tyt_mseg,
    le_mseg      TYPE LINE OF tyt_mseg,
    lt_mkpf      TYPE tyt_mkpf,
    le_mkpf      TYPE LINE OF tyt_mkpf,
    le_alv_trans TYPE LINE OF tyt_alv_trans,
    lt_alv_trans TYPE tyt_alv_trans.

  READ TABLE t_alv ASSIGNING <lfs_alv>
    WITH KEY sel = 'X'.

  IF <lfs_alv> IS ASSIGNED.

    SELECT ebeln ebelp mblnr mjahr zeile bwart menge meins dmbtr waers
      FROM mseg
      INTO TABLE lt_mseg
     WHERE ebeln EQ <lfs_alv>-ebeln
       AND ebelp EQ <lfs_alv>-ebelp.

    IF lt_mseg[] IS NOT INITIAL.

      SELECT mblnr mjahr blart bldat budat
        FROM mkpf
        INTO TABLE lt_mkpf
         FOR ALL ENTRIES IN lt_mseg
       WHERE mblnr EQ lt_mseg-mblnr
         AND mjahr EQ lt_mseg-mjahr.

      LOOP AT lt_mseg INTO le_mseg.

        LOOP AT lt_mkpf INTO le_mkpf
          WHERE mblnr EQ le_mseg-mblnr
            AND mjahr EQ le_mseg-mjahr.

          CLEAR le_alv_trans.

          le_alv_trans-ebeln  = le_mseg-ebeln.
          le_alv_trans-ebelp  = le_mseg-ebelp.
          le_alv_trans-mblnr  = le_mseg-mblnr.
          le_alv_trans-mjahr  = le_mseg-mjahr.
          le_alv_trans-zeile  = le_mseg-zeile.
          le_alv_trans-blart  = le_mkpf-blart.
          le_alv_trans-bldat  = le_mkpf-bldat.
          le_alv_trans-budat  = le_mkpf-budat.
          le_alv_trans-bwart  = le_mseg-bwart.
          le_alv_trans-menge  = le_mseg-menge.
          le_alv_trans-meins  = le_mseg-meins.
          le_alv_trans-dmbtr  = le_mseg-dmbtr.
          le_alv_trans-waers  = le_mseg-waers.

          APPEND le_alv_trans TO lt_alv_trans.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    IF lt_alv_trans[] IS NOT INITIAL.

      PERFORM alv_trans_report
         USING
           lt_alv_trans.

    ENDIF.

*    IF sy-subrc EQ 0.
*
*      CONCATENATE 'Entrega:' lv_ebeln INTO lv_msg SEPARATED BY space.
*
*      MESSAGE lv_msg TYPE 'I'.
*
*    ELSE.
*
*      MESSAGE 'La Factura no tiene Entrega' TYPE 'I'.
*
*    ENDIF.
*
  ENDIF.


  ENDFORM.                    " SHOW_MOV
*&---------------------------------------------------------------------*
*&      Form  ALV_TRANS_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ALV_TRANS  text
*----------------------------------------------------------------------*
FORM alv_trans_report
  USING
    us_t_alv_trans TYPE tyt_alv_trans.

  DATA:
    lv_callback_program TYPE sy-repid,
    le_layout           TYPE slis_layout_alv,
    lt_fieldcat         TYPE slis_t_fieldcat_alv.

  lv_callback_program = sy-repid. " titulo del programa

  PERFORM set_trans_layout
    CHANGING
      le_layout.

  PERFORM set_trans_fieldcat
    CHANGING
      lt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = lv_callback_program
      is_layout          = le_layout
      it_fieldcat        = lt_fieldcat
    TABLES
      t_outtab           = us_t_alv_trans
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_TRANS_REPORT
*&---------------------------------------------------------------------*
*&      Form  SET_TRANS_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LE_LAYOUT  text
*----------------------------------------------------------------------*
FORM set_trans_layout
  CHANGING
    ch_e_layout TYPE slis_layout_alv.

  ch_e_layout-colwidth_optimize = abap_true. " Optimiza el tamaño de la celda
  ch_e_layout-zebra = abap_true. " Colorea las filas

ENDFORM.                    " SET_TRANS_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
*       Devuelve el fieldcat
*----------------------------------------------------------------------*
*      <--CH_T_FIELDCAT[]  Tabla de Fieldcat
*----------------------------------------------------------------------*
FORM SET_TRANS_FIELDCAT
  CHANGING
    ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'EBELN'
       'Orden de Compra'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'EBELP'
       'Posicion de Compra'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'MBLNR'
       'Movimiento'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'MJAHR'
       'Año'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'ZEILE'
       'Posición'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'BLART'
       'Clase'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'BLDAT'
       'Fecha de Documento'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'BUDAT'
       'Fecha de Contabilización'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'BWART'
       'Clase de Movimiento'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'MENGE'
       'Cantidad'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'MEINS'
       'Unidad'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'DMBTR'
       'Importe'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_trans_fieldcat_field
    USING
       'US_T_ALV_TRANS'
       'WAERS'
       'Moneda'
    CHANGING
      ch_t_fieldcat[].

ENDFORM.                    " SET_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_FIELD
*&---------------------------------------------------------------------*
*       Cargar un campo al fieldcat
*----------------------------------------------------------------------*
*      -->us_v_tabname      TABLA
*      -->us_v_field_name   CAMPO DE TABLA
*      -->us_v_text         NOMBRE
*      <--ch_t_fieldcat     FIELDCAT
*----------------------------------------------------------------------*
FORM SET_TRANS_FIELDCAT_FIELD
  USING
     us_v_tabname
     us_v_field_name
     us_v_text
  CHANGING
     ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:
    le_fieldcat TYPE LINE OF slis_t_fieldcat_alv.

  CLEAR le_fieldcat.

  le_fieldcat-tabname         =  us_v_tabname.
  le_fieldcat-fieldname       =  us_v_field_name.
  le_fieldcat-seltext_l       =  us_v_text.

  APPEND le_fieldcat TO ch_t_fieldcat.

ENDFORM.                    " SET_FIELDCAT_FIELD