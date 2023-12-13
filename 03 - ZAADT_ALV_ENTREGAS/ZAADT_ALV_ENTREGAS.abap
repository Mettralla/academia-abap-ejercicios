*&---------------------------------------------------------------------*
*& Report  ZAA23DT_REPORTE_ALV_ENTREGAS
*-
*- Posición (LIPS-POSNR)
*- Descripción (de la Clase de Entrega)
*- Nombre (del Destinatario, KNA1-NAME1)
*- Descripción (del Puesto de Exp.)
*- Material (LIPS-MATNR)
*- Descripción (MAKT-MAKTX)
*- Cantidad (LIPS-LFIMG)
*- Unidad (LIPS-VRKME)
*- Descripción (de la Unidad)
*&---------------------------------------------------------------------*

REPORT  zaadt_alv_entregas.

*&---------------------------------------------------------------------*
*& INCLUDE
*&---------------------------------------------------------------------*
INCLUDE:
  zaadt_alv_entregas_top.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

SELECT-OPTIONS:
   s_vbeln FOR likp-vbeln,
   s_erdat FOR likp-erdat,
   s_lfart FOR likp-lfart,
   s_kunnr FOR likp-kunnr.

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

    MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.

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
    lt_likp  TYPE tyt_likp,
    lt_kna1  TYPE tyt_kna1,
    lt_tvlkt TYPE tyt_tvlkt,
    lt_tvstt TYPE tyt_tvstt,
    lt_lips  TYPE tyt_lips,
    lt_makt  TYPE tyt_makt,
    lt_t006a TYPE tyt_t006a.

  PERFORM get_data_db
    CHANGING
      lt_likp[]
      lt_kna1[]
      lt_tvlkt[]
      lt_tvstt[]
      lt_lips[]
      lt_makt[]
      lt_t006a[].

  PERFORM process_data
    USING
      lt_likp[]
      lt_kna1[]
      lt_tvlkt[]
      lt_tvstt[]
      lt_lips[]
      lt_makt[]
      lt_t006a[]
    CHANGING
      ch_t_alv[].


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DB
*&---------------------------------------------------------------------*
*       Carga las tablas que se usaran para rellenar la tabla ALV
*----------------------------------------------------------------------*
*      <--CH_T_LIKP  ENTREGAS
*      <--CH_T_KNA1  DESTINATARIOS
*      <--CH_T_TVLKT CLASE ENTREGA DESC
*      <--CH_T_TVSTT PUESTO EXP DESC
*----------------------------------------------------------------------*
FORM get_data_db
  CHANGING
    ch_t_likp  TYPE tyt_likp
    ch_t_kna1  TYPE tyt_kna1
    ch_t_tvlkt TYPE tyt_tvlkt
    ch_t_tvstt TYPE tyt_tvstt
    ch_t_lips  TYPE tyt_lips
    ch_t_makt  TYPE tyt_makt
    ch_t_t006a TYPE tyt_t006a.

  SELECT vbeln erdat lfart kunnr xblnr vstel
    FROM likp
    INTO TABLE ch_t_likp
   WHERE vbeln IN s_vbeln
     AND erdat IN s_erdat
     AND lfart IN s_lfart
     AND kunnr IN s_kunnr.

  IF ch_t_likp[] IS NOT INITIAL.

    SELECT kunnr name1
      FROM kna1
      INTO TABLE ch_t_kna1
       FOR ALL ENTRIES IN ch_t_likp
     WHERE kunnr EQ ch_t_likp-kunnr.

    SORT ch_t_kna1 BY kunnr. " Ordena la tabla para binary search

    SELECT lfart vtext
      FROM tvlkt
      INTO TABLE ch_t_tvlkt
       FOR ALL ENTRIES IN ch_t_likp
     WHERE spras EQ sy-langu
       AND lfart EQ ch_t_likp-lfart.

    SORT ch_t_tvlkt BY lfart. " Ordena la tabla para binary search

    SELECT vstel vtext
      FROM tvstt
      INTO TABLE ch_t_tvstt
       FOR ALL ENTRIES IN ch_t_likp
     WHERE spras EQ sy-langu
       AND vstel EQ ch_t_likp-vstel.

    SORT ch_t_tvstt BY vstel. " Ordena la tabla para binary search

    SELECT vbeln posnr matnr lfimg vrkme
      FROM lips
      INTO TABLE ch_t_lips
       FOR ALL ENTRIES IN ch_t_likp
     WHERE vbeln EQ ch_t_likp-vbeln.

    IF ch_t_lips[] IS NOT INITIAL.

      SELECT matnr maktx
        FROM makt
        INTO TABLE ch_t_makt
         FOR ALL ENTRIES IN ch_t_lips
       WHERE matnr EQ ch_t_lips-matnr
         AND spras EQ sy-langu.

      SORT ch_t_makt BY matnr.

      SELECT msehi mseht
        FROM t006a
        INTO TABLE ch_t_t006a
         FOR ALL ENTRIES IN ch_t_lips
       WHERE msehi EQ ch_t_lips-vrkme
         AND spras EQ sy-langu.

      SORT ch_t_t006a BY msehi.

    ENDIF.



  ENDIF.

ENDFORM.                    " GET_DATA_DB
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*   Recorre las tablas aux y rellena la tabla ALV
*----------------------------------------------------------------------*
*      -->US_T_LIKP  ENTREGAS
*      -->US_T_KNA1  DESTINATARIOS
*      -->US_T_TVLKT CLASE ENTREGA DESC
*      -->US_T_TVSTT PUESTO EXP DESC
*      <--CH_T_ALV   REPORTE ALV
*----------------------------------------------------------------------*
FORM process_data
  USING
    us_t_likp  TYPE tyt_likp
    us_t_kna1  TYPE tyt_kna1
    us_t_tvlkt TYPE tyt_tvlkt
    us_t_tvstt TYPE tyt_tvstt
    us_t_lips  TYPE tyt_lips
    us_t_makt  TYPE tyt_makt
    us_t_t006a TYPE tyt_t006a
  CHANGING
    ch_t_alv    TYPE tyt_alv.

  DATA:
    le_likp     TYPE LINE OF tyt_likp,
    le_kna1     TYPE LINE OF tyt_kna1,
    le_tvlkt    TYPE LINE OF tyt_tvlkt,
    le_tvstt    TYPE LINE OF tyt_tvstt,
    le_lips     TYPE LINE OF tyt_lips,
    le_makt     TYPE LINE OF tyt_makt,
    le_t006a    TYPE LINE OF tyt_t006a,
    le_alv      TYPE LINE OF tyt_alv.

  IF us_t_likp[] IS NOT INITIAL.

    LOOP AT us_t_likp INTO le_likp.

      CLEAR le_kna1.

      READ TABLE us_t_kna1 INTO le_kna1 BINARY SEARCH
        WITH KEY kunnr = le_likp-kunnr.

      CLEAR le_tvlkt.

      READ TABLE us_t_tvlkt INTO le_tvlkt BINARY SEARCH
        WITH KEY lfart = le_likp-lfart.

      CLEAR le_tvstt.

      READ TABLE us_t_tvstt INTO le_tvstt BINARY SEARCH
        WITH KEY vstel = le_likp-vstel.

      LOOP AT us_t_lips INTO le_lips
        WHERE vbeln EQ le_likp-vbeln.

        CLEAR le_makt.

        READ TABLE us_t_makt INTO le_makt BINARY SEARCH
          WITH KEY matnr = le_lips-matnr.

        CLEAR le_t006a.

        READ TABLE us_t_t006a INTO le_t006a BINARY SEARCH
          WITH KEY msehi = le_lips-vrkme.

        CLEAR le_alv.

        le_alv-vbeln        = le_likp-vbeln.
        le_alv-posnr        = le_lips-posnr.
        le_alv-erdat        = le_likp-erdat.
        le_alv-lfart        = le_likp-lfart.
        le_alv-lfart_desc   = le_tvlkt-vtext.
        le_alv-kunnr        = le_likp-kunnr.
        le_alv-kunnr_name1  = le_kna1-name1.
        le_alv-xblnr        = le_likp-xblnr.
        le_alv-vstel        = le_likp-vstel.
        le_alv-vstel_desc   = le_tvstt-vtext.
        le_alv-matnr        = le_lips-matnr.
        le_alv-maktx        = le_makt-maktx.
        le_alv-lfimg        = le_lips-lfimg.
        le_alv-vrkme        = le_lips-vrkme.
        le_alv-vrkme_desc   = le_t006a-mseht.

        APPEND le_alv TO ch_t_alv.

      ENDLOOP.

    ENDLOOP.

  ELSE.

    MESSAGE 'No hay datos para los parámetros de selección' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.


ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_REPORT
*&---------------------------------------------------------------------*
*       Toma la tabla y muestra el reporte
*----------------------------------------------------------------------*
*      -->US_T_ALV  TABLA ALV
*----------------------------------------------------------------------*
FORM SHOW_REPORT
  USING
    us_t_alv TYPE tyt_alv.

  DATA:
    lv_callback_program TYPE sy-repid,
    le_layout           TYPE slis_layout_alv,
    lt_fieldcat         TYPE slis_t_fieldcat_alv.

  lv_callback_program = sy-repid. " titulo del programa

  PERFORM set_layout
    CHANGING
      le_layout.

  PERFORM set_fieldcat
    CHANGING
      lt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM                = lv_callback_program
     IS_LAYOUT                         = le_layout
     IT_FIELDCAT                       = lt_fieldcat
    TABLES
      t_outtab                          = us_t_alv
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " SHOW_REPORT
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       Configura la vista del alv
*----------------------------------------------------------------------*
*      <--P_LE_LAYOUT  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT
  CHANGING
    ch_e_layout TYPE slis_layout_alv.

  ch_e_layout-colwidth_optimize = abap_true. " Optimiza el tamaño de la celda
  ch_e_layout-zebra = abap_true. " Colorea las filas

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
*       Devuelve el fieldcat
*----------------------------------------------------------------------*
*      <--CH_T_FIELDCAT[]  Tabla de Fieldcat
*----------------------------------------------------------------------*
FORM SET_FIELDCAT
  CHANGING
    ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'VBELN'
       'Entrega'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'POSNR'
       'Posicion'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'ERDAT'
       'Fecha'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'LFART'
       'Clase Entrega'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'LFART_DESC'
       'Descripcion'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'KUNNR'
       'Destinatario'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'KUNNR_NAME1'
       'Nombre'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'XBLNR'
       'Remito'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'VSTEL'
       'Puesto de Exp.'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'VSTEL_DESC'
       'Descripcion'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MATNR'
       'Material'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MAKTX'
       'Descripcion'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'LFIMG'
       'Cantidad'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'VRKME'
       'Unidad'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'VRKME_DESC'
       'Descripcion'
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
FORM SET_FIELDCAT_FIELD
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