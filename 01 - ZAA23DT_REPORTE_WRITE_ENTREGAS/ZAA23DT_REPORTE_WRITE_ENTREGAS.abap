*&---------------------------------------------------------------------*
*& Report  ZAA23DT_REPORTE_WRITE_ENTREGAS
*&---------------------------------------------------------------------*

REPORT zaa23dt_reporte_write_entregas.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*

TABLES:
  likp.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_likp,
    vbeln TYPE likp-vbeln,
    erdat TYPE likp-erdat,
    lfart TYPE likp-lfart,
    kunnr TYPE likp-kunnr,
    xblnr TYPE likp-xblnr,
    vstel TYPE likp-vstel,
  END OF ty_likp,

  tyt_likp TYPE STANDARD TABLE OF ty_likp.

TYPES:
  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,

  tyt_kna1 TYPE STANDARD TABLE OF ty_kna1.

TYPES:
  BEGIN OF ty_tvlkt,
    lfart TYPE tvlkt-lfart,
    vtext TYPE tvlkt-vtext,
  END OF ty_tvlkt,

  tyt_tvlkt TYPE STANDARD TABLE OF ty_tvlkt.

TYPES:
  BEGIN OF ty_tvstt,
    vstel TYPE tvstt-vstel,
    vtext TYPE tvstt-vtext,
  END OF ty_tvstt,

  tyt_tvstt TYPE STANDARD TABLE OF ty_tvstt.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*

DATA:
  t_likp     TYPE tyt_likp,
  e_likp     TYPE LINE OF tyt_likp,
  t_kna1     TYPE tyt_kna1,
  e_kna1     TYPE LINE OF tyt_kna1,
  t_tvlkt    TYPE tyt_tvlkt,
  e_tvlkt    TYPE LINE OF tyt_tvlkt,
  t_tvstt    TYPE tyt_tvstt,
  e_tvstt    TYPE LINE OF tyt_tvstt,
  lv_mensaje TYPE string.

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
*& INITIALIZATION
*&---------------------------------------------------------------------*

INITIALIZATION.
  s_erdat-low = sy-datum - 1.
  s_erdat-high = sy-datum.
  APPEND s_erdat.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*

END-OF-SELECTION.

  SELECT vbeln erdat lfart kunnr xblnr vstel
    FROM likp
    INTO TABLE t_likp
   WHERE vbeln IN s_vbeln
     AND erdat IN s_erdat
     AND lfart IN s_lfart
     AND kunnr IN s_kunnr.

  IF t_likp IS NOT INITIAL.

    SELECT kunnr name1
      FROM kna1
      INTO TABLE t_kna1
       FOR ALL ENTRIES IN t_likp
     WHERE kunnr = t_likp-kunnr.

    SELECT lfart vtext
      FROM tvlkt
      INTO TABLE t_tvlkt
       FOR ALL ENTRIES IN t_likp
     WHERE spras = sy-langu
       AND lfart = t_likp-lfart.

    SELECT vstel vtext
      FROM tvstt
      INTO TABLE t_tvstt
       FOR ALL ENTRIES IN t_likp
     WHERE spras = sy-langu
       AND vstel = t_likp-vstel.

  ENDIF.

  IF t_likp IS NOT INITIAL.

    LOOP AT t_likp INTO e_likp.

      CLEAR e_kna1.

      READ TABLE t_kna1 INTO e_kna1
        WITH KEY kunnr = e_likp-kunnr.

      CLEAR e_tvlkt.

      READ TABLE t_tvlkt INTO e_tvlkt
        WITH KEY lfart = e_likp-lfart.

      CLEAR e_tvstt.

      READ TABLE t_tvstt INTO e_tvstt
        WITH KEY vstel = e_likp-vstel.

      WRITE: / e_likp-vbeln, e_likp-erdat, e_likp-lfart, e_tvlkt-vtext, e_likp-kunnr, e_kna1-name1, e_likp-xblnr, e_likp-vstel, e_tvstt-vtext.

    ENDLOOP.

  ELSE.

    MESSAGE 'No hay datos para los parámetros de selección' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.