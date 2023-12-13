*&---------------------------------------------------------------------*
*& Report  ZAADT_REPORTE_WRITE_FACTURAS_V
*&
*& Seleccionar Facturas de Venta y mostrar en un reporte WRITE, los siguientes valores:
*&
*& Factura (VBRK-VBELN)
*& Fecha (VBRK-FKDAT)
*& Clase Factura (VBRK-FKART)
*& Descripción (de la Clase de Factura, buscar por dominio) TVFKT-FKART (Factura) - TVFKT-VTEXT (DESC)
*& Solicitante (VBRK-KUNAG)
*& Nombre (del Solicitante, KNA1-NAME1)
*& Pagador (VBRK-KUNRG)
*& Nombre (del Pagador, KNA1-NAME1)
*& Num. Legal (VBRK-XBLNR)
*&
*& Filtros
*& Factura
*& Fecha (por defecto Fecha del día)
*& Clase de Factura
*& Num. Legal
*& Solicitante
*&
*&---------------------------------------------------------------------*

REPORT  zaadt_reporte_write_facturas_v.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*

TABLES:
  vbrk.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_vbrk,
    vbeln TYPE vbrk-vbeln,
    fkdat TYPE vbrk-fkdat,
    fkart TYPE vbrk-fkart,
    kunag TYPE vbrk-kunag,
    kunrg TYPE vbrk-kunrg,
    xblnr TYPE vbrk-xblnr,
  END OF ty_vbrk,

  tyt_vbrk TYPE STANDARD TABLE OF ty_vbrk.

TYPES:
  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,

  tyt_kna1 TYPE STANDARD TABLE OF ty_kna1.

TYPES:
  BEGIN OF ty_tvfkt,
    fkart TYPE tvfkt-fkart,
    vtext TYPE tvfkt-vtext,
  END OF ty_tvfkt,

  tyt_tvfkt TYPE STANDARD TABLE OF ty_tvfkt.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*

DATA:
  t_vbrk    TYPE tyt_vbrk,
  e_vbrk    TYPE LINE OF tyt_vbrk,
  t_sold_to TYPE tyt_kna1,
  e_sold_to TYPE LINE OF tyt_kna1,
  t_payer   TYPE tyt_kna1,
  e_payer   TYPE LINE OF tyt_kna1,
  t_tvfkt   TYPE tyt_tvfkt,
  e_tvfkt   TYPE LINE OF tyt_tvfkt.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

SELECT-OPTIONS:
   s_vbeln FOR vbrk-vbeln,
   s_fkdat FOR vbrk-fkdat,
   s_fkart FOR vbrk-fkart,
   s_xblnr FOR vbrk-xblnr,
   s_kunag FOR vbrk-kunag.

SELECTION-SCREEN END OF BLOCK b01.

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*

INITIALIZATION.
  s_fkdat-low = sy-datum - 1.
  s_fkdat-high = sy-datum.
  APPEND s_fkdat.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*

END-OF-SELECTION.

  SELECT vbeln fkdat fkart kunag kunrg xblnr
    FROM vbrk
    INTO TABLE t_vbrk
   WHERE vbeln IN s_vbeln
     AND fkdat IN s_fkdat
     AND fkart IN s_fkart
     AND kunag IN s_kunag
     AND xblnr IN s_xblnr.

  IF t_vbrk[] IS NOT INITIAL.

    " SOLICITANTE
    SELECT kunnr name1
      FROM kna1
      INTO TABLE t_sold_to
       FOR ALL ENTRIES IN t_vbrk
     WHERE kunnr = t_vbrk-kunag.

    " PAGADOR
    SELECT kunnr name1
      FROM kna1
      INTO TABLE t_payer
       FOR ALL ENTRIES IN t_vbrk
     WHERE kunnr = t_vbrk-kunrg.

    SELECT fkart vtext
      FROM tvfkt
      INTO TABLE t_tvfkt
       FOR ALL ENTRIES IN t_vbrk
     WHERE fkart = t_vbrk-fkart.

  ENDIF.

  IF t_vbrk[] IS NOT INITIAL.

    LOOP AT t_vbrk INTO e_vbrk.
*
      CLEAR e_sold_to.

      READ TABLE t_sold_to INTO e_sold_to
        WITH KEY kunnr = e_vbrk-kunag.

      CLEAR e_payer.

      READ TABLE t_payer INTO e_payer
        WITH KEY kunnr = e_vbrk-kunrg.

      CLEAR e_tvfkt.

      READ TABLE t_tvfkt INTO e_tvfkt
        WITH KEY fkart = e_vbrk-fkart.

      WRITE: / e_vbrk-vbeln, e_vbrk-fkdat, e_vbrk-fkart, e_tvfkt-vtext, e_vbrk-kunag, e_sold_to-name1, e_vbrk-kunrg, e_payer-name1, e_vbrk-xblnr.

    ENDLOOP.

  ELSE.

    MESSAGE 'No hay datos para los parámetros de selección' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.