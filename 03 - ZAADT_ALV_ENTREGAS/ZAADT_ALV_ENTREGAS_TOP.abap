*&---------------------------------------------------------------------*
*&  Include           ZAA23DT_ALV_ENTREGAS_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& TYPE POOLS
*&---------------------------------------------------------------------*
*& Simple List Integration Services (slis): proporciona un conjunto de
*& funciones y estructuras de datos que facilitan la creación y
*& presentación de listas de datos en formatos tabulares.
*&---------------------------------------------------------------------*

TYPE-POOLS:
  slis,
  abap.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*
*& LIKP (Sales Doc: Delivery Header)
*& LIPS (Sales Doc: Delivery Items)
*& MAKT (Material Description)
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

TYPES:
  BEGIN OF ty_lips,
    vbeln TYPE lips-vbeln,
    posnr TYPE lips-posnr,
    matnr TYPE lips-matnr,
    lfimg TYPE lips-lfimg,
    vrkme TYPE lips-vrkme,
  END OF ty_lips,

  tyt_lips TYPE STANDARD TABLE OF ty_lips.

TYPES:
  BEGIN OF ty_t006a,
    msehi TYPE t006a-msehi, "comparacion
    mseht TYPE t006a-mseht, "DESC
  END OF ty_t006a,

  tyt_t006a TYPE STANDARD TABLE OF ty_t006a.

TYPES:
  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  tyt_makt TYPE STANDARD TABLE OF ty_makt.

TYPES:
  BEGIN OF ty_alv,
    vbeln       TYPE likp-vbeln,
    posnr       TYPE lips-posnr,
    erdat       TYPE likp-erdat,
    lfart       TYPE likp-lfart,
    lfart_desc  TYPE tvlkt-vtext,
    kunnr       TYPE likp-kunnr,
    kunnr_name1 TYPE kna1-name1,
    xblnr       TYPE likp-xblnr,
    vstel       TYPE likp-vstel,
    vstel_desc  TYPE tvstt-vtext,
    matnr       TYPE lips-matnr,
    maktx       TYPE makt-maktx,
    lfimg       TYPE lips-lfimg,
    vrkme       TYPE lips-vrkme,
    vrkme_desc  TYPE t006a-mseht,
  END OF ty_alv,

  tyt_alv TYPE STANDARD TABLE OF ty_alv.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*

DATA:
   t_alv   TYPE tyt_alv.