*&---------------------------------------------------------------------*
*&  Include           ZAADT_ALV_ORDEN_COMPRA_TOP
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

TABLES:
  ekko.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& PRIMER ALV
*&---------------------------------------------------------------------*

TYPES:
  BEGIN OF ty_ekko,
    ebeln TYPE ekko-ebeln,
    bukrs TYPE ekko-bukrs,
    bsart TYPE ekko-bsart,
    ernam TYPE ekko-ernam,
    lifnr TYPE ekko-lifnr,
    knumv TYPE ekko-knumv,
    bedat TYPE ekko-bedat,
  END OF ty_ekko,

  tyt_ekko TYPE STANDARD TABLE OF ty_ekko.

TYPES:
  BEGIN OF ty_ekko_org,
    ebeln TYPE ekko-ebeln,
    ekorg TYPE ekko-ekorg,
  END OF ty_ekko_org,

  tyt_ekko_org TYPE STANDARD TABLE OF ty_ekko_org.

TYPES:
  BEGIN OF ty_t161t,
    bsart TYPE t161t-bsart,
    batxt TYPE t161t-batxt,
  END OF ty_t161t,

  tyt_t161t TYPE STANDARD TABLE OF ty_t161t.

TYPES:
  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
  END OF ty_lfa1,

  tyt_lfa1 TYPE STANDARD TABLE OF ty_lfa1.

TYPES:
  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    werks TYPE ekpo-werks,
    lgort TYPE ekpo-lgort,
    matnr TYPE ekpo-matnr,
    txz01 TYPE ekpo-txz01,
    meins TYPE ekpo-meins,
  END OF ty_ekpo,

  tyt_ekpo TYPE STANDARD TABLE OF ty_ekpo.

TYPES:
  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  tyt_makt TYPE STANDARD TABLE OF ty_makt.

TYPES:
  BEGIN OF ty_eket,
    ebeln TYPE eket-ebeln,
    ebelp TYPE eket-ebelp,
    etenr TYPE eket-etenr,
    eindt TYPE eket-eindt,
    menge TYPE eket-menge,
  END OF ty_eket,

  tyt_eket TYPE STANDARD TABLE OF ty_eket.

TYPES:
  BEGIN OF ty_konv,
    knumv TYPE konv-knumv,
    kposn TYPE konv-kposn,
    kbetr TYPE konv-kbetr,
    kschl TYPE konv-kschl,
  END OF ty_konv,

  tyt_konv TYPE STANDARD TABLE OF ty_konv.

TYPES:
  BEGIN OF ty_alv,
    sel         TYPE char1,
    ebeln       TYPE ekko-ebeln,
    ebelp       TYPE ekpo-ebelp,
    etenr       TYPE eket-etenr,
    eindt       TYPE eket-eindt,
    bukrs       TYPE ekko-bukrs,
    bsart       TYPE ekko-bsart,
    bsart_desc  TYPE t161t-batxt,
    ernam       TYPE ekko-ernam,
    lifnr       TYPE ekko-lifnr,
    lifnr_name  TYPE lfa1-name1,
    werks       TYPE ekpo-werks,
    lgort       TYPE ekpo-lgort,
    matnr       TYPE ekpo-matnr,
    matnr_desc  TYPE c LENGTH 40,
    menge       TYPE eket-menge,
    meins       TYPE ekpo-meins,
    kbetr       TYPE konv-kbetr,
    kbetr_total TYPE konv-kbetr,
  END OF ty_alv,

  tyt_alv TYPE STANDARD TABLE OF ty_alv.

*&---------------------------------------------------------------------*
*& SEGUNDO ALV
*&---------------------------------------------------------------------*

TYPES:
BEGIN OF ty_mseg,
  ebeln TYPE mseg-ebeln,
  ebelp TYPE mseg-ebelp,
  mblnr TYPE mseg-mblnr,
  mjahr TYPE mseg-mjahr,
  zeile TYPE mseg-zeile,
  bwart TYPE mseg-bwart,
  menge TYPE mseg-menge,
  meins TYPE mseg-meins,
  dmbtr TYPE mseg-dmbtr,
  waers TYPE mseg-waers,
END OF ty_mseg,

tyt_mseg TYPE STANDARD TABLE OF ty_mseg.

TYPES:
BEGIN OF ty_mkpf,
  mblnr TYPE mkpf-mblnr,
  mjahr TYPE mkpf-mjahr,
  blart TYPE mkpf-blart,
  bldat TYPE mkpf-bldat,
  budat TYPE mkpf-budat,
END OF ty_mkpf,

tyt_mkpf TYPE STANDARD TABLE OF ty_mkpf.

TYPES:
BEGIN OF ty_alv_trans,
  ebeln TYPE mseg-ebeln,
  ebelp TYPE mseg-ebelp,
  mblnr TYPE mseg-mblnr,
  mjahr TYPE mseg-mjahr,
  zeile TYPE mseg-zeile,
  blart TYPE mkpf-blart,
  bldat TYPE mkpf-bldat,
  budat TYPE mkpf-budat,
  bwart TYPE mseg-bwart,
  menge TYPE mseg-menge,
  meins TYPE mseg-meins,
  dmbtr TYPE mseg-dmbtr,
  waers TYPE mseg-waers,
END OF ty_alv_trans,

tyt_alv_trans TYPE STANDARD TABLE OF ty_alv_trans.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*

DATA:
   t_alv   TYPE tyt_alv.