*&---------------------------------------------------------------------*
*&  Include           ZCDYMOPCBHTOHGD_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& TYPE POOLS
*&---------------------------------------------------------------------*

type-pools:
  slis,
  abap.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*

tables:
  bkpf,
  vbsegk.

*&---------------------------------------------------------------------*
*& TYPES
*&
*& BKPF  - Accounting Document Header
*& T003T - Document Text (BLART)
*& TSTCT - Transaction Text (TCODE)
*& TCURT - Currency Text (WAERS)
*&---------------------------------------------------------------------*

types:
  tyt_string type standard table of string.

types:
  begin of ty_bkpf,
    bukrs      type bkpf-bukrs,
    belnr      type bkpf-belnr,
    gjahr      type bkpf-gjahr,
    blart      type bkpf-blart,
    bldat      type bkpf-bldat,
    budat      type bkpf-budat,
    xblnr      type bkpf-xblnr,
    monat      type bkpf-monat,
    tcode      type bkpf-tcode,
    waers      type bkpf-waers,
  end of ty_bkpf,

  tyt_bkpf type standard table of ty_bkpf.

types:
  begin of ty_t003t,
    blart type t003t-blart,
    ltext type t003t-ltext,
  end of ty_t003t,

  tyt_t003t type standard table of ty_t003t.

types:
  begin of ty_tstct,
    tcode type tstct-tcode,
    ttext type tstct-ttext,
  end of ty_tstct,

  tyt_tstct type standard table of ty_tstct.

types:
  begin of ty_tcurt,
    waers type tcurt-waers,
    ltext type tcurt-ltext,
  end of ty_tcurt,

  tyt_tcurt type standard table of ty_tcurt.

types:
  begin of ty_vbsegk,
    ausbk type vbsegk-ausbk,
    belnr type vbsegk-belnr,
    gjahr type vbsegk-gjahr,
    bzkey type vbsegk-bzkey,
    bschl type vbsegk-bschl,
    lifnr type vbsegk-lifnr,
    zfbdt type vbsegk-zfbdt,
    zbd1t type vbsegk-zbd1t,
    wrbtr type vbsegk-wrbtr,
    shkzg type vbsegk-shkzg,
  end of ty_vbsegk,

  tyt_vbsegk type standard table of ty_vbsegk.

types:
  begin of ty_tbslt,
    bschl type tbslt-bschl,
    ltext type tbslt-ltext,
  end of ty_tbslt,

  tyt_tbslt type standard table of ty_tbslt.

types:
  begin of ty_lfa1,
    lifnr  type lfa1-lifnr,
    name1  type lfa1-name1,
  end of ty_lfa1,

  tyt_lfa1 type standard table of ty_lfa1.

types:
  begin of ty_alv,
    sel         type char1,
    bukrs       type bkpf-bukrs,
    belnr       type bkpf-belnr,
    gjahr       type bkpf-gjahr,
    blart       type bkpf-blart,
    blart_desc  type t003t-ltext,
    bldat       type bkpf-bldat,
    budat       type bkpf-budat,
    xblnr       type bkpf-xblnr,
    monat       type bkpf-monat,
    monat_name  type string,
    tcode       type bkpf-tcode,
    tcode_desc  type tstct-ttext,
    bzkey       type vbsegk-bzkey,
    bschl       type vbsegk-bschl,
    bschl_desc  type tbslt-ltext,
    lifnr       type vbsegk-lifnr,
    lifnr_name1 type lfa1-name1,
    zfbdt       type vbsegk-zfbdt, " Fecha Base
    zfbdt_venc  type vbsegk-zfbdt, " Fecha Vencimiento
    wrbtr       type vbsegk-wrbtr,
    waers       type bkpf-waers,
    waers_desc  type tcurt-ltext,
    observacion type zcdymopcbhtohgdt-observacion,
  end of ty_alv,

  tyt_alv type standard table of ty_alv.

types:
  begin of ty_9000,
    mandt       type zcdymopcbhtohgdt-mandt,
    bukrs       type zcdymopcbhtohgdt-bukrs,
    belnr       type zcdymopcbhtohgdt-belnr,
    gjahr       type zcdymopcbhtohgdt-gjahr,
    bzkey       type zcdymopcbhtohgdt-bzkey,
    observacion type zcdymopcbhtohgdt-observacion,
  end of ty_9000,

  tyt_obs type standard table of ty_9000.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*

data:
   t_alv   type tyt_alv,
   s_9000  type ty_9000.