*&---------------------------------------------------------------------*
*&  Include           ZAA23DT_DYNPROS_COT_TOP
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS:
  abap,
  vrm.

*&---------------------------------------------------------------------*
*&  TYPES
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_9000,
    entrega TYPE zaaps_cot-vbeln,
    remito  TYPE zaaps_cot-xblnr,
    checkbox TYPE char1, " Si activo 'X'
  END OF ty_9000.

TYPES:
  BEGIN OF ty_9001,
    entrega    TYPE zaaps_cot-vbeln,
    xblnr      TYPE zaaps_cot-xblnr,
    werks      TYPE zaaps_cot-werks,
    werks_desc TYPE t001w-name1,
    nro_cot    TYPE zaaps_cot-nro_cot,
*    estado     TYPE zaaps_cot-estado,
    estado     TYPE char1,
    usuario    TYPE zaaps_cot-usuario,
    fecha      TYPE zaaps_cot-fecha,
    hora       TYPE zaaps_cot-hora,
  END OF ty_9001.

TYPES:
  BEGIN OF ty_9002,
    entrega TYPE zaaps_cot-vbeln,
  END OF ty_9002.

TYPES:
  BEGIN OF ty_9002_table,
    posnr        TYPE zaaps_cot_pos-posnr,
    nro_cot      TYPE zaaps_cot_pos-nro_cot,
    material_cot TYPE zaaps_cot_pos-material_cot,
    unidad_cot   TYPE zaaps_cot_pos-unidad_cot,
  END OF ty_9002_table,

  tyt_9002_table TYPE STANDARD TABLE OF ty_9002_table.


*&---------------------------------------------------------------------*
*&  DATA
*&---------------------------------------------------------------------*
DATA:
  s_9000       TYPE ty_9000,
  s_9001       TYPE ty_9001,
  s_9002       TYPE ty_9002,
  t_9002       TYPE tyt_9002_table,
  s_9002_table TYPE ty_9002_table.

DATA:
  v_init_9001 TYPE abap_bool,
  v_init_9002 TYPE abap_bool.