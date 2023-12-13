*&---------------------------------------------------------------------*
*& Report  ZAA23DT_REPORTE_ALV_INTEG
*&---------------------------------------------------------------------*

REPORT  zaa23dt_reporte_alv_integ.

*&---------------------------------------------------------------------*
*& TYPE-POOL
*&---------------------------------------------------------------------*
TYPE-POOLS:
  slis,
  icon.

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
    knumv TYPE vbrk-knumv,
  END OF ty_vbrk,

  tyt_vbrk TYPE STANDARD TABLE OF ty_vbrk.


TYPES:
  BEGIN OF ty_vbrp,
    vbeln TYPE vbrp-vbeln,
    posnr TYPE vbrp-posnr,
    matnr TYPE vbrp-matnr,
    fkimg TYPE vbrp-fkimg,
    vrkme TYPE vbrp-vrkme,
    meins TYPE vbrp-meins,
  END OF ty_vbrp,

  tyt_vbrp TYPE STANDARD TABLE OF ty_vbrp.


TYPES:
  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  tyt_makt TYPE STANDARD TABLE OF ty_makt.


TYPES:
  BEGIN OF ty_konv,
    knumv TYPE konv-knumv,
    kposn TYPE konv-kposn,
    stunr TYPE konv-stunr,
    zaehk TYPE konv-zaehk,
    kschl TYPE konv-kschl,
    kwert TYPE konv-kwert,
  END OF ty_konv,

  tyt_konv TYPE STANDARD TABLE OF ty_konv.

TYPES:
  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    belnr TYPE bkpf-belnr,
    gjahr TYPE bkpf-gjahr,
    awkey TYPE bkpf-awkey,
  END OF ty_bkpf,

  tyt_bkpf TYPE STANDARD TABLE OF ty_bkpf.

TYPES:
 BEGIN OF ty_alv,
   sel           TYPE char1,
   vbeln         TYPE vbrk-vbeln,
   belnr         TYPE bkpf-belnr,
   gjahr         TYPE bkpf-gjahr,
   posnr         TYPE vbrp-posnr,
   matnr         TYPE vbrp-matnr,
   maktx         TYPE makt-maktx,
   fkimg         TYPE vbrp-fkimg,
   vrkme         TYPE vbrp-vrkme,
   fkimg_base    TYPE vbrp-fkimg,
   meins         TYPE vbrp-meins,
   kwert_precio  TYPE konv-kwert,
   kwert_iva     TYPE konv-kwert,
   icono_iva     TYPE icon-id," Si IVA mayor a cero, ICON_PRICE, si es igual a cero ICON_AVERAGE
 END OF ty_alv,

 tyt_alv TYPE STANDARD TABLE OF ty_alv.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*
DATA:
  t_alv TYPE tyt_alv.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

SELECT-OPTIONS:
   s_vbeln FOR vbrk-vbeln,
   s_fkdat FOR vbrk-fkdat.

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
FORM get_data
  CHANGING
    ch_t_alv TYPE tyt_alv.

  FIELD-SYMBOLS:
    <lfs_vbrk>      TYPE ty_vbrk,
    <lfs_vbrp>      TYPE ty_vbrp,
    <lfs_bkpf>      TYPE ty_bkpf,
    <lfs_makt>      TYPE ty_makt,
    <lfs_konv_pr00> TYPE ty_konv,
    <lfs_konv_j1ax> TYPE ty_konv,
    <lfs_alv>       TYPE ty_alv.

  TYPES:
    BEGIN OF lty_vbrk_aux,
      vbeln TYPE bkpf-awkey,
    END OF lty_vbrk_aux.

  DATA:
    lt_vbrk     TYPE tyt_vbrk,
    lt_vbrp     TYPE tyt_vbrp,
    lt_makt     TYPE tyt_makt,
    lt_konv     TYPE tyt_konv,
    lt_bkpf     TYPE tyt_bkpf,
    lt_vbrk_aux TYPE STANDARD TABLE OF lty_vbrk_aux,
    le_vbrk_aux TYPE lty_vbrk_aux.

  SELECT vbeln knumv
    FROM vbrk
    INTO TABLE lt_vbrk
   WHERE vbeln IN s_vbeln
     AND fkdat IN s_fkdat
     AND vbtyp EQ 'M'.

  IF lt_vbrk[] IS NOT INITIAL.

    SELECT vbeln posnr matnr fkimg vrkme meins
      FROM vbrp
      INTO TABLE lt_vbrp
       FOR ALL ENTRIES IN lt_vbrk
     WHERE vbeln EQ lt_vbrk-vbeln.

    IF lt_vbrp[] IS NOT INITIAL.

      SELECT matnr maktx
        FROM makt
        INTO TABLE lt_makt
         FOR ALL ENTRIES IN lt_vbrp
       WHERE matnr EQ lt_vbrp-matnr
         AND spras EQ sy-langu.

    ENDIF.

    SELECT knumv kposn stunr zaehk kschl kwert
      FROM konv
      INTO TABLE lt_konv
       FOR ALL ENTRIES IN lt_vbrk
     WHERE knumv EQ lt_vbrk-knumv.

    LOOP AT lt_vbrk ASSIGNING <lfs_vbrk>.

      le_vbrk_aux-vbeln = <lfs_vbrk>-vbeln.
      APPEND le_vbrk_aux TO lt_vbrk_aux.

    ENDLOOP.

    IF lt_vbrk_aux[] IS NOT INITIAL.

      SELECT bukrs belnr gjahr awkey
        FROM bkpf
        INTO TABLE lt_bkpf
         FOR ALL ENTRIES IN lt_vbrk_aux
       WHERE awkey EQ lt_vbrk_aux-vbeln
         AND awtyp EQ 'VBRK'.

    ENDIF.

  ENDIF.

  LOOP AT lt_vbrk ASSIGNING <lfs_vbrk>.

    READ TABLE lt_bkpf ASSIGNING <lfs_bkpf>
      WITH KEY awkey = <lfs_vbrk>-vbeln.

    LOOP AT lt_vbrp ASSIGNING <lfs_vbrp>
      WHERE vbeln EQ <lfs_vbrk>-vbeln.

      READ TABLE lt_makt ASSIGNING <lfs_makt>
        WITH KEY matnr = <lfs_vbrp>-matnr.

      READ TABLE lt_konv ASSIGNING <lfs_konv_pr00>
        WITH KEY knumv = <lfs_vbrk>-knumv
                 kposn = <lfs_vbrp>-posnr
                 kschl = 'PR00'.

      READ TABLE lt_konv ASSIGNING <lfs_konv_j1ax>
        WITH KEY knumv = <lfs_vbrk>-knumv
                 kposn = <lfs_vbrp>-posnr
                 kschl = 'J1AX'.

      APPEND INITIAL LINE TO ch_t_alv ASSIGNING <lfs_alv>.

      <lfs_alv>-vbeln        = <lfs_vbrk>-vbeln.

      IF <lfs_bkpf> IS ASSIGNED.

        <lfs_alv>-belnr        = <lfs_bkpf>-belnr.
        <lfs_alv>-gjahr        = <lfs_bkpf>-gjahr.

      ENDIF.

      <lfs_alv>-posnr        = <lfs_vbrp>-posnr.
      <lfs_alv>-matnr        = <lfs_vbrp>-matnr.
      <lfs_alv>-fkimg        = <lfs_vbrp>-fkimg.
      <lfs_alv>-vrkme        = <lfs_vbrp>-vrkme.
      <lfs_alv>-meins        = <lfs_vbrp>-meins.

      IF <lfs_makt> IS ASSIGNED.
        <lfs_alv>-maktx        = <lfs_makt>-maktx.
      ENDIF.

      IF <lfs_konv_pr00> is ASSIGNED.
        <lfs_alv>-kwert_precio = <lfs_konv_pr00>-kwert.
      ENDIF.

      IF <lfs_konv_j1ax> is ASSIGNED.
        <lfs_alv>-kwert_iva    = <lfs_konv_j1ax>-kwert.
      ENDIF.

      IF <lfs_alv>-kwert_iva GT 0.
        <lfs_alv>-icono_iva  = icon_price.
      ELSE.
        <lfs_alv>-icono_iva  = icon_average.
      ENDIF.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = <lfs_alv>-matnr
          i_in_me              = <lfs_vbrp>-vrkme
          i_out_me             = <lfs_vbrp>-meins
          i_menge              = <lfs_vbrp>-fkimg
        IMPORTING
          e_menge              = <lfs_alv>-fkimg_base
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_REPORT
*&---------------------------------------------------------------------*
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

  le_sort-tabname   = 'T_ALV'.
  le_sort-fieldname = 'VBELN'.
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

  SET PF-STATUS 'STATUS_ALV' EXCLUDING us_t_extab. " Nombre del status creado

ENDFORM. "set_status_alv

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
     le_alv TYPE ty_alv.

  IF us_v_ucomm EQ 'PEDIDO'.

    PERFORM mostrar_pedido.

  ELSEIF us_v_ucomm EQ 'ENTREGA'.

    PERFORM mostrar_entrega.

  ELSEIF us_v_ucomm EQ '&IC1'.

    CASE us_e_selfield-fieldname.
      WHEN 'MATNR'.

        READ TABLE t_alv ASSIGNING <lfs_alv> INDEX us_e_selfield-tabindex.

        IF <lfs_alv> IS ASSIGNED.

          IF <lfs_alv>-matnr IS NOT INITIAL.

            SET PARAMETER ID 'MAT' FIELD <lfs_alv>-matnr.
            CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

          ENDIF.

        ENDIF.

      WHEN 'BELNR'.

        READ TABLE t_alv ASSIGNING <lfs_alv> INDEX us_e_selfield-tabindex.

        IF <lfs_alv> IS ASSIGNED.

          IF <lfs_alv>-belnr IS NOT INITIAL.

            SET PARAMETER ID 'BLN' FIELD <lfs_alv>-belnr.
            SET PARAMETER ID 'GJR' FIELD <lfs_alv>-gjahr.
            SET PARAMETER ID 'BUK' FIELD '1000'.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          ENDIF.

        ENDIF.

    ENDCASE.

  ENDIF.

ENDFORM. "alv_user_command


*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout
  CHANGING
    ch_e_layout TYPE slis_layout_alv.

  ch_e_layout-colwidth_optimize = 'X'. " Optimiza el tamaño de la celda
  ch_e_layout-zebra = 'X'. " Colorea las filas
  ch_e_layout-box_fieldname = 'SEL'. " Seleccionar linea del alv

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
FORM set_fieldcat
  CHANGING
    ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'VBELN'
       'Factura'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'BELNR'
       'Doc. Contable'
       'X'
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'GJAHR'
       'Ejercicio'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'POSNR'
       'Posición'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MATNR'
       'Material'
       'X'
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MAKTX'
       'Descripción'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'FKIMG'
       'Cantidad'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'VRKME'
       'Unidad de Venta'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'FKIMG_BASE'
       'Cantidad de UMB'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'MEINS'
       'Unidad Base'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'KWERT_PRECIO'
       'Precio'
       space
       'X'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'KWERT_IVA'
       'IVA'
       space
       space
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'ICONO_IVA'
       'Icono'
       space
       space
    CHANGING
      ch_t_fieldcat[].

ENDFORM.                    " SET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_FIELD
*&---------------------------------------------------------------------*
FORM set_fieldcat_field
  USING
     us_v_tabname
     us_v_field_name
     us_v_text
     us_v_hotspot
     us_v_do_sum
  CHANGING
     ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:
    le_fieldcat TYPE LINE OF slis_t_fieldcat_alv.

  CLEAR le_fieldcat.

  le_fieldcat-tabname    =  us_v_tabname.
  le_fieldcat-fieldname  =  us_v_field_name.
  le_fieldcat-seltext_l  =  us_v_text.
  le_fieldcat-hotspot    =  us_v_hotspot.
  le_fieldcat-do_sum     =  us_v_do_sum.

  APPEND le_fieldcat TO ch_t_fieldcat.


ENDFORM.                    " SET_FIELDCAT_FIELD
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_PEDIDO
*&---------------------------------------------------------------------*
FORM MOSTRAR_PEDIDO .

  FIELD-SYMBOLS:
    <lfs_alv> TYPE ty_alv.

  DATA:
    lv_vbelv TYPE vbfa-vbelv,
    lv_msg   TYPE string.

  READ TABLE t_alv ASSIGNING <lfs_alv>
    WITH KEY sel = 'X'.

  IF <lfs_alv> IS ASSIGNED.

    SELECT SINGLE vbelv
      FROM vbfa
      INTO lv_vbelv
     WHERE vbeln EQ <lfs_alv>-vbeln
       AND vbtyp_n EQ 'M'
       AND vbtyp_v EQ 'C'.

    IF sy-subrc EQ 0.

      CONCATENATE 'Pedido:' lv_vbelv INTO lv_msg SEPARATED BY space.

      MESSAGE lv_msg TYPE 'I'.

    ENDIF.

  ENDIF.

ENDFORM.                    " MOSTRAR_PEDIDO
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ENTREGA
*&---------------------------------------------------------------------*
FORM MOSTRAR_ENTREGA .

  FIELD-SYMBOLS:
    <lfs_alv> TYPE ty_alv.

  DATA:
    lv_vbelv TYPE vbfa-vbelv,
    lv_msg   TYPE string.

  READ TABLE t_alv ASSIGNING <lfs_alv>
    WITH KEY sel = 'X'.

  IF <lfs_alv> IS ASSIGNED.

    SELECT SINGLE vbelv
      FROM vbfa
      INTO lv_vbelv
     WHERE vbeln EQ <lfs_alv>-vbeln
       AND vbtyp_n EQ 'M'
       AND vbtyp_v EQ 'J'.

    IF sy-subrc EQ 0.

      CONCATENATE 'Entrega:' lv_vbelv INTO lv_msg SEPARATED BY space.

      MESSAGE lv_msg TYPE 'I'.

    ELSE.

      MESSAGE 'La Factura no tiene Entrega' TYPE 'I'.

    ENDIF.

  ENDIF.

ENDFORM.                    " MOSTRAR_ENTREGA