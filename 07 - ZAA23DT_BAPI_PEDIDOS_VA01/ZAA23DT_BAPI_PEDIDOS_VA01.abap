*&---------------------------------------------------------------------*
*& Report  ZAA23DT_BAPI_PEDIDOS_VA01
*&---------------------------------------------------------------------*

REPORT  ZAA23DT_BAPI_PEDIDOS_VA01.

*&---------------------------------------------------------------------*
*& TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS:
  abap.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  tyt_string TYPE STANDARD TABLE OF string.

TYPES:
  BEGIN OF ty_posiciones_file,
    material TYPE vbap-matnr,
    cantidad TYPE vbap-kwmeng,
    unidad   TYPE vbap-vrkme,
    centro   TYPE vbap-werks,
  END OF ty_posiciones_file,

  tyt_posiciones_file TYPE STANDARD TABLE OF ty_posiciones_file WITH DEFAULT KEY.

TYPES:
  BEGIN OF ty_pedidos_file,
    pedido         TYPE i,
    clase_pedido   TYPE vbak-auart,
    org_ventas     TYPE vbak-vkorg,
    canal_distr    TYPE vbak-vtweg,
    sector         TYPE vbak-spart,
    solicitante    TYPE vbak-kunnr,
    pedido_cliente TYPE vbak-bstnk,
    posiciones     TYPE tyt_posiciones_file,
  END OF ty_pedidos_file,

  tyt_pedidos_file TYPE STANDARD TABLE OF ty_pedidos_file.

DATA:
  t_pedidos_file TYPE tyt_pedidos_file.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.

PARAMETERS:
  p_file TYPE admi_file.

SELECTION-SCREEN END OF BLOCK b01.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM get_filename
    CHANGING
      p_file.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM upload_file
    USING
      p_file
    CHANGING
      t_pedidos_file[].

  PERFORM bapi_crea_pedidos
    USING
      t_pedidos_file[].

*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
FORM get_filename
  CHANGING
    ch_v_filename.

  PERFORM get_filename_pc
    CHANGING
      ch_v_filename.

ENDFORM. " GET_FILENAME

*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_PC
*&---------------------------------------------------------------------*
FORM get_filename_pc
  CHANGING
    ch_v_filename.

  DATA:
    lv_default_filename TYPE string,
    lv_filter           TYPE string,
    lt_file_table       TYPE filetable,
    lw_file_table       TYPE LINE OF filetable,
    lv_rc               TYPE i.

  lv_default_filename = '*.*'.
  lv_filter           = '*.*'.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_filename        = lv_default_filename
      file_filter             = lv_filter
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc EQ 0.

    CLEAR lw_file_table.
    READ TABLE lt_file_table INTO lw_file_table INDEX 1.
    IF sy-subrc EQ 0.

      MOVE lw_file_table-filename TO ch_v_filename.

    ENDIF.

  ENDIF.

ENDFORM.                    "get_filename_pc

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM upload_file
  USING
    us_filename
  CHANGING
    ch_t_pedidos_file TYPE tyt_pedidos_file.

  DATA:
    lt_file TYPE tyt_string.

  PERFORM upload_file_pc
    USING
      us_filename
    CHANGING
      lt_file[].

  PERFORM format_file
    USING
      lt_file
    CHANGING
      ch_t_pedidos_file.

ENDFORM. " UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
FORM upload_file_pc
  USING
    us_filename
  CHANGING
    ch_t_file   TYPE tyt_string.

  DATA:
    lv_filename TYPE string.

  MOVE us_filename TO lv_filename.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filename
    CHANGING
      data_tab                = ch_t_file
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.

ENDFORM.                    "upload_file_pc

*&---------------------------------------------------------------------*
*&      Form  FORMAT_FILE
*&---------------------------------------------------------------------*
FORM format_file
  USING
    us_t_file         TYPE tyt_string
  CHANGING
    ch_t_pedidos_file TYPE tyt_pedidos_file.

  DATA:
    lv_string          TYPE string,
    lv_tipo            TYPE c LENGTH 1,
    lv_pedido          TYPE i,
    lv_cantidad        TYPE c LENGTH 30,
    le_pedidos_file    TYPE ty_pedidos_file,
    le_posiciones_file TYPE ty_posiciones_file.

  LOOP AT us_t_file INTO lv_string.

    IF lv_string(1) = 'H'.

      IF le_pedidos_file IS NOT INITIAL.
        APPEND le_pedidos_file TO ch_t_pedidos_file.
      ENDIF.

      CLEAR:
        le_pedidos_file.

      SPLIT lv_string AT ';'
        INTO lv_tipo
             le_pedidos_file-clase_pedido
             le_pedidos_file-org_ventas
             le_pedidos_file-canal_distr
             le_pedidos_file-sector
             le_pedidos_file-solicitante
             le_pedidos_file-pedido_cliente.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = le_pedidos_file-solicitante
        IMPORTING
          output = le_pedidos_file-solicitante.

      ADD 1 TO lv_pedido.
      le_pedidos_file-pedido = lv_pedido.

    ELSEIF lv_string(1) = 'D'.

      CLEAR:
        le_posiciones_file.

      SPLIT lv_string AT ';'
        INTO lv_tipo
             le_posiciones_file-material
             lv_cantidad
             le_posiciones_file-unidad
             le_posiciones_file-centro.

      le_posiciones_file-cantidad = lv_cantidad.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = le_posiciones_file-material
        IMPORTING
          output       = le_posiciones_file-material
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = le_posiciones_file-unidad
        IMPORTING
          output         = le_posiciones_file-unidad
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.

      APPEND le_posiciones_file TO le_pedidos_file-posiciones.

    ENDIF.

    AT LAST.

      IF le_pedidos_file IS NOT INITIAL.
        APPEND le_pedidos_file TO ch_t_pedidos_file.
      ENDIF.

    ENDAT.

  ENDLOOP.

  SORT ch_t_pedidos_file BY pedido.

ENDFORM.                    "format_csv

*&---------------------------------------------------------------------*
*&      Form  BAPI_CREA_PEDIDOS
*&---------------------------------------------------------------------*
FORM bapi_crea_pedidos
  USING
    us_t_pedidos_file TYPE tyt_pedidos_file.

  DATA:
    ls_pedidos_file        LIKE LINE OF us_t_pedidos_file,
    ls_posiciones          LIKE LINE OF ls_pedidos_file-posiciones,
    "ls_posiciones          TYPE ty_posiciones_file,
    ls_order_header_in     TYPE bapisdhd1,
    ls_order_header_inx    TYPE bapisdhd1x,
    ls_order_partners      TYPE bapiparnr,
    lt_order_partners      TYPE STANDARD TABLE OF bapiparnr,
    ls_order_items_in      TYPE bapisditm,
    lt_order_items_in      TYPE STANDARD TABLE OF bapisditm,
    ls_order_items_inx     TYPE bapisditmx,
    lt_order_items_inx     TYPE STANDARD TABLE OF bapisditmx,
    ls_order_schedules_in  TYPE bapischdl,
    lt_order_schedules_in  TYPE STANDARD TABLE OF bapischdl,
    ls_order_schedules_inx TYPE bapischdlx,
    lt_order_schedules_inx TYPE STANDARD TABLE OF bapischdlx,
    ls_return              TYPE bapiret2,
    lt_return              TYPE STANDARD TABLE OF bapiret2,
    lv_salesdocument       TYPE bapivbeln-vbeln,
    lv_itm_number          LIKE ls_order_items_in-itm_number.

  LOOP AT us_t_pedidos_file INTO ls_pedidos_file.

    CLEAR:
      ls_order_header_in,
      ls_order_header_inx,
      lv_salesdocument.

    REFRESH:
      lt_order_partners,
      lt_return,
      lt_order_items_in,
      lt_order_items_inx,
      lt_order_schedules_in,
      lt_order_schedules_inx.

    ls_order_header_in-doc_type  = ls_pedidos_file-clase_pedido.
    ls_order_header_inx-doc_type = abap_true.

    ls_order_header_in-sales_org  = ls_pedidos_file-org_ventas.
    ls_order_header_inx-sales_org = abap_true.

    ls_order_header_in-distr_chan  = ls_pedidos_file-canal_distr.
    ls_order_header_inx-distr_chan = abap_true.

    ls_order_header_in-division  = ls_pedidos_file-sector.
    ls_order_header_inx-division = abap_true.

    ls_order_header_in-purch_no_c  = ls_pedidos_file-pedido_cliente.
    ls_order_header_inx-purch_no_c = abap_true.

    CLEAR ls_order_partners.
    ls_order_partners-partn_role = 'AG'. " AG -> Solicitante
    ls_order_partners-partn_numb = ls_pedidos_file-solicitante.
    APPEND ls_order_partners TO lt_order_partners.

    CLEAR ls_order_partners.
    ls_order_partners-partn_role = 'WE'. " WE -> Destinatario
    ls_order_partners-partn_numb = ls_pedidos_file-solicitante.
    APPEND ls_order_partners TO lt_order_partners.

    LOOP AT ls_pedidos_file-posiciones INTO ls_posiciones.

      CLEAR:
        ls_order_items_in,
        ls_order_items_inx,
        ls_order_schedules_in,
        ls_order_schedules_inx.

      ADD 1 TO lv_itm_number.

      ls_order_items_in-itm_number  = lv_itm_number.
      ls_order_items_inx-itm_number = 'X'.

      ls_order_items_in-material  = ls_posiciones-material.
      ls_order_items_inx-material = 'X'.

      ls_order_items_in-plant  = ls_posiciones-centro.
      ls_order_items_inx-plant = 'X'.

      ls_order_items_in-target_qty  = ls_posiciones-cantidad.
      ls_order_items_inx-target_qty = 'X'.

      APPEND ls_order_items_in  TO lt_order_items_in.
      APPEND ls_order_items_inx TO lt_order_items_inx.

*      ls_order_schedules_in-itm_number  = lv_itm_number.
*      ls_order_schedules_inx-itm_number = 'X'.
*
*      ls_order_schedules_in-req_qty  = ls_posiciones-cantidad.
*      ls_order_schedules_inx-req_qty = 'X'.
*
*      APPEND ls_order_schedules_in  TO lt_order_schedules_in.
*      APPEND ls_order_schedules_inx TO lt_order_schedules_inx.

    ENDLOOP.

    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
        order_header_in     = ls_order_header_in
        order_header_inx    = ls_order_header_inx " confirmacion de cargado
      IMPORTING
        salesdocument       = lv_salesdocument
      TABLES
        return              = lt_return " tabla de mensajes
        order_items_in      = lt_order_items_in " las posiciones
        order_items_inx     = lt_order_items_inx " confirmacion de cargado
        order_partners      = lt_order_partners " solicitante y destinatario
        order_schedules_in  = lt_order_schedules_in
        order_schedules_inx = lt_order_schedules_inx. " confirmacion de cargado

    IF lv_salesdocument IS NOT INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

    WRITE: / 'Log de creación de Pedido'.

    LOOP AT lt_return INTO ls_return.
      WRITE: / ls_return-message.
    ENDLOOP.

    WRITE: / '--------------------------------------------------'.

  ENDLOOP.

ENDFORM.                    " BAPI_CREA_PEDIDOS