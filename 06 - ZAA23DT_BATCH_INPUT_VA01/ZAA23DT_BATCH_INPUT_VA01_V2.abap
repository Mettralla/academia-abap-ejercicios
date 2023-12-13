*&---------------------------------------------------------------------*
*& Report  ZAA23DT_BATCH_INPUT_VA02
*&---------------------------------------------------------------------*

REPORT  ZAA23DT_BATCH_INPUT_VA02.

*&---------------------------------------------------------------------*
*& TYPE-POOL
*&---------------------------------------------------------------------*
TYPE-POOLS:
  abap.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  tyt_string  TYPE STANDARD TABLE OF string,
  tyt_bdcdata TYPE STANDARD TABLE OF bdcdata.

TYPES:
  BEGIN OF ty_posiciones_file,
    material TYPE vbap-matnr,
    cantidad TYPE vbap-kwmeng,
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

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*
DATA:
  t_pedidos_file TYPE tyt_pedidos_file.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.

PARAMETERS:
  p_file TYPE string LOWER CASE OBLIGATORY.

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

  PERFORM batch_input_va01
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
    lv_string        TYPE string,
    lv_tipo          TYPE c LENGTH 1,
    lv_pedido        TYPE i,
    lv_cantidad_char TYPE c LENGTH 30,
    le_pedidos_file  TYPE ty_pedidos_file,
    le_posiciones_file TYPE ty_posiciones_file.

  LOOP AT us_t_file INTO lv_string.

    IF lv_string(1) EQ 'H'.

      IF le_pedidos_file IS NOT INITIAL.
        APPEND le_pedidos_file TO ch_t_pedidos_file.
      ENDIF.

      CLEAR:
        le_pedidos_file.

      SPLIT lv_string AT ';' INTO lv_tipo
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

    ELSEIF lv_string(1) EQ 'D'.

      CLEAR:
        le_posiciones_file.

      SPLIT lv_string AT ';' INTO lv_tipo
                                  le_posiciones_file-material
                                  lv_cantidad_char
                                  le_posiciones_file-centro.

      CONDENSE lv_cantidad_char.
      le_posiciones_file-cantidad = lv_cantidad_char.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = le_posiciones_file-material
        IMPORTING
          output       = le_posiciones_file-material
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      APPEND le_posiciones_file TO le_pedidos_file-posiciones.

    ENDIF.

    AT LAST.
      APPEND le_pedidos_file TO ch_t_pedidos_file.
    ENDAT.

  ENDLOOP.

  SORT ch_t_pedidos_file BY pedido.

ENDFORM.                    "format_csv

*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT_VA01
*&---------------------------------------------------------------------*
FORM batch_input_va01
  USING
     us_t_pedidos_file TYPE tyt_pedidos_file.

  DATA:
    le_pedidos_file     TYPE ty_pedidos_file,
    le_posiciones_file  TYPE ty_posiciones_file,
    lt_bdcdata          TYPE tyt_bdcdata,
    le_options          TYPE ctu_params,
    lv_posicion         TYPE n LENGTH 2,
    lv_field            TYPE bdcdata-fnam,
    lv_cantidad         TYPE char30,
    lt_messages         TYPE tab_bdcmsgcoll,
    le_messages         TYPE LINE OF tab_bdcmsgcoll,
    lv_mensaje          TYPE string.

  le_options-dismode  = 'N'.
  le_options-updmode  = 'S'.
  le_options-racommit = abap_true.

  LOOP AT us_t_pedidos_file INTO le_pedidos_file.

    REFRESH:
      lt_bdcdata,
      lt_messages.

    CLEAR lv_posicion.

    PERFORM bdc_dynpro USING 'SAPMV45A'
                             '0101'
                    CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'BDC_OKCODE'
                            '/00'
                   CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'VBAK-AUART'
                            le_pedidos_file-clase_pedido
                   CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'VBAK-VKORG'
                            le_pedidos_file-org_ventas
                   CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'VBAK-VTWEG'
                            le_pedidos_file-canal_distr
                   CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'VBAK-SPART'
                            le_pedidos_file-sector
                   CHANGING lt_bdcdata.

    PERFORM bdc_dynpro USING 'SAPMV45A'
                             '4001'
                    CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'BDC_OKCODE'
                            '/00'
                   CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'VBKD-BSTKD'
                            le_pedidos_file-pedido_cliente
                   CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'KUAGV-KUNNR'
                            le_pedidos_file-solicitante
                   CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'KUWEV-KUNNR'
                            le_pedidos_file-solicitante
                   CHANGING lt_bdcdata.

    LOOP AT le_pedidos_file-posiciones INTO le_posiciones_file.

      ADD 1 TO lv_posicion.

      PERFORM bdc_dynpro USING 'SAPMV45A'
                               '4001'
                      CHANGING lt_bdcdata.

      PERFORM bdc_field USING 'BDC_OKCODE'
                              '/00'
                     CHANGING lt_bdcdata.

      CONCATENATE 'RV45A-MABNR(' lv_posicion ')' INTO lv_field.

      PERFORM bdc_field USING lv_field
                              le_posiciones_file-material
                     CHANGING lt_bdcdata.

      CONCATENATE 'RV45A-KWMENG(' lv_posicion ')' INTO lv_field.

      WRITE le_posiciones_file-cantidad TO lv_cantidad.
      CONDENSE lv_cantidad.

      PERFORM bdc_field USING lv_field
                              lv_cantidad
                     CHANGING lt_bdcdata.

      IF le_posiciones_file-centro IS NOT INITIAL.

        CONCATENATE 'VBAP-WERKS(' lv_posicion ')' INTO lv_field.

        PERFORM bdc_field USING lv_field
                                le_posiciones_file-centro
                       CHANGING lt_bdcdata.

      ENDIF.

    ENDLOOP.

    PERFORM bdc_dynpro USING 'SAPMV45A'
                             '4001'
                    CHANGING lt_bdcdata.

    PERFORM bdc_field USING 'BDC_OKCODE'
                            '=SICH'
                   CHANGING lt_bdcdata.

    CALL TRANSACTION 'VA01'
               USING lt_bdcdata
        OPTIONS FROM le_options
       MESSAGES INTO lt_messages.

    WRITE: / 'Log de creación de Pedido'.

    LOOP AT lt_messages INTO le_messages.

      MESSAGE ID le_messages-msgid
            TYPE le_messages-msgtyp
          NUMBER le_messages-msgnr
            WITH le_messages-msgv1
                 le_messages-msgv2
                 le_messages-msgv3
                 le_messages-msgv4
            INTO lv_mensaje.

      WRITE: / lv_mensaje.

    ENDLOOP.

    WRITE: / '--------------------------------------------------'.

  ENDLOOP.

ENDFORM.                    " BATCH_INPUT_VA01

*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
FORM bdc_dynpro
  USING
    us_v_program
    us_v_dynpro
  CHANGING
    ch_t_bdcdata TYPE tyt_bdcdata.

  DATA:
    ls_bdcdata TYPE bdcdata.

  ls_bdcdata-program  = us_v_program.
  ls_bdcdata-dynpro   = us_v_dynpro.
  ls_bdcdata-dynbegin = abap_true.

  APPEND ls_bdcdata TO ch_t_bdcdata.

ENDFORM. "bdc_dynpro

*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
FORM bdc_field
  USING
    us_v_fnam
    us_v_fval
  CHANGING
    ch_t_bdcdata TYPE tyt_bdcdata.

  DATA:
    ls_bdcdata TYPE bdcdata.

  ls_bdcdata-fnam = us_v_fnam.
  ls_bdcdata-fval = us_v_fval.

  APPEND ls_bdcdata TO ch_t_bdcdata.

ENDFORM. "bdc_field