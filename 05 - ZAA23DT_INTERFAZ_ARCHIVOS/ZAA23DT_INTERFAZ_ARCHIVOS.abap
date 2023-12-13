*&---------------------------------------------------------------------*
*& Report  ZAA23DT_INTERFAZ_ARCHIVOS
*&---------------------------------------------------------------------*

REPORT  zaa23dt_interfaz_archivos.

*&---------------------------------------------------------------------*
*& TYPE-POOL
*&---------------------------------------------------------------------*
TYPE-POOLS:
  slis.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES:
  ty_t_string TYPE STANDARD TABLE OF string.

TYPES:
  BEGIN OF ty_file_pedidos,
    pedido  TYPE vbak-vbeln,
    fecha   TYPE d, " AAAAMMDD
    hora    TYPE t, " HHMMSS
    importe TYPE vbak-netwr,
    unidad  TYPE vbap-vrkme,
  END OF ty_file_pedidos,

  ty_t_file_pedidos TYPE STANDARD TABLE OF ty_file_pedidos.

TYPES:
  BEGIN OF ty_vbak,
    vbeln TYPE vbak-vbeln,
    auart TYPE vbak-auart,
    erdat TYPE vbak-erdat,
    erzet TYPE vbak-erzet,
  END OF ty_vbak,

  ty_t_vbak TYPE STANDARD TABLE OF ty_vbak.

TYPES:
  BEGIN OF ty_vbap,
    vbeln  TYPE vbap-vbeln,
    posnr  TYPE vbap-posnr,
    netwr  TYPE vbap-netwr,
    waerk  TYPE vbap-waerk,
    matnr  TYPE vbap-matnr,
    kwmeng TYPE vbap-kwmeng,
    vrkme  TYPE vbap-vrkme,
  END OF ty_vbap,

  ty_t_vbap TYPE STANDARD TABLE OF ty_vbap.

TYPES:
  BEGIN OF ty_alv,
    vbeln  TYPE vbak-vbeln,
    posnr  TYPE vbap-posnr,
    auart  TYPE vbak-auart,
    erdat  TYPE vbak-erdat,
    erzet  TYPE vbak-erzet,
    netwr  TYPE vbap-netwr,
    waerk  TYPE vbap-waerk,
    matnr  TYPE vbap-matnr,
    kwmeng TYPE vbap-kwmeng,
    vrkme  TYPE vbap-vrkme,
  END OF ty_alv,

  ty_t_alv TYPE STANDARD TABLE OF ty_alv.


*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*
DATA:
  t_file_pedidos TYPE ty_t_file_pedidos,
  t_alv          TYPE ty_t_alv.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

PARAMETERS:
  p_file TYPE string LOWER CASE.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.

PARAMETERS:
  p_pc RADIOBUTTON GROUP r02 DEFAULT 'X' USER-COMMAND usr2,
  p_sv RADIOBUTTON GROUP r02.

SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.

PARAMETERS:
  p_txt RADIOBUTTON GROUP r01 DEFAULT 'X' USER-COMMAND usr1,
  p_csv RADIOBUTTON GROUP r01.

SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-b04.

PARAMETERS:
  p_alv  RADIOBUTTON GROUP r03 DEFAULT 'X' USER-COMMAND usr3,
  p_down RADIOBUTTON GROUP r03.

PARAMETERS:
  p_fileo TYPE string LOWER CASE.

SELECTION-SCREEN END OF BLOCK b04.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.

  PERFORM hide_show_screen.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM get_filename
    CHANGING
      p_file.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fileo.

  PERFORM get_filename_down
    CHANGING
      p_fileo.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM upload_file
    USING
       p_file
    CHANGING
       t_file_pedidos[].

  PERFORM get_data
    USING
       t_file_pedidos[]
    CHANGING
       t_alv[].

  IF p_alv EQ 'X'.

    PERFORM show_alv
      USING
         t_alv[].

  ELSEIF p_down EQ 'X'.

    PERFORM download_file
     USING
        p_fileo
        t_alv[].

  ENDIF.





*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
FORM get_filename
  CHANGING
    ch_v_file.

  IF p_pc EQ 'X'.

    PERFORM get_filename_pc
      CHANGING
        ch_v_file.

  ELSEIF p_sv EQ 'X'.

    PERFORM get_filename_sv
      CHANGING
        ch_v_file.

  ENDIF.

ENDFORM.                    " GET_FILENAME

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM upload_file
  USING
     us_v_file
  CHANGING
     ch_t_file_pedidos TYPE ty_t_file_pedidos.

  DATA:
     lt_file TYPE ty_t_string.

  IF p_pc EQ 'X'.

    PERFORM upload_file_pc
      USING
        us_v_file
      CHANGING
        lt_file[].

  ELSEIF p_sv EQ 'X'.

    PERFORM upload_file_sv
      USING
        us_v_file
      CHANGING
        lt_file[].

  ENDIF.

  IF p_txt EQ 'X'.

    PERFORM format_txt
      USING
        lt_file[]
      CHANGING
        ch_t_file_pedidos[].

  ELSEIF p_csv EQ 'X'.

    PERFORM format_csv
      USING
        lt_file[]
      CHANGING
        ch_t_file_pedidos[].

  ENDIF.


ENDFORM.                    " UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
FORM upload_file_pc
  USING
    us_v_file
  CHANGING
    ch_t_file TYPE ty_t_string.

  DATA:
     lv_filename TYPE string.

  lv_filename = us_v_file.

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

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
*&      Form  FORMAT_TXT
*&---------------------------------------------------------------------*
FORM format_txt
  USING
    us_t_file         TYPE ty_t_string
  CHANGING
    ch_t_file_pedidos TYPE ty_t_file_pedidos.

  DATA:
    lv_string       TYPE string,
    lv_fecha        TYPE string,
    lv_hora         TYPE string,
    lv_importe      TYPE string,
    le_file_pedidos TYPE LINE OF ty_t_file_pedidos.

  LOOP AT us_t_file INTO lv_string.

    CHECK lv_string NE space.

    CLEAR le_file_pedidos.

    le_file_pedidos-pedido   = lv_string(4).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = le_file_pedidos-pedido
      IMPORTING
        output = le_file_pedidos-pedido.

    lv_fecha = lv_string+4(10).

    CONCATENATE lv_fecha+6(4)
                lv_fecha+3(2)
                lv_fecha+0(2)
           INTO le_file_pedidos-fecha.

    lv_hora = lv_string+14(8).

    CONCATENATE lv_hora+0(2)
                lv_hora+3(2)
                lv_hora+6(2)
           INTO le_file_pedidos-hora.

    lv_importe = lv_string+22(10).

    CONDENSE lv_importe.

    REPLACE ',' IN lv_importe WITH '.'.

    le_file_pedidos-importe  = lv_importe.

    le_file_pedidos-unidad   = lv_string+32(2).

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = le_file_pedidos-unidad
      IMPORTING
        output         = le_file_pedidos-unidad
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    APPEND le_file_pedidos TO ch_t_file_pedidos.

  ENDLOOP.

ENDFORM.                    " FORMAT_TXT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data
  USING
    us_t_file_pedidos TYPE ty_t_file_pedidos
  CHANGING
    ch_t_alv          TYPE ty_t_alv.

  DATA:
    lt_vbak TYPE ty_t_vbak,
    lt_vbap TYPE ty_t_vbap,
    le_vbak TYPE ty_vbak,
    le_vbap TYPE ty_vbap,
    le_alv  TYPE ty_alv.

*  IF us_t_file_pedidos[] IS INITIAL.
*    EXIT.
*  ENDIF.

  CHECK us_t_file_pedidos[] IS NOT INITIAL.

  " REFRESH us_t_fie_pedidos. " Limpia la tabla es el equivalente a CLEAR

  SELECT vbeln auart erdat erzet
    FROM vbak
    INTO TABLE lt_vbak
     FOR ALL ENTRIES IN us_t_file_pedidos
   WHERE vbeln EQ us_t_file_pedidos-pedido.

  IF lt_vbak[] IS NOT INITIAL.

    SELECT vbeln posnr netwr waerk matnr kwmeng vrkme
      FROM vbap
      INTO TABLE lt_vbap
       FOR ALL ENTRIES IN lt_vbak
     WHERE vbeln EQ lt_vbak-vbeln.

  ENDIF.

  LOOP AT lt_vbak INTO le_vbak.

    LOOP AT lt_vbap INTO le_vbap
      WHERE vbeln = le_vbak-vbeln.

      CLEAR le_alv.

      le_alv-vbeln  = le_vbak-vbeln.
      le_alv-posnr  = le_vbap-posnr.
      le_alv-auart  = le_vbak-auart.
      le_alv-erdat  = le_vbak-erdat.
      le_alv-erzet  = le_vbak-erzet.
      le_alv-netwr  = le_vbap-netwr.
      le_alv-waerk  = le_vbap-waerk.
      le_alv-matnr  = le_vbap-matnr.
      le_alv-kwmeng = le_vbap-kwmeng.
      le_alv-vrkme  = le_vbap-vrkme.

      APPEND le_alv TO ch_t_alv.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
FORM show_alv
  USING
    us_t_alv TYPE ty_t_alv.

  DATA:
    lv_repid     TYPE sy-repid VALUE sy-repid,
    le_layout    TYPE slis_layout_alv,
    lt_fieldcat  TYPE slis_t_fieldcat_alv.

  lv_repid = sy-repid.

  PERFORM set_layout
    CHANGING
      le_layout.

  PERFORM set_fieldcat
    CHANGING
      lt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = lv_repid
      is_layout          = le_layout
      it_fieldcat        = lt_fieldcat
    TABLES
      t_outtab           = us_t_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " SHOW_ALV

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout
  CHANGING
    ch_e_layout TYPE slis_layout_alv.

  ch_e_layout-colwidth_optimize = 'X'. " Optimiza el tamaño de la celda
  ch_e_layout-zebra = 'X'. " Colorea las filas

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
       'Pedido'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'POSNR'
       'Posición'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'AUART'
       'Clase Doc.'
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
       'ERZET'
       'Hora'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'NETWR'
       'Importe'
    CHANGING
      ch_t_fieldcat[].

  PERFORM set_fieldcat_field
    USING
       'T_ALV'
       'WAERK'
       'Moneda'
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
       'KWMENG'
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

ENDFORM.                    " SET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_FIELD
*&---------------------------------------------------------------------*
FORM set_fieldcat_field
  USING
     us_v_tabname
     us_v_field_name
     us_v_text
  CHANGING
     ch_t_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:
    le_fieldcat TYPE LINE OF slis_t_fieldcat_alv.

  CLEAR le_fieldcat.

  le_fieldcat-tabname    =  us_v_tabname.
  le_fieldcat-fieldname  =  us_v_field_name.
  le_fieldcat-seltext_l  =  us_v_text.

  APPEND le_fieldcat TO ch_t_fieldcat.


ENDFORM.                    " SET_FIELDCAT_FIELD
*&---------------------------------------------------------------------*
*&      Form  FORMAT_CSV
*&---------------------------------------------------------------------*
FORM format_csv
  USING
    us_t_file         TYPE ty_t_string
  CHANGING
    ch_t_file_pedidos TYPE ty_t_file_pedidos.

  DATA:
    lv_string       TYPE string,
    lv_fecha        TYPE string,
    lv_hora         TYPE string,
    lv_importe      TYPE string,
    le_file_pedidos TYPE LINE OF ty_t_file_pedidos.

  LOOP AT us_t_file INTO lv_string.

    CHECK lv_string NE space.

    CLEAR le_file_pedidos.

    SPLIT lv_string AT ';' INTO le_file_pedidos-pedido
                                lv_fecha
                                lv_hora
                                lv_importe
                                le_file_pedidos-unidad.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = le_file_pedidos-pedido
      IMPORTING
        output = le_file_pedidos-pedido.

    IF lv_fecha IS NOT INITIAL.

      CONCATENATE lv_fecha+6(4)
                  lv_fecha+3(2)
                  lv_fecha+0(2)
             INTO le_file_pedidos-fecha.

    ENDIF.

    IF lv_hora IS NOT INITIAL.

      CONCATENATE lv_hora+0(2)
                  lv_hora+3(2)
                  lv_hora+6(2)
             INTO le_file_pedidos-hora.

    ENDIF.

    CONDENSE lv_importe.

    REPLACE ',' IN lv_importe WITH '.'.

    le_file_pedidos-importe  = lv_importe.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = le_file_pedidos-unidad
      IMPORTING
        output         = le_file_pedidos-unidad
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    APPEND le_file_pedidos TO ch_t_file_pedidos.

  ENDLOOP.

ENDFORM.                    " FORMAT_CSV
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_SV
*&---------------------------------------------------------------------*
FORM upload_file_sv
  USING
    us_v_file
  CHANGING
    ch_t_file TYPE ty_t_string.

  DATA:
    lv_string TYPE string.

  OPEN DATASET us_v_file FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.

  IF sy-subrc EQ 0.

    DO.
      READ DATASET us_v_file INTO lv_string.

      IF sy-subrc EQ 0.
        APPEND lv_string TO ch_t_file.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CLOSE DATASET us_v_file.

  ENDIF.

ENDFORM.                    " UPLOAD_FILE_SV
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_PC
*&---------------------------------------------------------------------*
FORM get_filename_pc
  CHANGING
    ch_v_file.

  FIELD-SYMBOLS:
    <lsf_file_table> TYPE LINE OF filetable.

  DATA:
    lv_default_extension  TYPE string,
    lv_file_filter        TYPE string,
    lt_file_table         TYPE filetable,
    lv_rc                 TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_extension       = lv_default_extension
      file_filter             = lv_file_filter
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

    READ TABLE lt_file_table ASSIGNING <lsf_file_table> INDEX 1.

    IF <lsf_file_table> IS ASSIGNED.

      ch_v_file = <lsf_file_table>-filename.

    ENDIF.

  ENDIF.

ENDFORM.                    " GET_FILENAME_PC
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_SV
*&---------------------------------------------------------------------*
FORM get_filename_sv
  CHANGING
    ch_v_file.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
*   EXPORTING
*     DIRECTORY              = ' '
*     FILEMASK               = ' '
   IMPORTING
     serverfile             = ch_v_file
   EXCEPTIONS
     canceled_by_user       = 1
     OTHERS                 = 2.


ENDFORM.                    " GET_FILENAME_SV
*&---------------------------------------------------------------------*
*&      Form  HIDE_SHOW_SCREEN
*&---------------------------------------------------------------------*
FORM hide_show_screen .

  LOOP AT SCREEN.
    " Esto loopea la tabla (con cabecera) que almacena los select-screen items para cambiar los atributos.

    IF p_alv EQ 'X'.

      IF screen-name CS 'P_FILEO'.
        " CS = Contain String

        screen-active = 0.
        MODIFY SCREEN.

      ENDIF.

    ELSEIF p_down EQ 'X'.

      IF screen-name CS 'P_FILEO'.

        screen-active = 1.
        MODIFY SCREEN.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " HIDE_SHOW_SCREEN
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_DOWN
*&---------------------------------------------------------------------*
FORM get_filename_down
  CHANGING
    ch_v_fileo.

  IF p_pc EQ 'X'.

    PERFORM get_filename_pc_down
      CHANGING
        ch_v_fileo.

  ELSEIF p_sv EQ 'X'.

    PERFORM get_filename_sv_down
      CHANGING
        ch_v_fileo.

  ENDIF.

ENDFORM.                    " GET_FILENAME_DOWN
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_PC_DOWN
*&---------------------------------------------------------------------*
FORM get_filename_pc_down
  CHANGING
    ch_v_fileo.

  DATA:
    lv_window_title        TYPE string,
    lv_default_extension   TYPE string,
    lv_file_filter         TYPE string,
    lv_filename            TYPE string,
    lv_path                TYPE string,
    lv_selected_folder     TYPE string,
    lv_fullpath            TYPE string.

*  lv_window_title      = 'Guardar Archivo'.
*  lv_default_extension = '*.txt'.
*  lv_file_filter       = '*.txt'.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
*      window_title              = lv_window_title
*      default_extension         = lv_default_extension
*      file_filter               = lv_file_filter
*    CHANGING
*      filename                  = lv_filename
*      path                      = lv_path
*      fullpath                  = lv_fullpath
*    EXCEPTIONS
*      cntl_error                = 1
*      error_no_gui              = 2
*      not_supported_by_gui      = 3
*      invalid_default_file_name = 4
*      others                    = 5.
*
*  ch_v_fileo = lv_fullpath.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = lv_selected_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  ch_v_fileo = lv_selected_folder.

ENDFORM.                    " GET_FILENAME_PC_DOWN
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_SV_DOWN
*&---------------------------------------------------------------------*
FORM get_filename_sv_down
  CHANGING
    ch_v_fileo.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = ch_v_fileo
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.

ENDFORM.                    " GET_FILENAME_SV_DOWN
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM download_file
  USING
    us_v_file
    us_t_alv TYPE ty_t_alv.

  DATA:
    lv_filename TYPE string,
    lt_file     TYPE ty_t_string.

  CONCATENATE us_v_file
              '/'
              'Pedidos'
              sy-datum
              sy-uzeit INTO lv_filename. "ESTE es el nombre del archivo

  IF p_txt EQ 'X'.

    CONCATENATE lv_filename '.txt' INTO lv_filename.

  ELSEIF p_csv EQ 'X'.

    CONCATENATE lv_filename '.csv' INTO lv_filename. " LE AGREGO la extension

  ENDIF.


  PERFORM format_out_file
    USING
      us_t_alv[]
    CHANGING
      lt_file[].

  IF p_pc EQ 'X'.

    PERFORM download_file_to_pc
      USING
        lv_filename
        lt_file[].

  ELSEIF p_sv EQ 'X'.

    PERFORM download_file_to_sv
      USING
        lv_filename
        lt_file[].

  ENDIF.

ENDFORM.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  FORMAT_OUT_FILE
*&---------------------------------------------------------------------*
FORM format_out_file
  USING
    us_t_alv  TYPE ty_t_alv
  CHANGING
    ch_t_file TYPE ty_t_string.

  DATA:
    le_alv    TYPE ty_alv,
    lv_string TYPE string.

  LOOP AT us_t_alv INTO le_alv.

    CLEAR lv_string.

    IF p_txt EQ 'X'.

      PERFORM format_out_txt_line
        USING
          le_alv
        CHANGING
          lv_string.

    ELSEIF p_csv EQ 'X'.

      PERFORM format_out_csv_line
        USING
          le_alv
        CHANGING
          lv_string.

    ENDIF.

    APPEND lv_string TO ch_t_file.

  ENDLOOP.

ENDFORM.                    " FORMAT_OUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FORMAT_OUT_TXT_LINE
*&---------------------------------------------------------------------*
FORM format_out_txt_line
  USING
    us_e_alv   TYPE ty_alv
  CHANGING
    ch_v_string.

  DATA:
    lv_posnr_aux   TYPE char10,
    lv_hora_aux    TYPE char8,
    lv_fecha_aux   TYPE char10,
    lv_netwr_aux   TYPE char20,
    lv_kwmeng_aux  TYPE char20.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = us_e_alv-vbeln
    IMPORTING
      output = us_e_alv-vbeln.

  SHIFT us_e_alv-vbeln RIGHT DELETING TRAILING space.

  lv_posnr_aux = us_e_alv-posnr.
  SHIFT lv_posnr_aux LEFT DELETING LEADING '0'.
  SHIFT lv_posnr_aux RIGHT DELETING TRAILING space.

  CONCATENATE us_e_alv-erdat+6(2)
              us_e_alv-erdat+4(2)
              us_e_alv-erdat+0(2)
         INTO lv_fecha_aux SEPARATED BY '/'.

  CONCATENATE us_e_alv-erzet+0(2)
              us_e_alv-erzet+2(2)
              us_e_alv-erzet+4(2)
         INTO lv_hora_aux SEPARATED BY ':'.

  lv_netwr_aux = us_e_alv-netwr.
  REPLACE '.' IN lv_netwr_aux WITH ','.
  SHIFT lv_netwr_aux RIGHT DELETING TRAILING space.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = us_e_alv-matnr
    IMPORTING
      output = us_e_alv-matnr.

  SHIFT us_e_alv-matnr RIGHT DELETING TRAILING space.

  lv_kwmeng_aux = us_e_alv-kwmeng.
  REPLACE '.' IN lv_kwmeng_aux WITH ','.
  SHIFT lv_kwmeng_aux RIGHT DELETING TRAILING space.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = us_e_alv-vrkme
    IMPORTING
      output         = us_e_alv-vrkme
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  CONCATENATE us_e_alv-vbeln
              lv_posnr_aux
              us_e_alv-auart
              lv_fecha_aux
              lv_hora_aux
              lv_netwr_aux
              us_e_alv-waerk
              us_e_alv-matnr
              lv_kwmeng_aux
              us_e_alv-vrkme
         INTO ch_v_string RESPECTING BLANKS.

ENDFORM.                    " FORMAT_OUT_TXT_LINE
*&---------------------------------------------------------------------*
*&      Form  FORMAT_OUT_CSV_LINE
*&---------------------------------------------------------------------*
FORM format_out_csv_line
  USING
    us_e_alv   TYPE ty_alv
  CHANGING
    ch_v_string.

  DATA:
    lv_posnr_aux     TYPE c LENGTH 10,
    lv_fecha_aux     TYPE c LENGTH 10,
    lv_hora_aux      TYPE c LENGTH 8,
    lv_importe_aux   TYPE c LENGTH 20,
    lv_cantidad_aux  TYPE c LENGTH 20.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = us_e_alv-vbeln
    IMPORTING
      output = us_e_alv-vbeln.

  CONDENSE us_e_alv-vbeln.

  lv_posnr_aux = us_e_alv-posnr.
  SHIFT lv_posnr_aux LEFT DELETING LEADING '0'.
  CONDENSE lv_posnr_aux.

  CONCATENATE us_e_alv-erdat+6(2)
              us_e_alv-erdat+4(2)
              us_e_alv-erdat+0(4)
         INTO lv_fecha_aux SEPARATED BY '/'.

  CONCATENATE us_e_alv-erzet+0(2)
              us_e_alv-erzet+2(2)
              us_e_alv-erzet+4(2)
         INTO lv_hora_aux SEPARATED BY ':'.

  lv_importe_aux = us_e_alv-netwr.
  REPLACE '.' IN lv_importe_aux WITH ','.
  CONDENSE lv_importe_aux.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = us_e_alv-matnr
    IMPORTING
      output = us_e_alv-matnr.

  CONDENSE us_e_alv-matnr.

  lv_cantidad_aux = us_e_alv-kwmeng.
  REPLACE '.' IN lv_cantidad_aux WITH ','.
  CONDENSE lv_cantidad_aux.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = us_e_alv-vrkme
    IMPORTING
      output         = us_e_alv-vrkme
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  CONDENSE us_e_alv-vrkme.

  CONCATENATE us_e_alv-vbeln
              lv_posnr_aux
              us_e_alv-auart
              lv_fecha_aux
              lv_hora_aux
              lv_importe_aux
              us_e_alv-waerk
              us_e_alv-matnr
              lv_cantidad_aux
              us_e_alv-vrkme
         INTO ch_v_string SEPARATED BY ';'.



ENDFORM.                    " FORMAT_OUT_CSV_LINE
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE_TO_PC
*&---------------------------------------------------------------------*
FORM download_file_to_pc
  USING
    us_v_filename
    us_t_file TYPE ty_t_string.

  DATA:
    lv_filename TYPE string.

  lv_filename = us_v_filename.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = lv_filename
    CHANGING
      data_tab                = us_t_file
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.

ENDFORM.                    " DOWNLOAD_FILE_TO_PC

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE_TO_SV
*&---------------------------------------------------------------------*
FORM download_file_to_sv
  USING
    us_v_filename
    us_t_file TYPE ty_t_string.

  DATA:
    lv_string TYPE string.

  OPEN DATASET us_v_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc EQ 0.

    LOOP AT us_t_file INTO lv_string.

      TRANSFER lv_string TO us_v_filename.

    ENDLOOP.

    CLOSE DATASET us_v_filename.

  ENDIF.

ENDFORM.                    " DOWNLOAD_FILE_TO_SV