*&---------------------------------------------------------------------*
*& Report  ZCDYMOPCBHTOHGD
*&---------------------------------------------------------------------*

report  zcdymopcbhtohgd.

*&---------------------------------------------------------------------*
*& INCLUDE
*&---------------------------------------------------------------------*
include:
  zcdymopcbhtohgd_top,
  zcdymopcbhtohgd_dyn.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

selection-screen begin of block b01 with frame title text-b01.

select-options:
   s_bukrs for bkpf-bukrs, " Sociedad
   s_belnr for bkpf-belnr, " Documento
   s_gjahr for bkpf-gjahr, " Ejercicio
   s_bldat for bkpf-bldat, " Fecha de Documento
   s_lifnr for vbsegk-lifnr no intervals. " Acreedor

selection-screen end of block b01.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*

end-of-selection.

  perform get_data
    changing
      t_alv[].

  if t_alv[] is not initial.

    perform show_report
      using
        t_alv[].

  else.

    message text-e01 type 'S' display like 'E'.

  endif.

*&---------------------------------------------------------------------*
*&   Form  GET_DATA
*&---------------------------------------------------------------------*
form get_data
  changing
    ch_t_alv type tyt_alv.

  data:
    lt_bkpf   type tyt_bkpf,
    lt_t003t  type tyt_t003t,
    lt_tstct  type tyt_tstct,
    lt_tcurt  type tyt_tcurt,
    lt_vbsegk type tyt_vbsegk,
    lt_tbslt  type tyt_tbslt,
    lt_lfa1   type tyt_lfa1,
    lt_obs    type tyt_obs.

  perform get_data_db
    changing
      lt_bkpf[]
      lt_t003t[]
      lt_tstct[]
      lt_tcurt[]
      lt_vbsegk[]
      lt_tbslt[]
      lt_lfa1[]
      lt_obs[].

  perform process_data
    using
      lt_bkpf[]
      lt_t003t[]
      lt_tstct[]
      lt_tcurt[]
      lt_vbsegk[]
      lt_tbslt[]
      lt_lfa1[]
      lt_obs[]
    changing
      ch_t_alv[].

endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DB
*&---------------------------------------------------------------------*
form get_data_db
  changing
    ch_t_bkpf   type tyt_bkpf
    ch_t_t003t  type tyt_t003t
    ch_t_tstct  type tyt_tstct
    ch_t_tcurt  type tyt_tcurt
    ch_t_vbsegk type tyt_vbsegk
    ch_t_tbslt  type tyt_tbslt
    ch_t_lfa1   type tyt_lfa1
    ch_t_obs    type tyt_obs.

  select bukrs belnr gjahr blart bldat budat xblnr monat tcode waers
    from bkpf
    into table ch_t_bkpf
   where bukrs in s_bukrs
     and belnr in s_belnr
     and gjahr in s_gjahr
     and bldat in s_bldat.

  if ch_t_bkpf[] is not initial.

    select blart ltext
      from t003t
      into table ch_t_t003t
       for all entries in ch_t_bkpf
     where spras eq sy-langu
       and blart eq ch_t_bkpf-blart.

    sort ch_t_t003t by blart.

    select tcode ttext
      from tstct
      into table ch_t_tstct
       for all entries in ch_t_bkpf
     where sprsl eq sy-langu
       and tcode eq ch_t_bkpf-tcode.

    sort ch_t_tstct by tcode.

    select waers ltext
      from tcurt
      into table ch_t_tcurt
       for all entries in ch_t_bkpf
     where spras eq sy-langu
       and waers eq ch_t_bkpf-waers.

    sort ch_t_tcurt by waers.

    select ausbk belnr gjahr bzkey bschl lifnr zfbdt zbd1t wrbtr shkzg
      from vbsegk
      into table ch_t_vbsegk
       for all entries in ch_t_bkpf
     where ausbk = ch_t_bkpf-bukrs
       and belnr = ch_t_bkpf-belnr
       and gjahr = ch_t_bkpf-gjahr
       and lifnr in s_lifnr.

    if ch_t_vbsegk[] is not initial.

      select bschl ltext
        from tbslt
        into table ch_t_tbslt
         for all entries in ch_t_vbsegk
       where spras eq sy-langu
         and bschl eq ch_t_vbsegk-bschl.

      sort ch_t_tbslt by bschl.

      select lifnr name1
        from lfa1
        into table ch_t_lfa1
         for all entries in ch_t_vbsegk
       where lifnr eq ch_t_vbsegk-lifnr.

      sort ch_t_lfa1 by lifnr.

      select mandt bukrs belnr gjahr bzkey observacion
        from zcdymopcbhtohgdt
        into table ch_t_obs
         for all entries in ch_t_vbsegk
       where bukrs eq ch_t_vbsegk-ausbk
         and belnr eq ch_t_vbsegk-belnr
         and gjahr eq ch_t_vbsegk-gjahr
         and bzkey eq ch_t_vbsegk-bzkey.

    endif.

  endif.

endform.                    " GET_DATA_DB
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
form process_data
  using
    us_t_bkpf   type tyt_bkpf
    us_t_t003t  type tyt_t003t
    us_t_tstct  type tyt_tstct
    us_t_tcurt  type tyt_tcurt
    us_t_vbsegk type tyt_vbsegk
    us_t_tbslt  type tyt_tbslt
    us_t_lfa1   type tyt_lfa1
    us_t_obs   type tyt_obs
  changing
    ch_t_alv  type tyt_alv.

  field-symbols:
    <lfs_bkpf>   type line of tyt_bkpf,
    <lfs_t003t>  type line of tyt_t003t,
    <lfs_tstct>  type line of tyt_tstct,
    <lfs_tcurt>  type line of tyt_tcurt,
    <lfs_vbsegk> type line of tyt_vbsegk,
    <lfs_tbslt>  type line of tyt_tbslt,
    <lfs_lfa1>   type line of tyt_lfa1,
    <lfs_obs>    type line of tyt_obs,
    <lfs_alv>    type line of tyt_alv.

  data:
    lv_wrbtr     type vbsegk-wrbtr,
    lv_wrbtr_txt type c length 13.

  if us_t_bkpf[] is not initial.

    loop at us_t_bkpf assigning <lfs_bkpf>.

      if <lfs_bkpf> is assigned.

        unassign <lfs_t003t>.

        read table us_t_t003t assigning <lfs_t003t> binary search
          with key blart = <lfs_bkpf>-blart.

        unassign <lfs_tstct>.

        read table us_t_tstct assigning <lfs_tstct> binary search
          with key tcode = <lfs_bkpf>-tcode.

        unassign <lfs_tcurt>.

        read table us_t_tcurt assigning <lfs_tcurt> binary search
          with key waers = <lfs_bkpf>-waers.

        loop at us_t_vbsegk assigning <lfs_vbsegk>
          where ausbk = <lfs_bkpf>-bukrs
            and belnr = <lfs_bkpf>-belnr
            and gjahr = <lfs_bkpf>-gjahr.

          if <lfs_vbsegk> is assigned.

            unassign <lfs_tbslt>.

            read table us_t_tbslt assigning <lfs_tbslt> binary search
              with key bschl = <lfs_vbsegk>-bschl.

            unassign <lfs_lfa1>.

            read table us_t_lfa1 assigning <lfs_lfa1> binary search
              with key lifnr = <lfs_vbsegk>-lifnr.

            unassign <lfs_obs>.

            read table us_t_obs assigning <lfs_obs> binary search
              with key bukrs = <lfs_vbsegk>-ausbk
                       belnr = <lfs_vbsegk>-belnr
                       gjahr = <lfs_vbsegk>-gjahr
                       bzkey = <lfs_vbsegk>-bzkey.

            append initial line to ch_t_alv assigning <lfs_alv>.

            <lfs_alv>-bukrs       = <lfs_bkpf>-bukrs.
            <lfs_alv>-belnr       = <lfs_bkpf>-belnr.
            <lfs_alv>-gjahr       = <lfs_bkpf>-gjahr.
            <lfs_alv>-blart       = <lfs_bkpf>-blart.
            <lfs_alv>-bldat       = <lfs_bkpf>-bldat.
            <lfs_alv>-budat       = <lfs_bkpf>-budat.
            <lfs_alv>-xblnr       = <lfs_bkpf>-xblnr.
            <lfs_alv>-monat       = <lfs_bkpf>-monat.
            <lfs_alv>-tcode       = <lfs_bkpf>-tcode.
            <lfs_alv>-waers       = <lfs_bkpf>-waers.

            <lfs_alv>-bzkey       = <lfs_vbsegk>-bzkey.
            <lfs_alv>-bschl       = <lfs_vbsegk>-bschl.
            <lfs_alv>-lifnr       = <lfs_vbsegk>-lifnr.
            <lfs_alv>-zfbdt       = <lfs_vbsegk>-zfbdt.
            <lfs_alv>-zfbdt_venc  = <lfs_vbsegk>-zfbdt + <lfs_vbsegk>-zbd1t.

            if <lfs_t003t> is assigned.
              <lfs_alv>-blart_desc  = <lfs_t003t>-ltext.
            endif.

            if <lfs_tstct> is assigned.

              <lfs_alv>-tcode_desc  = <lfs_tstct>-ttext.

            endif.

            if <lfs_tbslt> is assigned.

              <lfs_alv>-bschl_desc  = <lfs_tbslt>-ltext.

            endif.

            if <lfs_lfa1> is assigned.

              <lfs_alv>-lifnr_name1 = <lfs_lfa1>-name1.

            endif.

            if <lfs_vbsegk>-shkzg = 'H'.

              <lfs_alv>-wrbtr = <lfs_vbsegk>-wrbtr * -1.

            else.

              <lfs_alv>-wrbtr = <lfs_vbsegk>-wrbtr.

            endif.

            if <lfs_tcurt> is assigned.

              <lfs_alv>-waers_desc = <lfs_tcurt>-ltext.

            endif.

            if <lfs_obs> is assigned.

              <lfs_alv>-observacion = <lfs_obs>-observacion.

            endif.

            call function 'ZCDYMOPCBHTOHGD_GET_MONTH_NAME'
              exporting
                im_month      = <lfs_bkpf>-monat
              importing
                ex_month_name = <lfs_alv>-monat_name.

          endif.

        endloop.

      endif.

    endloop.

  endif.


endform.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_REPORT
*&---------------------------------------------------------------------*
form show_report
  using
    us_t_alv type tyt_alv.

  data:
    lv_callback_program type sy-repid,
    le_layout           type slis_layout_alv,
    lt_fieldcat         type slis_t_fieldcat_alv,
    lt_sort             type slis_t_sortinfo_alv,
    le_sort             type line of slis_t_sortinfo_alv.

  lv_callback_program = sy-repid.

  perform set_layout
    changing
      le_layout.

  perform set_fieldcat
    changing
      lt_fieldcat[].

  clear le_sort.

  le_sort-tabname   = 'T_ALV'.
  le_sort-fieldname = 'BELNR'.
  le_sort-subtot    = 'X'.

  append le_sort to lt_sort.

  sort us_t_alv.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = lv_callback_program
      is_layout                = le_layout
      it_fieldcat              = lt_fieldcat
      i_callback_pf_status_set = 'SET_STATUS_ALV'
      i_callback_user_command  = 'ALV_USER_COMMAND'
      it_sort                  = lt_sort
    tables
      t_outtab                 = us_t_alv
    exceptions
      program_error            = 1
      others                   = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " SHOW_REPORT

*&---------------------------------------------------------------------*
*& Form SET_STATUS_ALV
*&---------------------------------------------------------------------*
form set_status_alv
  using
     us_t_extab type slis_t_extab.

  set pf-status 'STATUS_ALV' excluding us_t_extab.

endform. "set_status_alv

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
form set_layout
  changing
    ch_e_layout type slis_layout_alv.

  ch_e_layout-colwidth_optimize = abap_true.
  ch_e_layout-zebra = abap_true.
  ch_e_layout-box_fieldname = 'SEL'.

endform.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
form set_fieldcat
  changing
    ch_t_fieldcat type slis_t_fieldcat_alv.

  perform set_fieldcat_field
    using
       'T_ALV'
       'BUKRS'
       'Sociedad'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BELNR'
       'Documento'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'GJAHR'
       'Ejercicio'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BLART'
       'Clase de Doc.'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BLART_DESC'
       'Descripción'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BLDAT'
       'Fecha de Documento'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BUDAT'
       'Fecha de Contabilización'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'XBLNR'
       'Referencia'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'MONAT'
       'Periodo'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'MONAT_NAME'
       'Periodo'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'TCODE'
       'Transacción'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'TCODE_DESC'
       'Descripción'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BZKEY'
       'Posición'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BSCHL'
       'Clave de contabiliz.'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'BSCHL_DESC'
       'Descripción'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'LIFNR'
       'Acreedor'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'LIFNR_NAME1'
       'Nombre'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'ZFBDT'
       'Fecha Base'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'ZFBDT_VENC'
       'Fecha de Venc.'
        abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'WRBTR'
       'Importe'
       abap_true
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'WAERS'
       'Moneda'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'WAERS_DESC'
       'Descripción'
       abap_false
    changing
      ch_t_fieldcat[].

  perform set_fieldcat_field
    using
       'T_ALV'
       'OBSERVACION'
       'Observación'
       abap_false
    changing
      ch_t_fieldcat[].

endform.                    " SET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_FIELD
*&---------------------------------------------------------------------*
form set_fieldcat_field
  using
     us_v_tabname
     us_v_field_name
     us_v_text
     us_v_do_sum
  changing
     ch_t_fieldcat type slis_t_fieldcat_alv.

  data:
    le_fieldcat type line of slis_t_fieldcat_alv.

  clear le_fieldcat.

  le_fieldcat-tabname         =  us_v_tabname.
  le_fieldcat-fieldname       =  us_v_field_name.
  le_fieldcat-seltext_l       =  us_v_text.
  le_fieldcat-do_sum          =  us_v_do_sum.

  append le_fieldcat to ch_t_fieldcat.

endform.                    " SET_FIELDCAT_FIELD

*&---------------------------------------------------------------------*
*& Form ALV_USER_COMMAND
*&---------------------------------------------------------------------*
form alv_user_command
  using
     us_v_ucomm    type sy-ucomm
     us_e_selfield type slis_selfield.

  field-symbols:
     <lfs_alv> type ty_alv.

  data:
     le_alv type ty_alv,
     lv_only_one   type string.

  if us_v_ucomm eq 'DOWNLOAD'.

    perform download_csv.

  elseif us_v_ucomm eq 'SHOW_FORM'.

    perform show_smartform.

  elseif us_v_ucomm eq 'ADD_OBS'.

    perform is_only_one_selected
      changing
         lv_only_one.

    if lv_only_one eq abap_true.

      call screen 9000 starting at 5 5 ending at 65 15.

    else.

      message 'Debe escoger solo un registro' type 'S' display like 'E'.

    endif.

  endif.

endform. "alv_user_command
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_CSV
*&---------------------------------------------------------------------*
form download_csv.

  data:
    lv_selected_folder  type string,
    lv_filename         type string,
    lt_sel_rows         type tyt_alv,
    lt_csv_string       type tyt_string.

  perform get_selected_rows
      changing
        lt_sel_rows[].

  if lt_sel_rows[] is not initial.

    perform get_folder_route
      changing
        lv_selected_folder.

    if lv_selected_folder is not initial.

      concatenate lv_selected_folder
                  '/'
                  'doc_contables_acreedores'
                  sy-datum
                  sy-uzeit
                  '.csv'   into lv_filename.

        perform format_out_file
          using
            lt_sel_rows
          changing
            lt_csv_string.

        perform download_file_to_pc
          using
            lv_filename
            lt_csv_string[].

      endif.

  else.

        message 'No se ha seleccionado ninguna linea' type 'S' display like 'E'.

  endif.

endform.                    " DOWNLOAD_CSV
*&---------------------------------------------------------------------*
*&      Form  GET_FOLDER_ROUTE
*&---------------------------------------------------------------------*
form get_folder_route
  changing
    ch_v_selected_folder type string.

  call method cl_gui_frontend_services=>directory_browse
    changing
      selected_folder      = ch_v_selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.

endform.                    " GET_FOLDER_ROUTE
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
form get_selected_rows
  changing
    ch_t_alv_sel_rows type tyt_alv.

  data:
     le_alv_row          type line of tyt_alv.

  loop at t_alv into le_alv_row
    where sel = 'X'.

    append le_alv_row to ch_t_alv_sel_rows.

  endloop.

endform.                    " GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  FORMAT_OUT_FILE
*&---------------------------------------------------------------------*
form format_out_file
  using
    us_t_sel_rows    type tyt_alv
  changing
    ch_t_csv_string  type tyt_string.

  field-symbols:
    <lfs_sel_row>  type line of tyt_alv.

  data:
    lv_bldat_aux   type c length 10,
    lv_budat_aux   type c length 10,
    lv_bzkey_aux   type c length 3,
    lv_lifnr_aux   type c length 10,
    lv_zfbdt_aux   type c length 10,
    lv_wrbtr_aux   type c length 20,
    le_csv_row     type line of tyt_string.

  loop at us_t_sel_rows assigning <lfs_sel_row>.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = <lfs_sel_row>-belnr
      importing
        output = <lfs_sel_row>-belnr.

    condense <lfs_sel_row>-belnr.

    clear lv_bldat_aux.

    concatenate <lfs_sel_row>-bldat+6(2)
                <lfs_sel_row>-bldat+4(2)
                <lfs_sel_row>-bldat+0(4)
           into lv_bldat_aux separated by '/'.

    clear lv_budat_aux.

    concatenate <lfs_sel_row>-budat+6(2)
                <lfs_sel_row>-budat+4(2)
                <lfs_sel_row>-budat+0(4)
           into lv_budat_aux separated by '/'.

    lv_bzkey_aux = <lfs_sel_row>-bzkey.
    shift lv_bzkey_aux left deleting leading '0'.
    condense lv_bzkey_aux.

    clear lv_lifnr_aux.

    lv_lifnr_aux = <lfs_sel_row>-lifnr.
    shift lv_lifnr_aux left deleting leading '0'.
    condense lv_lifnr_aux.

    clear lv_zfbdt_aux.

    concatenate <lfs_sel_row>-zfbdt+6(2)
                <lfs_sel_row>-zfbdt+4(2)
                <lfs_sel_row>-zfbdt+0(4)
           into lv_zfbdt_aux separated by '/'.

    lv_wrbtr_aux = <lfs_sel_row>-wrbtr.
    replace '.' in lv_wrbtr_aux with ','.
    condense lv_wrbtr_aux.

    concatenate <lfs_sel_row>-bukrs
                <lfs_sel_row>-belnr
                lv_bldat_aux
                lv_budat_aux
                <lfs_sel_row>-xblnr
                <lfs_sel_row>-monat
                <lfs_sel_row>-tcode
                lv_bzkey_aux
                <lfs_sel_row>-bschl
                lv_lifnr_aux
                lv_zfbdt_aux
                lv_wrbtr_aux
                <lfs_sel_row>-waers
                into le_csv_row separated by ';'.

    append le_csv_row to ch_t_csv_string.

  endloop.

endform.                    " FORMAT_OUT_FILE

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE_TO_PC
*&---------------------------------------------------------------------*
form download_file_to_pc
  using
    us_v_filename
    us_t_file type tyt_string.

  data:
    lv_filename type string.

  lv_filename = us_v_filename.

  call method cl_gui_frontend_services=>gui_download
    exporting
      filename                = lv_filename
    changing
      data_tab                = us_t_file
    exceptions
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
      others                  = 24.

  if sy-subrc eq 0.
    message 'Documento descargado con exito' type 'E' display like 'S'.
  endif.


endform.                    " DOWNLOAD_FILE_TO_PC
*&---------------------------------------------------------------------*
*&      Form  SHOW_SMARTFORM
*&---------------------------------------------------------------------*
form show_smartform.

  field-symbols:
    <lfs_alv>     type ty_alv,
    <lfs_alv_pos> type ty_alv.

  data:
  s_cabecera   type zcdymopcbhtohgd_sf_header,
  t_posiciones type zcdymopcbhtohgd_sf_items_tyt,
  s_posiciones type line of zcdymopcbhtohgd_sf_items_tyt,
  lv_fm_name   type rs38l_fnam,
  lv_mensaje   type string,
  le_alv_row   type line of tyt_alv,
  lv_only_one   type string.

  perform is_only_one_selected
    changing
      lv_only_one.

  if lv_only_one eq abap_true.

    read table t_alv assigning <lfs_alv>
      with key sel = 'X'.

    s_cabecera-bukrs       = <lfs_alv>-bukrs.
    s_cabecera-belnr       = <lfs_alv>-belnr.
    s_cabecera-gjahr       = <lfs_alv>-gjahr.
    s_cabecera-bldat       = <lfs_alv>-bldat.
    s_cabecera-budat       = <lfs_alv>-budat.
    s_cabecera-blart       = <lfs_alv>-blart.
    s_cabecera-blart_desc  = <lfs_alv>-blart_desc.
    s_cabecera-xblnr       = <lfs_alv>-xblnr.
    s_cabecera-waers       = <lfs_alv>-waers.


    loop at t_alv assigning <lfs_alv_pos>
      where bukrs = <lfs_alv>-bukrs
        and belnr = <lfs_alv>-belnr
        and gjahr = <lfs_alv>-gjahr.

      s_posiciones-bschl    = <lfs_alv_pos>-bschl.
      s_posiciones-lifnr    = <lfs_alv_pos>-lifnr.
      s_posiciones-wrbtr    = <lfs_alv_pos>-wrbtr.

      add s_posiciones-wrbtr to s_cabecera-total.

      append s_posiciones to t_posiciones.

    endloop.

    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = 'ZCDYMOPCBHTOHGD_SMARTFORM'
      importing
        fm_name            = lv_fm_name
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.

    if sy-subrc eq 0.

      call function lv_fm_name
        exporting
          cabecera         = s_cabecera
          posiciones       = t_posiciones
        exceptions
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          user_canceled    = 4
          others           = 5.

    else.

      message 'Error' type 'S' display like 'E'.

    endif.

  else.

    message 'Debe escoger solo un registro' type 'S' display like 'E'.

  endif.


endform.                    " SHOW_SMARTFORM
*&---------------------------------------------------------------------*
*&      Form  IS_ONLY_ONE_SELECTED
*&---------------------------------------------------------------------*
form is_only_one_selected
  changing
  ch_v_only_one   type string.

  data:
     lv_condition type string,
     le_alv_row    type line of tyt_alv,
     selected_rows type tyt_alv.

  loop at t_alv into le_alv_row
    where sel = 'X'.

    append le_alv_row to selected_rows.

  endloop.

  describe table selected_rows lines lv_condition.

  if lv_condition eq 1.

    ch_v_only_one = abap_true.

  else.

    ch_v_only_one = abap_false.

  endif.

endform.                    " IS_ONLY_ONE_SELECTED
*&---------------------------------------------------------------------*
*&      Form  INIT_9000
*&---------------------------------------------------------------------*
form init_9000.

  field-symbols:
    <lfs_alv>     type ty_alv.

    read table t_alv assigning <lfs_alv>
      with key sel = 'X'.

    clear s_9000.

    s_9000-bukrs       = <lfs_alv>-bukrs.
    s_9000-belnr       = <lfs_alv>-belnr.
    s_9000-gjahr       = <lfs_alv>-gjahr.
    s_9000-bzkey       = <lfs_alv>-bzkey.

endform.                    " INIT_9000
*&---------------------------------------------------------------------*
*&      Form  SAVE_9000
*&---------------------------------------------------------------------*
form save_9000.

  data:
    ls_observacion type ty_9000.

  if s_9000-observacion is not initial.

    ls_observacion-mandt       = sy-mandt.
    ls_observacion-bukrs       = s_9000-bukrs.
    ls_observacion-belnr       = s_9000-belnr.
    ls_observacion-gjahr       = s_9000-gjahr.
    ls_observacion-bzkey       = s_9000-bzkey.
    ls_observacion-observacion = s_9000-observacion.

    modify zcdymopcbhtohgdt from ls_observacion.

    if sy-subrc eq 0.

      message 'Datos guardados' type 'S'.
      leave to screen 0.

    else.

      message 'Error al guardar. Intente nuevamente' type 'S' display like 'E'.

    endif.

  else.

    message 'No se ha guardado, la observacion no puede ser nula' type 'S' display like 'E'.
    leave to screen 0.

  endif.

endform.                    " SAVE_9000