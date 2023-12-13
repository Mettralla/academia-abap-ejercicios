*&---------------------------------------------------------------------*
*& Report  ZAADT_ALV_FACTORY
*&---------------------------------------------------------------------*

report zaadt_alv_factory.

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*
tables:
  vbak.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
types:
  begin of ty_alv,
    vbeln type vbak-vbeln,
    netwr type vbak-netwr,
    erdat type vbak-erdat,
    erzet type vbak-erzet,
    ernam type vbak-ernam,
    angdt type vbak-angdt,
    bnddt type vbak-bnddt,
    audat type vbak-audat,
    vbtyp type vbak-vbtyp,
    trvog type vbak-trvog,
    auart type vbak-auart,
    augru type vbak-augru,
    gwldt type vbak-gwldt,
    submi type vbak-submi,
  end of ty_alv,

  ty_t_alv type standard table of ty_alv.

*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*
data:
  t_alv type ty_t_alv.

data:
  o_alv         type ref to cl_salv_table,
  o_cx_salv_msg type ref to cx_salv_msg.

*----------------------------------------------------------------------*
*       CLASS lcl_events DEFINITION
*----------------------------------------------------------------------*
class lcl_events definition.

  public section.

    methods:

      on_link_click for event link_click of cl_salv_events_table
        importing
          row
          column,

     on_user_command for event added_function of cl_salv_events
       importing
         e_salv_function.

endclass.                    "lcl_events DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_events IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_events implementation.

  method on_link_click.

    field-symbols <fs_alv> type ty_alv.

    read table t_alv assigning <fs_alv> index row.
    if <fs_alv> is assigned.

      case column.
        when 'VBELN'. " Pedido

          set parameter id 'AUN' field <fs_alv>-vbeln.
          call transaction 'VA03' and skip first screen.

      endcase.

    endif.

  endmethod.                    "on_link_click

  method on_user_command.

    case e_salv_function.

      when 'ACCION'.

        perform custom_form.

    endcase.

  endmethod.                    "on_user_command

endclass.                    "lcl_events IMPLEMENTATION

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
selection-screen begin of block b01 with frame title text-b01.

select-options:
  s_vbeln for vbak-vbeln.

selection-screen end of block b01.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
end-of-selection.

  select vbeln
         netwr
         erdat
         erzet
         ernam
         angdt
         bnddt
         audat
         vbtyp
         trvog
         auart
         augru
         gwldt
         submi
    from vbak
    into table t_alv
   where vbeln in s_vbeln.

**********************************************************************

  data:
    lo_sel           type ref to cl_salv_selections,
    lo_events        type ref to cl_salv_events_table,
    lo_event_handler type ref to lcl_events.

  call method cl_salv_table=>factory
    importing
      r_salv_table = o_alv
    changing
      t_table      = t_alv.

  call method o_alv->set_screen_status
    exporting
      report        = sy-repid
      pfstatus      = 'ALV_STATUS'
      set_functions = o_alv->c_functions_all.

  perform change_columns_properties
    using
      o_alv.

  " Para poder seleccionar registros
  lo_sel = o_alv->get_selections( ).
  lo_sel->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  " Eventos del ALV
  lo_events = o_alv->get_event( ).

  create object lo_event_handler.

  " Hotspot
  set handler lo_event_handler->on_link_click for lo_events.

  " User Command
  set handler lo_event_handler->on_user_command for lo_events.

  o_alv->display( ).

*&---------------------------------------------------------------------*
*&      Form  CHANGE_COLUMNS_PROPERTIES
*&---------------------------------------------------------------------*
form change_columns_properties
  using
    us_o_alv type ref to cl_salv_table.

  data:
    lo_columns type ref to cl_salv_columns_table,
    lo_column  type ref to cl_salv_column_table.

  lo_columns = us_o_alv->get_columns( ).

  lo_columns->set_optimize( abap_true ).

  lo_column ?= lo_columns->get_column( 'VBELN' ).
  lo_column->set_short_text( 'Pedido' ).
  lo_column->set_medium_text( 'Pedido' ).
  lo_column->set_long_text( 'Pedido' ).
  lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

endform.                    " CHANGE_COLUMNS_PROPERTIES

*&---------------------------------------------------------------------*
*&      Form  CUSTOM_FORM
*&---------------------------------------------------------------------*
form custom_form.

  field-symbols:
    <fs_alv> type line of ty_t_alv.

  data:
    lo_sel type ref to  cl_salv_selections,
    lt_sel type         salv_t_row,
    lv_sel type line of salv_t_row.

  lo_sel = o_alv->get_selections( ).
  lt_sel[] = lo_sel->get_selected_rows( ).

  loop at lt_sel into lv_sel.

    unassign <fs_alv>.

    read table t_alv assigning <fs_alv> index lv_sel.

    message <fs_alv>-vbeln type 'I'.

  endloop.

endform.                    " CUSTOM_FORM