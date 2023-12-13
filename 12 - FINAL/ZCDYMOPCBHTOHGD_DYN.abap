*&---------------------------------------------------------------------*
*&  Include           ZCDYMOPCBHTOHGD_DYN
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
module status_9000 output.
  set pf-status 'STATUS_9000'.
  set titlebar 'TITLE_9000'.

endmodule.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
module user_command_9000 input.

  case sy-ucomm.

    when 'SAVE'.

      perform save_9000.

      leave to screen 0.

    when 'BACK' or 'EXIT' or 'CANCEL' or 'VOLVER'.

      leave to screen 0.

  endcase.

  clear sy-ucomm.

endmodule.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_9000  OUTPUT
*&---------------------------------------------------------------------*
module init_9000 output.

  perform init_9000.

endmodule.                 " INIT_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
module user_command_exit_command input.

  case sy-ucomm.

    when 'BACK' or 'EXIT' or 'CANCEL' or 'VOLVER'.

      leave to screen 0.

  endcase.

  clear sy-ucomm.

endmodule.                 " USER_COMMAND_EXIT_COMMAND  INPUT