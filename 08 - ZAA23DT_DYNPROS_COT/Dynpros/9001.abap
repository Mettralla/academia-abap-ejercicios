PROCESS BEFORE OUTPUT.
 MODULE STATUS_9001.

 MODULE init_9001.

PROCESS AFTER INPUT.
 MODULE user_command_exit_command AT EXIT-COMMAND.

 MODULE USER_COMMAND_9001.

   CHAIN. " Validacion del centro
    FIELD s_9001-werks MODULE validar_werks_9001 ON CHAIN-INPUT.
   ENDCHAIN.