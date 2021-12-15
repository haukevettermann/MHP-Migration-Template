*&---------------------------------------------------------------------*
*&  Include           ZMHP_RP_MIG_OM_FILE_SELOUTPU01
*&---------------------------------------------------------------------*
FORM deactivate_otype.
    LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'PCHOTYPE'.
        screen-input = 0.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
