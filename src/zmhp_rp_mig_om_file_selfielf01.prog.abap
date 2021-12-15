*----------------------------------------------------------------------*
***INCLUDE ZMHP_RP_MIG_OM_FILE_SELFIELF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELFIELDS_VORBELEGEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_selfields.
  DATA: plvar TYPE plvar.

  "Aktive Planvariante ermitteln
  CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
    IMPORTING
      act_plvar       = plvar
    EXCEPTIONS
      no_active_plvar = 1
      OTHERS          = 2.

  pchplvar = plvar.   "Aktive Planvariante
  pchotype = 'O'.     "Organisationseinheit
  pchostat = 1.       "Status

ENDFORM.
