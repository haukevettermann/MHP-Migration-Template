*&---------------------------------------------------------------------*
*& Report ZMHP_RP_MIG_BAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmhp_rp_mig_bal.

TABLES: pernr,
        sscrfields.

NODES: peras.

DATA: pernrs    TYPE rsdsselopt_t,
      it_aend   TYPE zmhp_int_it_aeup,
      p2006     type p2006_tab,
      it_aendup TYPE zmhp_tt_it_aeup.

INITIALIZATION.
  pnptimed = 'A'.

  "additional options
  SELECTION-SCREEN BEGIN OF BLOCK sel_file WITH FRAME TITLE TEXT-001.
  PARAMETERS: pa_simu TYPE flag DEFAULT abap_true.
  PARAMETERS: pa_alv  TYPE flag DEFAULT abap_true.
  SELECTION-SCREEN END OF BLOCK sel_file.

GET peras.
  "collect all pernr
  APPEND VALUE #( sign = 'I' option = 'EQ' low = pernr-pernr ) TO pernrs.

END-of-SELECTION.

  IF pernrs IS NOT INITIAL.

    "select all relevant entries from
    SELECT * FROM pa2006 INTO CORRESPONDING FIELDS OF TABLE p2006 WHERE pernr IN pernrs    AND
                                                                        sprps NE abap_true AND
                                                                        begda LE pn-endda  AND
                                                                        endda GE pn-begda.

    cl_hr_pnnnn_type_cast=>pnnnn_to_prelp_tab( EXPORTING pnnnn_tab = p2006
                                               IMPORTING prelp_tab = DATA(prelp) ).

    it_aendup = CORRESPONDING #( prelp ).

    it_aend-infty = '2006'.
    it_aend-mandt = sy-mandt.
    it_aend-timestamp = sy-timlo.
    it_aend-aedtm = sy-datum.
    it_aend-uname = sy-uname.
    it_aend-status = zmhp_cl_int_constants=>booking_status-initial.

    MODIFY it_aendup FROM it_aend TRANSPORTING mandt infty timestamp aedtm uname status WHERE status IS INITIAL.

    LOOP AT it_aendup ASSIGNING FIELD-SYMBOL(<aend_up>).
      <aend_up>-int_run = zmhp_cl_int_utility=>create_guid( ).
    ENDLOOP.

    DATA(mig_bal) = NEW zmhp_cl_mig_bal( simu  = pa_simu
                                         alv   = pa_alv
                                         batch = sy-batch ).

    "get skipped pernrs according to auth check
    pnp_get_auth_skipped_pernrs mig_bal->skipped_pernrs.

    "check simu
    IF pa_simu IS INITIAL.
      MODIFY zmhp_int_it_aeup FROM TABLE it_aendup.
    ENDIF.

    "prepare protocol
    mig_bal->prepare_protocol( it_aendup ).

  ENDIF.
