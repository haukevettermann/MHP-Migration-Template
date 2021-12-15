class ZMHP_CL_MIG_BAL definition
  public
  create public .

public section.

  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data SKIPPED_PERNRS type HRAHQ_PERNR_TABLE .
  data SIMU type BOOLEAN .

  methods PREPARE_PROTOCOL
    importing
      !IT_AEUP type ZMHP_TT_IT_AEUP .
  methods CONSTRUCTOR
    importing
      !SIMU type BOOLEAN
      !ALV type BOOLEAN
      !BATCH type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS ZMHP_CL_MIG_BAL IMPLEMENTATION.


METHOD constructor.

  me->simu  = simu.

  "prepare protocol in case dialog is active and ALV is requested
  IF alv   EQ abap_true AND
     batch EQ abap_false.
    message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
  ENDIF.

ENDMETHOD.


METHOD prepare_protocol.

  DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

  "Check message handler
  IF message_handler IS BOUND.

    "prepare layout options
    DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                          colwidth_optimize = 'X' ).

    "Define structure of successful and failed IT_AEND entries ALV table
    message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = 'ZMHP_INT_IT_AEUP'
                                                          IMPORTING et_fcat          = DATA(fcat_it_aend) ).

    message_handler->if_hrpay00_pal_services~add_table(
      EXPORTING
        i_parent_node_key = root_node
        it_fcat           = fcat_it_aend
        it_append_table   = it_aeup
        is_layout         = layout
        i_node_txt        = COND string( WHEN simu EQ abap_true
                                         THEN TEXT-001
                                         ELSE TEXT-002 ) ).
    "add skipped pernr's
    IF skipped_pernrs IS NOT INITIAL.
      message_handler->if_hrpay00_pal_services~add_table(
      EXPORTING
        i_parent_node_key = root_node
        it_append_table   = skipped_pernrs
        is_layout         = layout
        i_node_txt        = COND string( WHEN simu EQ abap_true
                                           THEN TEXT-003
                                           ELSE TEXT-004 ) ).
    ENDIF.

    "add message table
    message_handler->if_hrpay00_pal_services~add_messages( EXPORTING i_parent_node_key = root_node ).

    "show pals
    CALL METHOD message_handler->display_pal.

  ENDIF.

ENDMETHOD.
ENDCLASS.
