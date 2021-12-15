CLASS zmhp_cl_mig_assignment_man DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA pernr TYPE rsdsselopt_t .
    DATA begda TYPE begda .
    DATA endda TYPE endda .
    DATA cofu TYPE boolean .
    DATA cogl TYPE boolean .
    DATA molga TYPE rsdsselopt_t .
    DATA p0001 TYPE p0001_tab .
    DATA assignment_man_structure TYPE /iwbep/t_mgw_name_value_pair .
    CONSTANTS assignment_super TYPE string VALUE 'AssignmentSupervisor' ##NO_TEXT.
    CONSTANTS assignment TYPE string VALUE 'ASSIGNMENT_' ##NO_TEXT.
    CONSTANTS assignment_man TYPE string VALUE '_ASSIGNMENT_MANAGER' ##NO_TEXT.
    DATA p0000 TYPE p0000_tab .
    CONSTANTS wt TYPE string VALUE '_WT' ##NO_TEXT.
    CONSTANTS assign TYPE string VALUE 'ASN_' ##NO_TEXT.

    METHODS proceed_cofu_assign_manager
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
        !worker     TYPE REF TO zmhp_cl_mig_worker
      RETURNING
        VALUE(data) TYPE string .
    METHODS proceed_cogl_assign_manager
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
        !worker     TYPE REF TO zmhp_cl_mig_worker
        !workterms  TYPE REF TO zmhp_cl_mig_work_terms
      RETURNING
        VALUE(data) TYPE string .
    METHODS proceed_cogu_assign_manager
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
        !worker     TYPE REF TO zmhp_cl_mig_worker
        !workterms  TYPE REF TO zmhp_cl_mig_work_terms
      RETURNING
        VALUE(data) TYPE string .
    METHODS constructor
      IMPORTING
        !pernr TYPE rsdsselopt_t
        !begda TYPE begda
        !endda TYPE endda
        !cofu  TYPE boolean
        !cogl  TYPE boolean
        !molga TYPE rsdsselopt_t
        !cogu  TYPE boolean .
    METHODS create_metadata
      RETURNING
        VALUE(metadata) TYPE string .
    METHODS get_cofu_managers_of_emp
      IMPORTING
        !periods TYPE hrperiods_tab
        !pernr   TYPE pernr_d .
    METHODS map_cofu_data
      IMPORTING
        !periods    TYPE hrperiods_tab
        !pernr      TYPE pernr_d
      RETURNING
        VALUE(data) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF leader_objid_con ,
        pernr_emp TYPE pernr_d,
        pernr_man TYPE pernr_d,
        begda     TYPE begda,
        endda     TYPE endda,
      END OF leader_objid_con .

    DATA mapping_values_massn TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapping .
    DATA mapping_fields_massn TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapp_fi .
    DATA:
      leader_emp TYPE TABLE OF leader_objid_con WITH DEFAULT KEY .
    DATA leader_per TYPE /iwbep/t_mgw_name_value_pair .
    DATA vp_src_id TYPE /iwbep/t_mgw_name_value_pair .
    DATA pernr_history TYPE rsdsselopt_t .
    DATA cogu TYPE boolean .

    METHODS create_hire_entry
      RETURNING
        VALUE(data) TYPE string .
    METHODS map_mig_values
      IMPORTING
        !p0000 TYPE p0000
      EXPORTING
        !massn TYPE zmhp_dd_value .
    METHODS get_manager
      IMPORTING
        !pernr               TYPE pernr_d
        !begda               TYPE begda
        !endda               TYPE endda
      RETURNING
        VALUE(manager_pernr) TYPE pernr_d .
    METHODS get_managers_of_emp .
    METHODS get_cofu_data .
    METHODS get_cogl_data .
    METHODS get_mapping_fields .
    METHODS get_mapping_values .
    METHODS map_cogl_data
      RETURNING
        VALUE(data) TYPE string .
    METHODS map_cogu_data
      RETURNING
        VALUE(data) TYPE string .
ENDCLASS.



CLASS ZMHP_CL_MIG_ASSIGNMENT_MAN IMPLEMENTATION.


  METHOD constructor.

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cogl EQ abap_true OR
       cogu EQ abap_true.
      assignment_man_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                          ( name = 2  value = assignment_super )
                                          ( name = 3  value = 'SourceSystemId' )
                                          ( name = 4  value = 'SourceSystemOwner' )
                                          ( name = 5  value = 'AssignmentId(SourceSystemId)' )
                                          ( name = 6  value = 'EffectiveStartDate' )
                                          ( name = 7  value = 'EffectiveEndDate' )
                                          ( name = 8  value = 'ManagerAssignmentId(SourceSystemId)' )
                                          ( name = 13 value = 'ManagerId(SourceSystemId)' )
                                          ( name = 9  value = 'ManagerType' )
                                          ( name = 10 value = 'PersonId(SourceSystemId)' )
                                          ( name = 11 value = 'PrimaryFlag' )
                                          ( name = 12 value = 'ActionCode' ) ).

    ELSEIF cofu EQ abap_true.
      assignment_man_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                          ( name = 2  value = assignment_super )
                                          ( name = 3  value = 'SourceSystemId' )
                                          ( name = 4  value = 'SourceSystemOwner' )
                                          ( name = 5  value = 'AssignmentId(SourceSystemId)' )
                                          ( name = 6  value = 'EffectiveStartDate' )
                                          ( name = 7  value = 'EffectiveEndDate' )
                                          ( name = 8  value = 'ManagerAssignmentId(SourceSystemId)' )
                                          ( name = 13 value = 'ManagerId(SourceSystemId)' )
                                          ( name = 9  value = 'ManagerType' )
                                          ( name = 10 value = 'PersonId(SourceSystemId)' )
                                          ( name = 11 value = 'PrimaryFlag' )
                                          ( name = 12 value = 'ActionCode' ) ).
    ENDIF.
  ENDMETHOD.


  METHOD create_hire_entry.

    DATA: plvar           TYPE plvar,
          internal_number TYPE string,
          leader_assign   TYPE string,
          leader_pernr    TYPE string.

    "get history assignment
    DATA(p0001_history) = p0001.

    DELETE p0001_history WHERE begda GT sy-datum.
    DELETE p0001_history WHERE begda LE sy-datum AND
                               endda GE sy-datum.

    "delete history from main table
    LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001_historxy>).
      DELETE TABLE p0001 FROM <p0001_historxy>.
    ENDLOOP.

    "only first assignment relevant
    SORT p0001_history BY pernr begda ASCENDING.
    DELETE ADJACENT DUPLICATES FROM p0001_history COMPARING pernr.

    "get actual assignment
    DATA(p0001_actual) = p0001.
    DELETE p0001_actual WHERE endda LT sy-datum OR
                              begda GT sy-datum.

    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

    "get active plvar
    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001>).

      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "get actual assignment and set enddate of history entry to actual begda - 1
      LOOP AT p0001_actual ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr.
        <p0001>-endda = <p0001_actual>-begda - 1.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination ignore entry
*    CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it´s provided in WorkRelationship

      map_mig_values( EXPORTING p0000 = <p0000>
                      IMPORTING massn = DATA(massn) ).

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-endda ).
      CONCATENATE assign <p0001>-pernr assignment_man INTO DATA(src_id).
      CONCATENATE assign <p0001>-pernr INTO DATA(asn_id).

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      "get boss
      LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <p0001>-pernr AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
        leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                                THEN assign && CONV string( <leader>-pernr_man )
                                ELSE '' ).
        READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
        leader_pernr = leader_per_e-value.
        EXIT.
      ENDLOOP.

      IF leader_assign IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
        CONCATENATE zmhp_cl_mig_utils=>merge
                    assignment_super
                    src_id
                    sys_id
                    asn_id
                    begda_tmp
                    endda_tmp
                    leader_assign
                    leader_pernr
                    'LINE_MANAGER'
                    src_sys_id
                    zmhp_cl_mig_utils=>yes
                    massn
        INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

        CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      ENDIF.

      CLEAR: leader_pernr, leader_assign.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE assignment_man_structure LINES DATA(length).

    LOOP AT assignment_man_structure ASSIGNING FIELD-SYMBOL(<assign_man_struc>).

      "set METADATA title
      CASE <assign_man_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <assign_man_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_cofu_data.
    "Get IT0000
    SELECT pernr,
           begda,
           endda,
           massn INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

    "Get IT0001
    SELECT pernr,
           begda,
           endda,
           orgeh INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDMETHOD.


  METHOD get_cofu_managers_of_emp.

    DATA: plvar         TYPE plvar,
          manager_table TYPE objec_t,
          pernr_emp     TYPE TABLE OF leader_objid_con WITH DEFAULT KEY,
          leader_pernr  TYPE rsdsselopt_t.

    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    pernr_emp = VALUE #( FOR <period> IN periods ( pernr_emp = pernr
                                                   begda     = <period>-begda
                                                   endda     = <period>-endda ) ).

    SORT pernr_emp BY pernr_emp begda endda.
    DELETE ADJACENT DUPLICATES FROM pernr_emp.

    LOOP AT pernr_emp ASSIGNING FIELD-SYMBOL(<pernr_emp>).

      "check pernr already was readed in date range
      LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <pernr_emp>-pernr_emp AND
                                                                begda     LE <pernr_emp>-endda     AND
                                                                endda     GE <pernr_emp>-begda.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc NE 0.

      "get manager
      DATA(manager_pernr) = get_manager( pernr = <pernr_emp>-pernr_emp
                                         begda = <pernr_emp>-begda
                                         endda = <pernr_emp>-endda ).

      APPEND VALUE #( pernr_emp = <pernr_emp>-pernr_emp
                      begda = <pernr_emp>-begda
                      endda = <pernr_emp>-endda
                      pernr_man = manager_pernr ) TO leader_emp.

      APPEND VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = manager_pernr ) TO leader_pernr.
    ENDLOOP.

    CHECK leader_pernr IS NOT INITIAL.  "JMB20210911 I

    "get worker ID of leader
    NEW zmhp_cl_mig_worker( molga = molga
                            begda = begda
                            endda = endda
                            cogl  = cogl
                            cofu  = cofu
                            cogu  = cogu
                            pernr = leader_pernr )->proceed_cogl_worker( IMPORTING vp_src_id = leader_per ).

  ENDMETHOD.


  METHOD get_cogl_data.
    "Get IT0000
    SELECT pernr,
           begda,
           endda,
           massn INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

    "Get IT0001
    SELECT pernr,
           begda,
           endda,
           orgeh INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDMETHOD.


  METHOD get_manager.

    DATA: lt_leader_obj   TYPE hrobject_t,
          lv_sobid        TYPE sobid,
          lv_active_plvar TYPE plvar,
          lt_p1001        TYPE STANDARD TABLE OF p1001,
          ls_p1001        TYPE p1001,
          lv_pernr        TYPE pernr_d.

    CLEAR manager_pernr.

* get active plan variant
    CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
      IMPORTING
        act_plvar = lv_active_plvar.

* get leading positions
    lv_sobid = pernr.
    CALL FUNCTION 'RH_GET_LEADING_POSITION'
      EXPORTING
        plvar             = lv_active_plvar
        otype             = cl_hrpa_tclas=>otype_employee
        sobid             = lv_sobid
        date              = begda
        auth              = abap_false
        consider_vac_pos  = abap_true
      TABLES
        leading_pos       = lt_leader_obj
      EXCEPTIONS
        no_lead_pos_found = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* get holders of leading positions
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        with_stru_auth   = abap_true
        subty            = 'A008'
        begda            = begda
        endda            = endda
      TABLES
        objects          = lt_leader_obj
        i1001            = lt_p1001
      EXCEPTIONS
        nothing_found    = 1
        wrong_condition  = 2
        wrong_parameters = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RETURN.
    ELSE.
      READ TABLE lt_p1001 INTO DATA(p1001) INDEX 1.
      manager_pernr = p1001-sobid.
    ENDIF.

    CHECK manager_pernr IS NOT INITIAL.

    "check inactive employment
    CHECK zmhp_cl_mig_utils=>check_active_emp_status( pernr = manager_pernr ) EQ abap_false.

    CLEAR: manager_pernr.
  ENDMETHOD.


  METHOD get_managers_of_emp.

    DATA: plvar         TYPE plvar,
          manager_table TYPE objec_t,
          pernr_emp     TYPE TABLE OF leader_objid_con WITH DEFAULT KEY,
          leader_pernr  TYPE rsdsselopt_t.

    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    pernr_emp = VALUE #( FOR <p0001> IN p0001 ( pernr_emp = <p0001>-pernr
                                                begda     = <p0001>-begda
                                                endda     = <p0001>-endda ) ).

    SORT pernr_emp BY pernr_emp begda endda.
    DELETE ADJACENT DUPLICATES FROM pernr_emp.

    LOOP AT pernr_emp ASSIGNING FIELD-SYMBOL(<pernr_emp>).

      "check pernr already was readed in date range
      LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <pernr_emp>-pernr_emp AND
                                                                begda     LE <pernr_emp>-endda     AND
                                                                endda     GE <pernr_emp>-begda.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc NE 0.

      "get manager
      DATA(manager_pernr) = get_manager( pernr = <pernr_emp>-pernr_emp
                                         begda = <pernr_emp>-begda
                                         endda = <pernr_emp>-endda ).

      APPEND VALUE #( pernr_emp = <pernr_emp>-pernr_emp
                      begda = <pernr_emp>-begda
                      endda = <pernr_emp>-endda
                      pernr_man = manager_pernr ) TO leader_emp.

      APPEND VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = manager_pernr ) TO leader_pernr.
    ENDLOOP.

    CHECK leader_pernr IS NOT INITIAL.  "JMB20210911 I

    "get worker ID of leader
    NEW zmhp_cl_mig_worker( molga = molga
                            begda = begda
                            endda = endda
                            cogl  = cogl
                            cofu  = cofu
                            cogu  = cogu
                            pernr = leader_pernr )->proceed_cogl_worker( IMPORTING vp_src_id = leader_per ).

  ENDMETHOD.


  METHOD get_mapping_fields.

    "get mapping fields for actioncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_massn ).
  ENDMETHOD.


  METHOD get_mapping_values.

    "get mapping values for actioncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_massn ).
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: leader_assign TYPE string,
          leader_pernr  TYPE string.

    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
      "get boss
      LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ pernr AND
                                                                begda     LE <period>-endda AND
                                                                endda     GE <period>-begda.
        leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                                THEN assign && CONV string( <leader>-pernr_man )
                                ELSE '' ).
        READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
        leader_pernr = leader_per_e-value.
        EXIT.
      ENDLOOP.

      CHECK leader_assign IS NOT INITIAL.

      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination ignore entry
*      CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it´s provided in WorkRelationship

      map_mig_values( EXPORTING p0000 = <p0000>
                      IMPORTING massn = DATA(massn) ).

      "check hire entry for employee
      IF pernr  IN pernr_history AND
          pernr_history IS NOT INITIAL.
        "actioncode for these entries will be MANAGER_CHANGE
        massn = 'MANAGER_CHANGE'.
      ELSE.
        massn = COND #( WHEN massn NE 'HIRE'
                        THEN 'HIRE'
                        ELSE massn ).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = pernr ) TO pernr_history.
      ENDIF.

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <period>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <period>-endda ).
      CONCATENATE zmhp_cl_mig_utils=>assign pernr assignment_man INTO DATA(src_id).
      CONCATENATE zmhp_cl_mig_utils=>assign pernr INTO DATA(asn_id).

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = pernr
                                                        begda = <period>-begda
                                                        endda = <period>-endda
                                                        vp_src_id = vp_src_id ).

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment_super
                  src_id
                  sys_id
                  asn_id
                  begda_tmp
                  endda_tmp
                  leader_assign
                  leader_pernr
                  'LINE_MANAGER'
                  src_sys_id
                  zmhp_cl_mig_utils=>yes
                  massn
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

      CLEAR: leader_pernr, leader_assign.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_cogl_data.

    DATA: leader_assign TYPE string,
          leader_pernr  TYPE string.

    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    DELETE leader_emp WHERE pernr_man EQ '00021945'.  "JMB20210714 I - Don´t pass CEO in CoGl as manager

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
      "get boss
      LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <p0001>-pernr AND
                                                                begda     LE <p0001>-endda AND
                                                                endda     GE <p0001>-begda.
        leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                                THEN assign && CONV string( <leader>-pernr_man )
                                ELSE '' ).
        READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
        leader_pernr = leader_per_e-value.
        EXIT.
      ENDLOOP.

      CHECK leader_assign IS NOT INITIAL.

      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination ignore entry
      CHECK <p0000>-massn NOT IN massn_term.

      "check for terminations right after actual record
      DATA(datum) = CONV datum( <p0001>-endda + 1 ).
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                               endda GE datum         AND
                                                               pernr EQ <p0001>-pernr AND
                                                               massn IN massn_term.
        <p0001>-endda = <p0000_term>-endda.
      ENDLOOP.

      map_mig_values( EXPORTING p0000 = <p0000>
                      IMPORTING massn = DATA(massn) ).

      "check hire entry for employee
      IF <p0001>-pernr  IN pernr_history AND
          pernr_history IS NOT INITIAL.
        "actioncode for these entries will be MANAGER_CHANGE
        massn = 'MANAGER_CHANGE'.
      ELSE.
        massn = COND #( WHEN massn NE 'HIRE'
                        THEN 'HIRE'
                        ELSE massn ).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
      ENDIF.

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-endda ).
      CONCATENATE zmhp_cl_mig_utils=>assign <p0001>-pernr assignment_man INTO DATA(src_id).
      CONCATENATE zmhp_cl_mig_utils=>assign <p0001>-pernr INTO DATA(asn_id).

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment_super
                  src_id
                  sys_id
                  asn_id
                  begda_tmp
                  endda_tmp
                  leader_assign
                  leader_pernr
                  'LINE_MANAGER'
                  src_sys_id
                  zmhp_cl_mig_utils=>yes
                  massn
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

      CLEAR: leader_pernr, leader_assign.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_cogu_data.

    DATA: leader_assign TYPE string,
          leader_pernr  TYPE string.

    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
      "get boss
      LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <p0001>-pernr AND
                                                                begda     LE <p0001>-endda AND
                                                                endda     GE <p0001>-begda.
        leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                                THEN assign && CONV string( <leader>-pernr_man )
                                ELSE '' ).
        READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
        leader_pernr = leader_per_e-value.
        EXIT.
      ENDLOOP.

      CHECK leader_assign IS NOT INITIAL.

      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination ignore entry
      CHECK <p0000>-massn NOT IN massn_term.

      "check for terminations right after actual record
      DATA(datum) = CONV datum( <p0001>-endda + 1 ).
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                               endda GE datum         AND
                                                               pernr EQ <p0001>-pernr AND
                                                               massn IN massn_term.
        <p0001>-endda = <p0000_term>-endda.
      ENDLOOP.

      map_mig_values( EXPORTING p0000 = <p0000>
                      IMPORTING massn = DATA(massn) ).

      "check hire entry for employee
      IF <p0001>-pernr  IN pernr_history AND
          pernr_history IS NOT INITIAL.
        "actioncode for these entries will be MANAGER_CHANGE
        massn = 'MANAGER_CHANGE'.
      ELSE.
        massn = COND #( WHEN massn NE 'HIRE'
                        THEN 'HIRE'
                        ELSE massn ).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
      ENDIF.

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-endda ).
      CONCATENATE zmhp_cl_mig_utils=>assign <p0001>-pernr assignment_man INTO DATA(src_id).
      CONCATENATE zmhp_cl_mig_utils=>assign <p0001>-pernr INTO DATA(asn_id).

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment_super
                  src_id
                  sys_id
                  asn_id
                  begda_tmp
                  endda_tmp
                  leader_assign
                  leader_pernr
                  'LINE_MANAGER'
                  src_sys_id
                  zmhp_cl_mig_utils=>yes
                  massn
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

      CLEAR: leader_pernr, leader_assign.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_mig_values.
    DATA: value_tmp TYPE zmhp_dd_value.

    "Process MASSN mapping
    value_tmp = CONV #( p0000-massn ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = zmhp_cl_mig_utils=>massn
        field_oracle   = zmhp_cl_mig_utils=>actioncode
        mapping_fields = CONV #( mapping_fields_massn )
        mapping_values = CONV #( mapping_values_massn )
      CHANGING
        value          = value_tmp ).

    massn = value_tmp.
  ENDMETHOD.


  METHOD proceed_cofu_assign_manager.
    me->vp_src_id = vp_src_id.
    p0000         = worker->p0000.
    get_mapping_fields( ).
    get_mapping_values( ).
  ENDMETHOD.


  METHOD proceed_cogl_assign_manager.
    me->vp_src_id = vp_src_id.

    get_cogl_data( ).
    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                           CHANGING p0001 = p0001 ).
*  DATA(hire_data) = create_hire_entry( ).  "JMB20210427 D - Not needed due to only actual assignment is necessary

    zmhp_cl_mig_utils=>check_assign_supervisor( CHANGING p0001 = p0001 ).

    "get only actual and future managers
    DELETE p0001 WHERE endda LT sy-datum.

    get_managers_of_emp( ).
    get_mapping_fields( ).
    get_mapping_values( ).

    data = map_cogl_data( ).

*  CONCATENATE hire_data data INTO data.  "JMB20210427 D
  ENDMETHOD.


  METHOD proceed_cogu_assign_manager.
    me->vp_src_id = vp_src_id.

    get_cogl_data( ).
    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                           CHANGING p0001 = p0001 ).

    zmhp_cl_mig_utils=>check_assign_supervisor( CHANGING p0001 = p0001 ).

    "get only actual and future managers
    DELETE p0001 WHERE endda LT sy-datum.

    get_managers_of_emp( ).
    get_mapping_fields( ).
    get_mapping_values( ).

    data = map_cogu_data( ).

*  CONCATENATE hire_data data INTO data.  "JMB20210427 D
  ENDMETHOD.
ENDCLASS.
