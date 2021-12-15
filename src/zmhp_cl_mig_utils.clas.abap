CLASS zmhp_cl_mig_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      zmhp_tt_int_mapping TYPE TABLE OF zmhp_int_mapping .
    TYPES:
      zmhp_tt_int_mapp_fi TYPE TABLE OF zmhp_int_mapp_fi .

    CONSTANTS sap TYPE string VALUE 'SAP_' ##NO_TEXT.
    CONSTANTS separator TYPE char1 VALUE '|' ##NO_TEXT.
    CONSTANTS merge TYPE string VALUE 'MERGE' ##NO_TEXT.
    CONSTANTS metadata TYPE string VALUE 'METADATA' ##NO_TEXT.
    CONSTANTS it0002 TYPE infty VALUE '0002' ##NO_TEXT.
    CONSTANTS it0006 TYPE infty VALUE '0006' ##NO_TEXT.
    CONSTANTS it0001 TYPE infty VALUE '0001' ##NO_TEXT.
    CONSTANTS it0021 TYPE infty VALUE '0021' ##NO_TEXT.
    CONSTANTS it0105 TYPE infty VALUE '0105' ##NO_TEXT.
    CONSTANTS it0105_0010 TYPE subty VALUE '0010' ##NO_TEXT.
    CONSTANTS it0105_9002 TYPE subty VALUE '9002' ##NO_TEXT.
    CONSTANTS it0105_9004 TYPE subty VALUE '9004' ##NO_TEXT.
    CONSTANTS it0105_9005 TYPE subty VALUE '9005' ##NO_TEXT.
    CONSTANTS it0105_9003 TYPE subty VALUE '9003' ##NO_TEXT.
    CONSTANTS it0105_9901 TYPE subty VALUE '9901' ##NO_TEXT.
    CONSTANTS it0105_9906 TYPE subty VALUE '9906' ##NO_TEXT.
    CONSTANTS it0105_9905 TYPE subty VALUE '9905' ##NO_TEXT.
    CONSTANTS it0105_9902 TYPE subty VALUE '9902' ##NO_TEXT.
    CONSTANTS it0185 TYPE infty VALUE '0185' ##NO_TEXT.
    CONSTANTS it0105_9998 TYPE subty VALUE '9998' ##NO_TEXT.
    CONSTANTS it0105_9900 TYPE subty VALUE '9900' ##NO_TEXT.
    CONSTANTS it0050 TYPE infty VALUE '0050' ##NO_TEXT.
    CONSTANTS it0701 TYPE infty VALUE '0701' ##NO_TEXT.
    CONSTANTS it0000 TYPE infty VALUE '0000' ##NO_TEXT.
    CONSTANTS yes TYPE string VALUE 'Y' ##NO_TEXT.
    CONSTANTS no TYPE string VALUE 'N' ##NO_TEXT.
    CONSTANTS persk_assign_type TYPE zmhp_dd_field VALUE 'PERSK_ASSIGN_TYPE' ##NO_TEXT.
    CONSTANTS persk_assign_category TYPE zmhp_dd_field VALUE 'PERSK_ASSIGN_CATEGORY' ##NO_TEXT.
    CONSTANTS persk_assign_person_type TYPE zmhp_dd_field VALUE 'PERSK_ASSIGN_PERSON_TYPE' ##NO_TEXT.
    CONSTANTS persk_person_type TYPE zmhp_dd_field VALUE 'PERSK_PERSON_TYPE' ##NO_TEXT.
    CONSTANTS persk_system_person TYPE zmhp_dd_field VALUE 'PERSK_SYSTEM_PERSON' ##NO_TEXT.
    CONSTANTS persk_worker_type TYPE zmhp_dd_field VALUE 'PERSK_WORKER_TYPE' ##NO_TEXT.
    CONSTANTS massn TYPE zmhp_dd_field VALUE 'MASSN' ##NO_TEXT.
    CONSTANTS actioncode TYPE zmhp_dd_field VALUE 'ACTIONCODE' ##NO_TEXT.
    CONSTANTS legislationcode TYPE zmhp_dd_field VALUE 'LEGISLATIONCODE' ##NO_TEXT.
    CONSTANTS workertype TYPE zmhp_dd_field VALUE 'WORKERTYPE' ##NO_TEXT.
    CONSTANTS persg TYPE zmhp_dd_field VALUE 'PERSG' ##NO_TEXT.
    CONSTANTS massg TYPE zmhp_dd_field VALUE 'MASSG' ##NO_TEXT.
    CONSTANTS reasoncode TYPE zmhp_dd_field VALUE 'REASONCODE' ##NO_TEXT.
    CONSTANTS btrtl TYPE zmhp_dd_field VALUE 'BTRTL' ##NO_TEXT.
    CONSTANTS locationcode TYPE zmhp_dd_field VALUE 'LOCATIONCODE' ##NO_TEXT.
    CONSTANTS werks TYPE zmhp_dd_field VALUE 'WERKS' ##NO_TEXT.
    CONSTANTS legalemployername TYPE zmhp_dd_field VALUE 'LEGALEMPLOYERNAME' ##NO_TEXT.
    CONSTANTS bukrs TYPE zmhp_dd_field VALUE 'BUKRS' ##NO_TEXT.
    CONSTANTS businessunitshortcode TYPE zmhp_dd_field VALUE 'BUSINESSUNITSHORTCODE' ##NO_TEXT.
    CONSTANTS default_business_unit TYPE string VALUE 'Data Migration BU' ##NO_TEXT.
    CONSTANTS departmentname TYPE zmhp_dd_field VALUE 'DEPARTMENTNAME' ##NO_TEXT.
    CONSTANTS orgeh TYPE zmhp_dd_field VALUE 'ORGEH' ##NO_TEXT.
    CONSTANTS hire TYPE massn VALUE '01' ##NO_TEXT.
    CONSTANTS oracle_hd TYPE datum VALUE '47121231' ##NO_TEXT.
    CONSTANTS it0105_0001 TYPE subty VALUE '0001' ##NO_TEXT.
    CONSTANTS assign TYPE string VALUE 'ASN_' ##NO_TEXT.
    CONSTANTS it0105_9001 TYPE subty VALUE '9001' ##NO_TEXT.
    CONSTANTS contact_type TYPE zmhp_dd_field VALUE 'CONTACTTYPE' ##NO_TEXT.
    CONSTANTS it0004 TYPE infty VALUE '0004' ##NO_TEXT.
    CONSTANTS disability_category TYPE zmhp_dd_field VALUE 'DISABILITYCATEGORY' ##NO_TEXT.
    CONSTANTS sbgru TYPE zmhp_dd_field VALUE 'SBGRU' ##NO_TEXT.
    CONSTANTS componentcode TYPE zmhp_dd_field VALUE 'COMPONENTCODE' ##NO_TEXT.
    CONSTANTS fasex TYPE zmhp_dd_field VALUE 'FASEX' ##NO_TEXT.

    CLASS-METHODS get_hr_periods
      IMPORTING
        !table      TYPE table
      CHANGING
        !hr_periods TYPE hrperiods_tab .
    CLASS-METHODS summarize_past
      CHANGING
        !hr_periods TYPE hrperiods_tab .
    CLASS-METHODS check_active_emp_status
      IMPORTING
        !date         TYPE begda DEFAULT sy-datlo
        !pernr        TYPE pernr_d
      RETURNING
        VALUE(status) TYPE boolean .
    CLASS-METHODS check_assign_supervisor
      IMPORTING
        !cofu          TYPE boolean OPTIONAL
        !all_periods   TYPE boolean OPTIONAL
      EXPORTING
        !manager_pernr TYPE rsdsselopt_t
      CHANGING
        !p0001         TYPE p0001_tab .
    CLASS-METHODS check_assign_supervisor_v2
      IMPORTING
        !objects                  TYPE objec_t
      RETURNING
        VALUE(manager_assignment) TYPE zmhp_tt_position_manager_data .
    CLASS-METHODS get_legislation_codes
      IMPORTING
        !bukrs           TYPE rsdsselopt_t
      RETURNING
        VALUE(land1_map) TYPE /iwbep/t_mgw_name_value_pair .
    CLASS-METHODS get_mapping_fields
      IMPORTING
        !molga                TYPE rsdsselopt_t
        !infty                TYPE infty
        !sap_field            TYPE zmhp_dd_field
        !oracle_field         TYPE zmhp_dd_field
        !export               TYPE zmhp_dd_export
      EXPORTING
        VALUE(mapping_fields) TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapp_fi .
    CLASS-METHODS get_mapping_values
      IMPORTING
        !molga                TYPE rsdsselopt_t
        !infty                TYPE infty
        !sap_field            TYPE zmhp_dd_field
        !oracle_field         TYPE zmhp_dd_field
        !export               TYPE zmhp_dd_export
      EXPORTING
        VALUE(mapping_values) TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapping .
    CLASS-METHODS get_mngrs_to_orgunit
      IMPORTING
        !orgeh            TYPE rsdsselopt_t
        !begda            TYPE begda
        !endda            TYPE endda
      EXPORTING
        !managers         TYPE hrp1001_t
        !leader_positions TYPE hrp1001_t .
    CLASS-METHODS is_manager
      IMPORTING
        !p0001                   TYPE p0001
        !plvar                   TYPE plvar OPTIONAL
      RETURNING
        VALUE(is_manager_oracle) TYPE char1 .
    CLASS-METHODS summarize_it0000_cofu
      CHANGING
        !p0000 TYPE p0000_tab .
    CLASS-METHODS summarize_it0000_cogl
      CHANGING
        !p0000 TYPE p0000_tab .
    CLASS-METHODS summarize_it0002
      CHANGING
        !p0002 TYPE p0002_tab .
    CLASS-METHODS update_begin_date
      IMPORTING
        !p0000       TYPE p0000_tab
        !create_hire TYPE boolean OPTIONAL
      CHANGING
        !p0001       TYPE p0001_tab OPTIONAL
        !p0002       TYPE p0002_tab OPTIONAL .
    CLASS-METHODS get_src_id
      IMPORTING
        !vp_src_id    TYPE /iwbep/t_mgw_name_value_pair
        !pernr        TYPE pernr_d
        !begda        TYPE begda
        !endda        TYPE endda
      RETURNING
        VALUE(src_id) TYPE string .
    CLASS-METHODS convert_date
      IMPORTING
        !datum            TYPE datum
      RETURNING
        VALUE(form_datum) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZMHP_CL_MIG_UTILS IMPLEMENTATION.


  METHOD check_active_emp_status.

    DATA: p0000  TYPE p0000_tab,
          active TYPE stat2 VALUE '3'.

    status = abap_false.

    NEW zmhp_cl_int_it_operation( )->read_paxxxx( EXPORTING pernr = pernr
                                                            infty = CONV #( zmhp_cl_int_constants=>it0000 )
                                                            begda = date
                                                            endda = date
                                                            simu  = abap_false
                                                  IMPORTING record_tab = p0000 ).

    CHECK p0000 IS NOT INITIAL.
    READ TABLE p0000 ASSIGNING FIELD-SYMBOL(<p0000>) INDEX 1.

    CHECK <p0000> IS ASSIGNED.
    CHECK <p0000>-stat2 EQ active.
    status      = abap_true.

  ENDMETHOD.


  METHOD check_assign_supervisor.

    DATA: plvar             TYPE plvar,
          leader_type       TYPE otype,
          leader_id         TYPE realo,
          datum             TYPE datum,
          manager_info      TYPE objec_t,
          begin_dates       TYPE rsdsselopt_t,
          hr_periods        TYPE hrperiods_tab,
          hr_periods_search TYPE hrperiods_tab,
          hr_periods_all    TYPE hrperiods_tab,
          p0001_tmp         TYPE p0001_tab,
          count             TYPE i VALUE 0,
          index             TYPE i VALUE 0.

    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
      DATA(begda) = <p0001>-begda.
      DATA(endda) = <p0001>-endda.
      DATA(all_collect) = abap_false.

      IF <p0001>-orgeh IS INITIAL. "JMB20210811 I
        APPEND <p0001> TO p0001_tmp.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( begda = <p0001>-begda
                      endda = <p0001>-endda ) TO hr_periods.

      WHILE all_collect EQ abap_false.
        CALL FUNCTION 'HRCM_ORGUNIT_MANAGER_GET'
          EXPORTING
            plvar              = plvar
            otype              = 'O'
            objid              = <p0001>-orgeh
            begda              = begda
            endda              = endda
            path_id            = 'MAN_O'
          TABLES
            manager_info_table = manager_info
          EXCEPTIONS
            nothing_found      = 1
            path_error         = 2
            root_error         = 3.

        DELETE manager_info WHERE otype NE 'P'.

        LOOP AT manager_info ASSIGNING FIELD-SYMBOL(<mngr_info>).
          APPEND VALUE #( begda = <mngr_info>-begda
                          endda = <mngr_info>-endda ) TO hr_periods.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <mngr_info>-begda ) TO begin_dates.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <mngr_info>-objid ) TO manager_pernr.
        ENDLOOP.

**JMB20210802 start insert - in case no manager was found pass period
*
        IF manager_info IS INITIAL.

**JMB20211129 start insert - in case no manager was found, check assignment of OrgUnit to get time range
*
          SELECT begda, endda FROM hrp1001 INTO TABLE @DATA(o1001_orgunit) WHERE otype EQ 'O'            AND
                                                                                 objid EQ @<p0001>-orgeh AND
                                                                                 rsign EQ 'A'            AND
                                                                                 relat EQ '002'          AND
                                                                                 begda LE @endda         AND
                                                                                 endda GE @begda
                                                                           ORDER BY begda ASCENDING.

          IF o1001_orgunit IS NOT INITIAL.
            READ TABLE o1001_orgunit ASSIGNING FIELD-SYMBOL(<o1001_a002>) INDEX 1.
            IF <o1001_a002>-endda LT endda.
              endda = <o1001_a002>-endda.
            ENDIF.

            IF begda NOT IN begin_dates.
              APPEND VALUE #( sign = 'I' option = 'EQ' low = begda ) TO begin_dates.
              CLEAR: o1001_orgunit.
              CONTINUE.
            ENDIF.
          ENDIF.
*JMB20211129 insert end

          APPEND VALUE #( begda = begda
                          endda = endda ) TO hr_periods.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = begda ) TO begin_dates.
        ENDIF.
*JMB20210802 end insert

        "build periods
        CALL FUNCTION 'RHXPROVIDE_PERIODS'
          TABLES
            provide_tab = hr_periods.

        LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<hr_periods>) WHERE begda NOT IN begin_dates.
          APPEND <hr_periods> TO hr_periods_search.
          count = count + 1.
        ENDLOOP.

**JMB20210527 start insert - in case not all periods are needed
*
        IF all_periods  EQ abap_false AND
           manager_info IS INITIAL.
          DATA(hr_period) = VALUE hrperiods( begda = begda
                                             endda = endda ).
          DELETE TABLE hr_periods FROM hr_period.
        ENDIF.
*JMB20210527 insert end

        "get next period to be checked
        IF index LT count.
          index = index + 1.
          READ TABLE hr_periods_search ASSIGNING FIELD-SYMBOL(<search_period>) INDEX index.
          begda = <search_period>-begda.
          endda = <search_period>-endda.
        ELSE.
          all_collect = abap_true.
        ENDIF.

        CLEAR: manager_info. ", hr_periods.
      ENDWHILE.

      APPEND LINES OF hr_periods TO hr_periods_all.

      SORT hr_periods_all BY begda ASCENDING.

      "collect all old entries in actual IT0001 entry in one entry
      DATA(hr_periods_old) = hr_periods_all.
      DELETE hr_periods_old WHERE endda GE sy-datum.

      "check if there were older periods for COGL
      IF hr_periods_old IS NOT INITIAL AND
         cofu           IS INITIAL.
        "get earliest entry
        READ TABLE hr_periods_old ASSIGNING FIELD-SYMBOL(<hr_periods_old>) INDEX 1.

        IF sy-subrc EQ 0.
          <p0001>-begda = <hr_periods_old>-begda.
        ENDIF.

        "get latest entry
        SORT hr_periods_old BY begda DESCENDING.
        READ TABLE hr_periods_old ASSIGNING <hr_periods_old> INDEX 1.

        IF sy-subrc EQ 0.
          <p0001>-endda = <hr_periods_old>-endda.
        ENDIF.

        APPEND <p0001> TO p0001_tmp.

        "pass actual and future entries
        LOOP AT hr_periods_old ASSIGNING <hr_periods_old>.
          DELETE TABLE hr_periods_all FROM <hr_periods_old>.
        ENDLOOP.
      ENDIF.

      LOOP AT hr_periods_all ASSIGNING FIELD-SYMBOL(<hr_periods_all>).
        <p0001>-begda = <hr_periods_all>-begda.
        <p0001>-endda = <hr_periods_all>-endda.

        APPEND <p0001> TO p0001_tmp.
      ENDLOOP.

      CLEAR: manager_info, hr_periods, hr_periods_search, hr_periods_all, hr_periods_old, index, count, begin_dates.
    ENDLOOP.

    SORT manager_pernr BY low.
    DELETE ADJACENT DUPLICATES FROM manager_pernr COMPARING low.

    p0001 = p0001_tmp.
  ENDMETHOD.


  METHOD check_assign_supervisor_v2.


*  DATA: plvar             TYPE plvar,
*        leader_type       TYPE otype,
*        leader_id         TYPE realo,
*        datum             TYPE datum,
*        manager_info      TYPE objec_t,
*        begin_dates       TYPE rsdsselopt_t,
*        hr_periods        TYPE hrperiods_tab,
*        hr_periods_search TYPE hrperiods_tab,
*        hr_periods_all    TYPE hrperiods_tab,
*        p0001_tmp         TYPE p0001_tab,
*        count             TYPE i VALUE 0,
*        index             TYPE i VALUE 0.
*
*  CALL FUNCTION 'RH_GET_PLVAR'
*    IMPORTING
*      plvar = plvar.
*
*  LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
*    DATA(begda) = <object>-begda.
*    DATA(endda) = <object>-endda.
*    DATA(all_collect) = abap_false.
*
*    APPEND VALUE #( begda = <object>-begda
*                    endda = <object>-endda ) TO hr_periods.
*
*    WHILE all_collect EQ abap_false.
*      CALL FUNCTION 'HRCM_ORGUNIT_MANAGER_GET'
*        EXPORTING
*          plvar              = plvar
*          otype              = 'O'
*          objid              = <object>-objid
*          begda              = begda
*          endda              = endda
*          path_id            = 'MAN_O'
*        TABLES
*          manager_info_table = manager_info
*        EXCEPTIONS
*          nothing_found      = 1
*          path_error         = 2
*          root_error         = 3.
*
*      DELETE manager_info WHERE otype NE 'P'.
*
*      LOOP AT manager_info ASSIGNING FIELD-SYMBOL(<mngr_info>).
*        APPEND VALUE #( begda = <mngr_info>-begda
*                        endda = <mngr_info>-endda ) TO hr_periods.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = <mngr_info>-begda ) TO begin_dates.
*        APPEND INITIAL LINE TO manager_assignment  ASSIGNING FIELD-SYMBOL(<manager_assignment>).
*        <manager_assignment>-objid = <object>-objid.
*        <manager_assignment>-begda = <mngr_info>-begda.
*        <manager_assignment>-endda = <mngr_info>-endda.
*        <manager_assignment>-manager_id = <mngr_info>-objid.
*      ENDLOOP.
*
***JMB20210802 start insert - in case no manager was found pass period
**
*      IF manager_info IS INITIAL.
*        APPEND VALUE #( begda = begda
*                        endda = endda ) TO hr_periods.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = begda ) TO begin_dates.
*      ENDIF.
**JMB20210802 end insert
*
*      "build periods
*      CALL FUNCTION 'RHXPROVIDE_PERIODS'
*        TABLES
*          provide_tab = hr_periods.
*
*      LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<hr_periods>) WHERE begda NOT IN begin_dates.
*        APPEND <hr_periods> TO hr_periods_search.
*        count = count + 1.
**        DELETE TABLE hr_periods FROM <hr_periods>.
*      ENDLOOP.
*
***JMB20210527 start insert - in case not all periods are needed
**
*      IF all_periods  EQ abap_false AND
*         manager_info IS INITIAL.
*        DATA(hr_period) = VALUE hrperiods( begda = begda
*                                           endda = endda ).
*        DELETE TABLE hr_periods FROM hr_period.
*      ENDIF.
**JMB20210527 insert end
*
*      "get next period to be checked
*      IF index LT count.
*        index = index + 1.
*        READ TABLE hr_periods_search ASSIGNING FIELD-SYMBOL(<search_period>) INDEX index.
*        begda = <search_period>-begda.
*        endda = <search_period>-endda.
*      ELSE.
*        all_collect = abap_true.
*      ENDIF.
*
*      CLEAR: manager_info. ", hr_periods.
*    ENDWHILE.
*
*    APPEND LINES OF hr_periods TO hr_periods_all.
*
*    SORT hr_periods_all BY begda ASCENDING.
*
*    "collect all old entries in actual IT0001 entry in one entry
*    DATA(hr_periods_old) = hr_periods_all.
*    DELETE hr_periods_old WHERE endda GE sy-datum.
*
*    "check if there were older periods for COGL
*    IF hr_periods_old IS NOT INITIAL AND
*       cofu           IS INITIAL.
*      "get earliest entry
*      READ TABLE hr_periods_old ASSIGNING FIELD-SYMBOL(<hr_periods_old>) INDEX 1.
*
*      IF sy-subrc EQ 0.
*        <p0001>-begda = <hr_periods_old>-begda.
*      ENDIF.
*
*      "get latest entry
*      SORT hr_periods_old BY begda DESCENDING.
*      READ TABLE hr_periods_old ASSIGNING <hr_periods_old> INDEX 1.
*
*      IF sy-subrc EQ 0.
*        <p0001>-endda = <hr_periods_old>-endda.
*      ENDIF.
*
*      APPEND <p0001> TO p0001_tmp.
*
*      "pass actual and future entries
*      LOOP AT hr_periods_old ASSIGNING <hr_periods_old>.
*        DELETE TABLE hr_periods_all FROM <hr_periods_old>.
*      ENDLOOP.
*    ENDIF.
*
*    LOOP AT hr_periods_all ASSIGNING FIELD-SYMBOL(<hr_periods_all>).
*      <p0001>-begda = <hr_periods_all>-begda.
*      <p0001>-endda = <hr_periods_all>-endda.
*
*      APPEND <p0001> TO p0001_tmp.
*    ENDLOOP.
*
*    CLEAR: manager_info, hr_periods, hr_periods_search, hr_periods_all, hr_periods_old, index, count, begin_dates.
*  ENDLOOP.
*
*  SORT manager_pernr BY low.
*  DELETE ADJACENT DUPLICATES FROM manager_pernr COMPARING low.
*
*  p0001 = p0001_tmp.
  ENDMETHOD.


  METHOD convert_date.
    DATA(datum_tmp) = datum.

    "for oracle highdate is 31.12.4712
    IF datum_tmp GT oracle_hd.
      datum_tmp = oracle_hd.
    ENDIF.

    CONCATENATE datum_tmp+0(4) datum+4(2) datum+6(2) INTO form_datum SEPARATED BY '/'.
  ENDMETHOD.


  METHOD get_hr_periods.

    LOOP AT table ASSIGNING FIELD-SYMBOL(<entry>).
      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <entry> TO FIELD-SYMBOL(<begda>).
      ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <entry> TO FIELD-SYMBOL(<endda>).

      CHECK <begda> IS ASSIGNED AND
            <endda> IS ASSIGNED.

      APPEND VALUE #( begda = <begda>
                      endda = <endda> ) TO hr_periods.
    ENDLOOP.

    CALL FUNCTION 'RHXPROVIDE_PERIODS'
      TABLES
        provide_tab = hr_periods.
  ENDMETHOD.


  METHOD get_legislation_codes.
    SELECT bukrs, land1 FROM t001 INTO TABLE @DATA(land1_tmp) WHERE bukrs IN @bukrs.

    land1_map = VALUE #( FOR <land1_tmp> IN land1_tmp ( name = <land1_tmp>-bukrs value = <land1_tmp>-land1 ) ).
  ENDMETHOD.


  METHOD get_mapping_fields.

    IF molga IS NOT INITIAL.
      "get mapping values
      SELECT * FROM zmhp_int_mapp_fi INTO TABLE @mapping_fields WHERE molga        IN @molga        AND
                                                                      infty        =  @infty        AND
                                                                      field_sap    =  @sap_field    AND
                                                                      field_oracle =  @oracle_field AND
                                                                      export       =  @export.
    ENDIF.

    IF sy-subrc NE 0 OR
       molga    IS INITIAL.
      SELECT * FROM zmhp_int_mapp_fi INTO TABLE @mapping_fields WHERE molga        EQ '*'           AND
                                                                      infty        =  @infty        AND
                                                                      field_sap    =  @sap_field    AND
                                                                      field_oracle =  @oracle_field AND
                                                                      export       =  @export.
    ENDIF.

  ENDMETHOD.


  METHOD get_mapping_values.

    "get mapping values
    SELECT * FROM zmhp_int_mapping INTO TABLE @mapping_values WHERE molga        IN @molga        AND
                                                                    infty        =  @infty        AND
                                                                    field_sap    =  @sap_field    AND
                                                                    field_oracle =  @oracle_field AND
                                                                    export       =  @export.

    IF sy-subrc NE 0.
      SELECT * FROM zmhp_int_mapping INTO TABLE @mapping_values WHERE molga        EQ '*'           AND
                                                                      infty        =  @infty        AND
                                                                      field_sap    =  @sap_field    AND
                                                                      field_oracle =  @oracle_field AND
                                                                      export       =  @export.
    ENDIF.

  ENDMETHOD.


  METHOD get_mngrs_to_orgunit.

    "get leader position in orgunit
    SELECT objid, begda, endda, sobid INTO CORRESPONDING FIELDS OF TABLE @leader_positions FROM hrp1001 WHERE objid IN @orgeh AND
                                                                                                              otype EQ 'O'    AND
                                                                                                              rsign EQ 'B'    AND
                                                                                                              relat EQ '012'  AND
                                                                                                              sclas EQ 'S'    AND
                                                                                                              begda LE @endda AND
                                                                                                              endda GE @begda.

    "get all leader position
    DATA(position) = VALUE rsdsselopt_t( FOR <position> IN leader_positions ( sign = 'I' option = 'EQ' low = <position>-sobid ) ).

    "get pernr of leader position
    IF position IS NOT INITIAL.
      SELECT objid, begda, endda, sobid INTO CORRESPONDING FIELDS OF TABLE @managers FROM hrp1001 WHERE objid IN @position AND
                                                                                                        otype EQ 'S'       AND
                                                                                                        rsign EQ 'A'       AND
                                                                                                        relat EQ '008'     AND
                                                                                                        sclas EQ 'P'       AND
                                                                                                        begda LE @endda    AND
                                                                                                        endda GE @begda.
    ENDIF.

  ENDMETHOD.


  METHOD get_src_id.

    LOOP AT vp_src_id ASSIGNING FIELD-SYMBOL(<src_id>) WHERE name EQ pernr.

      "get date
      DATA(length) = strlen( <src_id>-value ).
      DATA(start)  = length - 8.
      DATA(datum)  = CONV datum( <src_id>-value+start(8) ).

      "default
      src_id = <src_id>-value.
      CHECK datum BETWEEN begda AND endda.
      src_id = <src_id>-value.
      EXIT.

    ENDLOOP.
  ENDMETHOD.


  METHOD is_manager.

    DATA: hrp1001   TYPE hrp1001_t,
          plvar_tmp TYPE plvar.

    plvar_tmp = plvar.
    IF plvar_tmp IS INITIAL.
      "get active plvar
      CALL FUNCTION 'RH_GET_PLVAR'
        IMPORTING
          plvar = plvar_tmp.
    ENDIF.

    is_manager_oracle = zmhp_cl_mig_utils=>no.

    CHECK p0001-plans IS NOT INITIAL.

    "check manager
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        plvar         = plvar_tmp
        otype         = 'S'
        objid         = p0001-plans
        subty         = 'A012'
        begda         = p0001-begda
        endda         = p0001-endda
      TABLES
        i1001         = hrp1001
      EXCEPTIONS
        nothing_found = 1.

    IF hrp1001 IS NOT INITIAL.
      is_manager_oracle = zmhp_cl_mig_utils=>yes.
    ENDIF.

  ENDMETHOD.


  METHOD summarize_it0000_cofu.
    DATA: pernr_old TYPE  rsdsselopt_t.

    DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                               ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '34' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '25' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '25' ) )
                                            "default Italy/Austria
                                            ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                          ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

    DATA(massn_hire) = SWITCH rsdsselopt_t( sy-mandt
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '06' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '08' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '15' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '16' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '17' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '60' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '62' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '70' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '91' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '92' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '97' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '98' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z1' )
                                                                                                               ( sign = 'I' option = 'EQ' low = 'Z5' )
                                                                                                               ( sign = 'I' option = 'EQ' low = 'Z8' )
                                                                                                               ( sign = 'I' option = 'EQ' low = 'Z9' )
                                                                                                               ( sign = 'I' option = 'EQ' low = 'ZA' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '12' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '90' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '91' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '90' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '91' ) )
                                            "default Italy/Austria
                                            ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                          ( sign = 'I' option = 'EQ' low = '10' ) ) ).

    "keep only terminations/hire in past
    DATA(p0000_term) = p0000.
    DATA(p0000_hire) = p0000.
    DELETE p0000_term WHERE massn NOT IN massn_term.
    DELETE p0000_hire WHERE massn NOT IN massn_hire.

    DELETE p0000_term WHERE begda GE sy-datum.
    DELETE p0000_hire WHERE begda GE sy-datum.

    CHECK p0000_term IS NOT INITIAL.

    "Get latest termination action and earliest hire
    SORT p0000_term BY pernr endda DESCENDING.
    SORT p0000_hire BY pernr begda ASCENDING.

    LOOP AT p0000_term ASSIGNING FIELD-SYMBOL(<p0000>).
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "delete all actions older than latest termination
      DELETE p0000 WHERE pernr EQ <p0000>-pernr AND
                         endda LT <p0000>-begda.

**JMB20210928 start insert - set enddate of first hire entry to begindate of last termination - 1
*
      "append earliest hire
      LOOP AT p0000_hire ASSIGNING FIELD-SYMBOL(<p0000_hire>) WHERE pernr EQ <p0000>-pernr.
        <p0000_hire>-endda = <p0000>-begda - 1.
        APPEND <p0000_hire> TO p0000.
        EXIT.
      ENDLOOP.
*JMB20210928 insert end

      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
    ENDLOOP.
    SORT p0000 BY pernr begda.
  ENDMETHOD.


  METHOD summarize_it0000_cogl.
    DATA: pernr_old TYPE  rsdsselopt_t.

    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).
    DATA(massn_hire) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01' ) ).

    "keep only terminations/hire and in past
    DATA(p0000_term) = p0000.
    DATA(p0000_hire) = p0000.
    DELETE p0000_term WHERE massn NOT IN massn_term.
    DELETE p0000_hire WHERE massn NOT IN massn_hire.

    DELETE p0000_term WHERE begda GE sy-datum.
    DELETE p0000_hire WHERE begda GE sy-datum.

    CHECK p0000_term IS NOT INITIAL.

    "Get latest termination action and earliest hire
    SORT p0000_term BY pernr endda DESCENDING.

    LOOP AT p0000_term ASSIGNING FIELD-SYMBOL(<p0000>).
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "delete all actions older than latest termination
      DELETE p0000 WHERE pernr EQ <p0000>-pernr AND
                         endda LT <p0000>-begda.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
    ENDLOOP.

    CLEAR pernr_old.

    SORT p0000_hire BY pernr begda ASCENDING.

    "append earliest hire
    LOOP AT p0000_hire ASSIGNING <p0000>.
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      APPEND <p0000> TO p0000.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
    ENDLOOP.

    SORT p0000 BY pernr begda.
  ENDMETHOD.


  METHOD summarize_it0002.
    DATA: p0002_tmp TYPE p0002_tab,
          p0002_sum TYPE p0002.
    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

      IF p0002_sum-pernr NE <p0002>-pernr.
        IF p0002_sum-pernr IS NOT INITIAL.
          APPEND p0002_sum TO p0002_tmp.
        ENDIF.
        CLEAR: p0002_sum.
        p0002_sum = <p0002>.
        CONTINUE.
      ENDIF.

      "check for lowest date and highest date
      IF p0002_sum-begda GT <p0002>-begda.
        p0002_sum-begda = <p0002>-begda.
      ENDIF.

      CHECK p0002_sum-endda LT <p0002>-endda.
      DATA(begda) = p0002_sum-begda.
      p0002_sum = <p0002>.
      p0002_sum-begda = begda.

    ENDLOOP.
    APPEND p0002_sum TO p0002_tmp.
    p0002 = p0002_tmp.
  ENDMETHOD.


  METHOD summarize_past.

    DATA: period_old TYPE hrperiods.

    "get actual entry
    LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<period>) WHERE begda LE sy-datum AND
                                                              endda GE sy-datum.
      EXIT.
    ENDLOOP.

    CHECK <period> IS ASSIGNED.

    "get past entries
    LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<period_old>) WHERE endda LT <period>-begda.
      IF ( period_old-begda IS INITIAL AND
           <period>-begda   NE <period_old>-begda ) OR
         ( period_old-begda IS NOT INITIAL AND
           <period>-begda   NE <period_old>-begda AND
           period_old-begda GT <period_old>-begda ).
        period_old-begda = <period_old>-begda.
      ENDIF.

      IF ( period_old-endda IS INITIAL AND
           <period>-endda   NE <period_old>-endda ) OR
         ( period_old-endda IS NOT INITIAL AND
           <period>-endda   NE <period_old>-endda AND
           period_old-endda LT <period_old>-endda ).
        period_old-endda = <period_old>-endda.
      ENDIF.
    ENDLOOP.

    DELETE hr_periods WHERE endda LT <period>-begda.

    CHECK period_old IS NOT INITIAL.

    APPEND period_old TO hr_periods.

    SORT hr_periods BY begda ASCENDING.

  ENDMETHOD.


  METHOD update_begin_date.
    DATA: pernr_old TYPE rsdsselopt_t,
          p0001_tmp TYPE p0001,
          p0002_tmp TYPE p0002.

    "delete all assignments older than first action
    CHECK p0000 IS NOT INITIAL.
    SORT p0001 BY pernr begda ASCENDING.
    SORT p0002 BY pernr begda ASCENDING.

    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>).
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "delete all records that are older than earliest hire entry
      DELETE p0001 WHERE pernr EQ <p0000>-pernr AND
                         endda LT <p0000>-begda.

      DELETE p0002 WHERE pernr EQ <p0000>-pernr AND
                         endda LT <p0000>-begda.

      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0000>-pernr.
        IF create_hire   EQ abap_true AND
           <p0001>-begda GT <p0000>-begda.

          <p0001>-begda = <p0000>-begda. "JMB20211030 I
**JMB20211030 start deletion - pass new date as begin date, due to each PERNR will be proceeded only once
*
*          p0001_tmp = <p0001>.
*          p0001_tmp-endda = <p0000>-endda.
*          p0001_tmp-begda = <p0000>-begda.
*          APPEND p0001_tmp TO p0001.
*          CLEAR p0001_tmp.
*JMB20211030 deletion end

        ELSE.
          <p0001>-begda = <p0000>-begda.
        ENDIF.

        EXIT.
      ENDLOOP.

      LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p0000>-pernr.
        <p0002>-begda = <p0000>-begda.
        EXIT.
      ENDLOOP.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
    ENDLOOP.
    SORT p0001 BY pernr begda.
  ENDMETHOD.
ENDCLASS.
