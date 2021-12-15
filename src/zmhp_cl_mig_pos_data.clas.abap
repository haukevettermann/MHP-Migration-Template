CLASS zmhp_cl_mig_pos_data DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA objects TYPE objec_t .
    DATA begda TYPE begda .
    DATA endda TYPE endda .
    CONSTANTS position TYPE string VALUE 'Position' ##NO_TEXT.
    CONSTANTS pos TYPE string VALUE 'POS_' ##NO_TEXT.
    DATA molga TYPE rsdsselopt_t .

    METHODS constructor
      IMPORTING
        !begda TYPE begda
        !endda TYPE endda
        !objec TYPE objec_t .
    METHODS proceed_pos_data
      RETURNING
        VALUE(data) TYPE string .
    METHODS create_metadata
      RETURNING
        VALUE(metadata) TYPE string .
    METHODS download_files
      IMPORTING
        !files TYPE /iwbep/t_mgw_name_value_pair .
    METHODS proceed_file_construction
      IMPORTING
        !metadata      TYPE string
        !data          TYPE string
      RETURNING
        VALUE(content) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA it1008 TYPE zmhp_hrp1008_t .
    DATA orgunits TYPE zmhp_hrp1000_t .
    DATA connections_to_jobs TYPE hrp1001_t .
    DATA connections_to_orgunits TYPE hrp1001_t .
    DATA positions TYPE zmhp_hrp1000_t .
    DATA jobs TYPE zmhp_hrp1000_t .
    DATA bukrs_texts TYPE t_t001 .
    DATA mapping_fields_bukrs TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapp_fi .
    DATA mapping_values_bukrs TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapping .

    METHODS map_pos_data
      RETURNING
        VALUE(data) TYPE string .
    METHODS get_pos_data .
    METHODS get_bukrs
      IMPORTING
        !orgunit_object_id  TYPE hrobjid
        !position_object_id TYPE hrobjid
      RETURNING
        VALUE(bukrs)        TYPE bukrs .
    METHODS get_orgunit_data
      IMPORTING
        !object_id               TYPE hrobjid
      EXPORTING
        VALUE(orgunit_name)      TYPE stext
        VALUE(orgunit_object_id) TYPE hrobjid .
    METHODS get_set_code
      RETURNING
        VALUE(set_code) TYPE string .
    METHODS get_job_data
      IMPORTING
        !object_id      TYPE hrobjid
      RETURNING
        VALUE(job_name) TYPE stext .
    METHODS get_effective_start_date
      IMPORTING
        !object_id                  TYPE hrobjid
      RETURNING
        VALUE(effective_start_date) TYPE begda .
    METHODS get_werks
      IMPORTING
        !orgunit_object_id TYPE hrobjid
      RETURNING
        VALUE(werks)       TYPE werks_d .
    METHODS get_bukrs_text
      IMPORTING
        !bukrs            TYPE bukrs
      RETURNING
        VALUE(bukrs_text) TYPE butxt .
    METHODS get_mapping_fields .
    METHODS get_mapping_values .
    METHODS map_mig_values
      IMPORTING
        !bukrs              TYPE bukrs
      EXPORTING
        !business_unit_name TYPE zmhp_dd_value
        !working_hours      TYPE zmhp_dd_value .
ENDCLASS.



CLASS ZMHP_CL_MIG_POS_DATA IMPLEMENTATION.


  METHOD constructor.

    me->begda = begda.
    me->endda = endda.
    me->objects = objec.

  ENDMETHOD.


  METHOD create_metadata.

    CONCATENATE zmhp_cl_mig_utils=>metadata
                position
                'SourceSystemOwner'
                'EffectiveStartDate'
                'EffectiveEndDate'
                'BusinessUnitName'
                'Name'
                'PositionCode'
                'ActionReasonCode'
                'ActiveStatus'
                'DepartmentName'
                'JobCode'
                'JobSetCode'
                'LocationCode'
                'LocationSetCode'
                'SupervisorPersonNumber'
                'FullPartTime'
                'RegularTemporary'
                'HiringStatus'
                'PositionType'
                'FTE'
                'HeadCount'
                'SecurityClearance'
                'ProbationPeriod'
                'ProbationPeriodUnitCd'
                'BargainingUnitCd'
                'CollectiveAgreementCode'
                'OverlapAllowedFlag'
                'SeasonalFlag'
                'GradeLadderName'
                'EntryGradeCode'
                'EntryGradeSetCode'
                'EntryStepName'
                'StandardWorkingHours'
                'StandardWorkingFrequency'
                'WorkingHours'
                'Frequency'
                'StartTime'
                'EndTime'
                'SourceSystemId'
                'UnionName'
                'UnionClassificationCode'
                'SeasonalStartDate'
                'SeasonalEndDate'
                'RequisitionNumber'
                'AssignmentCategory'
                'BudgetAmount'
                'BudgetAmountCurrency'
                'BudgetedPositionFlag'
                'CostCenterName'
                'DelegatePositionCode'
                'DelegatePositionBusinessUnitName'
                'FundedByExistingPositionFlag'
   INTO metadata SEPARATED BY zmhp_cl_mig_utils=>separator.

  ENDMETHOD.


  METHOD download_files.

    DATA: content   TYPE stringtab,
          filename  TYPE string,
          content_x TYPE xstring,
          zip_file  TYPE string,
          zip_tab   TYPE swxmlcont.



    LOOP AT files ASSIGNING FIELD-SYMBOL(<files>).
      APPEND <files>-value TO content.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename = <files>-name
          filetype = 'DAT'
          codepage = '4110' "UTF-8
        TABLES
          data_tab = content.
      CLEAR: content.
    ENDLOOP.



  ENDMETHOD.


  METHOD get_bukrs.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008_position>) WHERE objid = position_object_id AND otype = 'S'.
      bukrs = <it1008_position>-bukrs.
    ENDLOOP.

    IF sy-subrc <> 0.
      LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008_orgunit>) WHERE objid = orgunit_object_id AND otype = 'O'.
        bukrs = <it1008_orgunit>-bukrs.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD get_bukrs_text.

    LOOP AT bukrs_texts ASSIGNING FIELD-SYMBOL(<bukrs>) WHERE bukrs = bukrs.
      bukrs_text = <bukrs>-butxt.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_effective_start_date.

    DATA: position_begda   TYPE begda,
          connection_begda TYPE begda.

    LOOP AT positions ASSIGNING FIELD-SYMBOL(<position>) WHERE objid = object_id.
      position_begda = <position>-begda.
    ENDLOOP.

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      connection_begda = <connection_to_orgunit>-begda.
    ENDLOOP.

    IF connection_begda > position_begda.
      effective_start_date = connection_begda.
    ELSE.
      effective_start_date = position_begda.
    ENDIF.

  ENDMETHOD.


  METHOD get_job_data.

    DATA: job_object_id TYPE hrobjid.

    DELETE connections_to_jobs WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT connections_to_jobs ASSIGNING FIELD-SYMBOL(<connection_to_job>) WHERE objid = object_id.
      job_object_id = <connection_to_job>-sobid.
    ENDLOOP.

    DELETE jobs WHERE objid = job_object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT jobs ASSIGNING FIELD-SYMBOL(<job>) WHERE objid = job_object_id.
      job_name = <job>-stext.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_mapping_fields.

    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0001'
                                                     sap_field    = 'BUKRS'
                                                     oracle_field = 'BUSINESSUNITSHORTCODE'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs ).




    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0001'
                                                     sap_field    = 'BUKRS'
                                                     oracle_field = 'NORMALHOURS'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs ).

  ENDMETHOD.


  METHOD get_mapping_values.

    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0001'
                                                   sap_field    = 'BUKRS'
                                                   oracle_field = 'BUSINESSUNITSHORTCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs ).




    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                  infty        = '0001'
                                                  sap_field    = 'BUKRS'
                                                  oracle_field = 'NORMALHOURS'
                                                  export       = abap_true
                                        IMPORTING mapping_values = mapping_values_bukrs ).

  ENDMETHOD.


  METHOD get_orgunit_data.

    DELETE connections_to_orgunits WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      orgunit_object_id = <connection_to_orgunit>-sobid.
    ENDLOOP.

    DELETE orgunits WHERE objid = orgunit_object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT orgunits ASSIGNING FIELD-SYMBOL(<orgunit>) WHERE objid = orgunit_object_id.
      orgunit_name = <orgunit>-stext.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_pos_data.

    DATA system_language TYPE langu.

    system_language = sy-langu.

    "get all objid from class attribute
    DATA(objid) = VALUE rsdsselopt_t( FOR <object> IN objects ( sign = 'I' option = 'EQ' low = <object>-objid ) ).

    "get all revelant positions from IT1000, the language should be equal to the system language
    SELECT objid,
           begda,
           endda,
           langu,
           stext INTO CORRESPONDING FIELDS OF TABLE @positions FROM hrp1000 WHERE objid IN @objid AND
                                                                                  begda LE @endda AND
                                                                                  endda GE @begda AND
                                                                                  otype EQ 'S'    AND
                                                                                  plvar EQ '01'   AND
                                                                                  langu EQ @system_language.

    "get orgunits
    SELECT objid,
           plvar,
           otype,
           begda,
           endda,
           stext INTO CORRESPONDING FIELDS OF TABLE @orgunits FROM hrp1000 WHERE begda LE @endda AND
                                                                                 endda GE @begda AND
                                                                                 otype EQ 'O'    AND
                                                                                 plvar EQ '01'   AND
                                                                                 langu EQ @system_language.

    "get jobs
    SELECT objid,
           begda,
           endda,
           stext INTO CORRESPONDING FIELDS OF TABLE @jobs FROM hrp1000 WHERE begda LE @endda AND
                                                                             endda GE @begda AND
                                                                             otype EQ 'C'    AND
                                                                             plvar EQ '01'   AND
                                                                             langu EQ @system_language.

    "get connections to orgunits
    SELECT objid,
           begda,
           endda,
           sobid INTO CORRESPONDING FIELDS OF TABLE @connections_to_orgunits FROM hrp1001 WHERE objid IN @objid AND
                                                                                                begda LE @endda AND
                                                                                                endda GE @begda AND
                                                                                                otype EQ 'S'    AND
                                                                                                plvar EQ '01'   AND
                                                                                                relat EQ '003'  AND
                                                                                                sclas EQ 'O'.

    "get connections to jobs
    SELECT objid,
           begda,
           endda,
           sobid INTO CORRESPONDING FIELDS OF TABLE @connections_to_jobs FROM hrp1001 WHERE objid IN @objid AND
                                                                                            begda LE @endda AND
                                                                                            endda GE @begda AND
                                                                                            otype EQ 'S'    AND
                                                                                            plvar EQ '01'   AND
                                                                                            relat EQ '007'  AND
                                                                                            sclas EQ 'C'.

    "get IT1008 for bukrs
    SELECT objid,
           otype,
           begda,
           endda,
           bukrs INTO CORRESPONDING FIELDS OF TABLE @it1008 FROM hrp1008 WHERE "objid IN @objid AND
                                                                               begda LE @endda AND
                                                                               endda GE @begda.

    "get bukrs text
    SELECT bukrs,
           butxt INTO CORRESPONDING FIELDS OF TABLE @bukrs_texts FROM t001.

  ENDMETHOD.


  METHOD get_set_code.

    CASE sy-mandt.
      WHEN '102'.
        set_code = 'DE_SET'.
      WHEN '005'.
        set_code = 'FR_SET'.
      WHEN OTHERS.
        set_code = ''.
    ENDCASE.

  ENDMETHOD.


  METHOD get_werks.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      werks = <it1008>-werks.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_mig_values.

    DATA: value_tmp TYPE zmhp_dd_value.

    value_tmp = CONV #( bukrs ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0001'
        field_sap      = 'BUKRS'
        field_oracle   = 'BUSINESSUNITSHORTCODE'
        mapping_fields = CONV #( mapping_fields_bukrs )
        mapping_values = CONV #( mapping_values_bukrs )
     CHANGING
       value           = value_tmp ).

    business_unit_name             = value_tmp.



    value_tmp = CONV #( bukrs ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0001'
        field_sap      = 'BUKRS'
        field_oracle   = 'NORMALHOURS'
        mapping_fields = CONV #( mapping_fields_bukrs )
        mapping_values = CONV #( mapping_values_bukrs )
     CHANGING
       value           = value_tmp ).

    working_hours             = value_tmp.



  ENDMETHOD.


  METHOD map_pos_data.

    DATA: src_id               TYPE string,
          sys_id               TYPE string,
          hr_periods           TYPE hrperiods_tab,
          hr_periods_line      TYPE hrperiods,
          bukrs                TYPE bukrs,
          werks                TYPE werks_d,
          orgunit_name         TYPE stext,
          orgunit_object_id    TYPE hrobjid,
          job_name             TYPE stext,
          oldest_dataset_begda TYPE begda,
          count                TYPE i,
          is_deleted           TYPE bool,
          set_code             TYPE string.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    SORT positions ASCENDING.

    LOOP AT positions ASSIGNING FIELD-SYMBOL(<position>).

      IF is_deleted = abap_true.
        is_deleted = abap_false.
        CONTINUE.
      ENDIF.

      LOOP AT positions ASSIGNING FIELD-SYMBOL(<pos>) WHERE objid = <position>-objid.

        count = count + 1.
        IF count = 1.
          oldest_dataset_begda = <pos>-begda.
          DELETE positions WHERE objid = <pos>-objid AND endda LT sy-datum. "Keep only the actual and future entries
          IF <pos> IS NOT ASSIGNED.
            is_deleted = abap_true.
            CONTINUE.
          ENDIF.
        ELSE.
          <pos>-begda = oldest_dataset_begda.
        ENDIF.

        CONCATENATE pos <pos>-objid INTO src_id.

        get_orgunit_data( EXPORTING object_id = <pos>-objid
                          IMPORTING orgunit_name = orgunit_name
                                    orgunit_object_id = orgunit_object_id ).

        DELETE it1008 WHERE objid = <pos>-objid AND endda LT sy-datum. "Keep only the actual and future entries
        bukrs = get_bukrs( position_object_id = <pos>-objid
                           orgunit_object_id = orgunit_object_id ).
        werks = get_werks( orgunit_object_id = orgunit_object_id ).
        CONCATENATE bukrs werks INTO DATA(location_code).

        map_mig_values( EXPORTING bukrs = bukrs
                        IMPORTING business_unit_name = DATA(business_unit_name)
                                  working_hours = DATA(working_hours) ).

        job_name = get_job_data( object_id = <pos>-objid ).

        set_code = get_set_code( ).

        DATA(effective_start_date) = zmhp_cl_mig_utils=>convert_date( get_effective_start_date( <pos>-objid ) ).
        DATA(effective_end_date) = zmhp_cl_mig_utils=>convert_date( <pos>-endda ).

        CONCATENATE zmhp_cl_mig_utils=>merge
                          position
                          sys_id
                          effective_start_date
                          effective_end_date
                          business_unit_name
                          <pos>-stext "Name
                          <pos>-objid "PositionCode
                          '' "ActionReasonCode
                          'Active' "ActiveStatus
                          orgunit_name "DepartmentName
                          job_name "JobCode
                          set_code "JobSetCode
                          location_code "LocationCode
                          set_code "LocationSetCode
                          '' "SupervisorPersonNumber
                          '' "FullPartTime
                          '' "RegularTemporary
                          'Approved' "HiringStatus
                          'Single Incumbent' "PositionType
                          '' "FTE
                          '1' "HeadCount
                          '' "SecurityClearance
                          '' "ProbationPeriod
                          '' "ProbationPeriodUnitCd
                          '' "BargainingUnitCd
                          '' "CollectiveAgreementCode
                          'N' "OverlapAllowedFlag
                          '' "SeasonalFlag
                          '' "GradeLadderName
                          '' "EntryGradeCode
                          '' "EntryGradeSetCode
                          set_code "EntryStepName
                          '' "StandardWorkingHours
                          '' "StandardWorkingFrequency
                          working_hours "WorkingHours
                          'W' "Frequency
                          '' "StartTime
                          '' "EndTime
                          src_id "SourceSystemId
                          '' "UnionName
                          '' "UnionClassificationCode
                          '' "SeasonalStartDate
                          '' "SeasonalEndDate
                          '' "RequisitionNumber
                          '' "AssignmentCategory
                          '' "BudgetAmount
                          '' "BudgetAmountCurrency
                          '' "BudgetedPositionFlag
                          '' "CostCenterName
                          '' "DelegatePositionCode
                          '' "DelegatePositionBusinessUnitName
                          '' "FundedByExistingPositionFlag
              INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

        CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

      ENDLOOP.
      count = 0.

    ENDLOOP.

  ENDMETHOD.


  METHOD proceed_file_construction.

    CONCATENATE metadata data INTO content SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD proceed_pos_data.

    get_pos_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    data = map_pos_data( ).

  ENDMETHOD.
ENDCLASS.
