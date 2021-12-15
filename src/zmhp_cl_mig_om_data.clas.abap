class ZMHP_CL_MIG_OM_DATA definition
  public
  create public .

public section.

  data OBJEC type OBJEC_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  constants ORG_INFO type STRING value 'OrgInformation' ##NO_TEXT.
  constants PER_ORG_INFO type STRING value 'PER_ORG_MANAGER_INFO' ##NO_TEXT.
  constants DEPARTMENT_CODE type STRING value 'DEPARTMENT' ##NO_TEXT.
  constants DEPARTMENT_CLASS type STRING value 'Department' ##NO_TEXT.
  constants COST_ACCOUNT_DATA type STRING value 'CostAllocationAccountV3' ##NO_TEXT.
  constants COST_ALLOCATION_DATA type STRING value 'CostAllocationV3' ##NO_TEXT.
  constants SET type STRING value '_SET' ##NO_TEXT.
  constants DE_FILE type CHAR255 value 'OM_DE.zip' ##NO_TEXT.
  constants AT_FILE type CHAR255 value 'OM_AT.zip' ##NO_TEXT.
  constants IT_FILE type CHAR255 value 'OM_IT.zip' ##NO_TEXT.
  constants FR_FILE type CHAR255 value 'OM_FR.zip' ##NO_TEXT.
  constants NL_FILE type CHAR255 value 'OM_NL.zip' ##NO_TEXT.

  methods DOWNLOAD_FILES
    importing
      !FILES type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  methods CONSTRUCTOR
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !OBJEC type OBJEC_T
      !AL11 type BOOLEAN .
  methods PROCEED_OM_DATA
    exporting
      !OM_DATA_MIG type STRING
      !OM_DATA_CC type STRING
      !OM_DATA_CA type STRING
    returning
      value(OM_DATA) type TRUXS_T_TEXT_DATA .
  PROTECTED SECTION.
private section.

  data MOLGA_COUNTRY_CODES type T77SFEC_T500L_TAB .
  data HRP1001_UP type HRP1001_T .
  data HRP1001_LEA type HRP1001_T .
  data COST_CENTERS type HRBPREP_COST_DIST_IT .
  data HRP1001_PR type HRP1001_T .
  data HRP1008_BR type P1008_TAB .
  data MAPPING_VALUES_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data ORGID_COUNTRY_CODE type ZMHP_TT_MIG_ORGID_CC .
  data MOLGA type RSDSSELOPT_T .
  data BUKRS_TXT type T_T001 .
  data HRP1001_UP_TMP type HRP1001_T .
  data COST_CENTERS_TMP type HRBPREP_COST_DIST_IT .
  data HRP1001_PR_TMP type HRP1001_T .
  data HRP1008_BR_TMP type P1008_TAB .
  data HRP1008_COM type P1008_TAB .
  data HRP1008_COM_TMP type P1008_TAB .
  data PA9400_PERNR type ZMHP_P9400_TAB .
  constants ORG_INFO_PREFIX type STRING value '_OrgInfo' ##NO_TEXT.
  data MAPPING_FIELDS_DEPARTMENT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_DEPARTMENT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  constants ORG_SOURCE_TYPE type STRING value 'ORG' ##NO_TEXT.
  constants COST type STRING value 'COST' ##NO_TEXT.
  constants LEG_DATA_GROUP_NAME type STRING value 'LDG' ##NO_TEXT.
  data PA0001 type P0001_TAB .
  data PA0001_TMP type P0001_TAB .
  data AL11 type BOOLEAN .

  methods MAP_HR_PERIODS_CA
    importing
      value(HR_PERIODS) type HRPERIODS_TAB
    changing
      !OM_DATA_ENTRY type ZMHP_S_MIG_OM_DATA
    returning
      value(OM_DATA) type ZMHP_TT_MIG_OM_DATA .
  methods MAP_HR_PERIODS
    importing
      value(HR_PERIODS) type HRPERIODS_TAB
    exporting
      !OM_DATA_FUT type ZMHP_TT_MIG_OM_DATA
    changing
      !OM_DATA_ENTRY type ZMHP_S_MIG_OM_DATA
    returning
      value(OM_DATA) type ZMHP_TT_MIG_OM_DATA .
  methods GET_KOSTL_OF_FIRST_EMP
    importing
      !OBJID type OBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(KOSTL) type KOSTL .
  methods GET_HR_PERIODS_CA
    importing
      !OBJID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(HR_PERIODS) type HRPERIODS_TAB .
  methods GET_HR_PERIODS
    importing
      !OBJID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(HR_PERIODS) type HRPERIODS_TAB .
  methods GET_ACCOUNT_ASSIGNMENT .
  methods CREATE_MIG_FILE
    importing
      !DATA type ZMHP_TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods CREATE_META_CC
    returning
      value(METADATA) type STRING .
  methods CREATE_META_CA
    returning
      value(METADATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  methods CREATE_DATA_CC
    importing
      !DATA type ZMHP_TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods CREATE_DATA_CA
    importing
      !DATA type ZMHP_TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods CREATE_DATA
    importing
      !DATA type ZMHP_TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods CREATE_CC_FILE
    importing
      !DATA type ZMHP_TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods CREATE_CA_FILE
    importing
      !DATA type ZMHP_TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods GET_MAPPING_VALUES .
  methods GET_MAPPING_FIELDS .
  methods GET_HEADER_LINE
    returning
      value(HEADER) type STRING .
  methods GET_COUNTRY_TO_ORG_UNIT .
  methods GET_OM_DATA .
  methods MAP_OM_DATA
    exporting
      !OM_DATA_MIG type STRING
      !OM_DATA_CC type STRING
      !OM_DATA_CA type STRING
    returning
      value(OM_DATA_CONV) type TRUXS_T_TEXT_DATA .
  methods GET_BUKRS_TXT .
ENDCLASS.



CLASS ZMHP_CL_MIG_OM_DATA IMPLEMENTATION.


METHOD constructor.
  me->begda = begda.
  me->endda = endda.
  me->objec = objec.
  me->al11  = al11.
ENDMETHOD.


METHOD CREATE_CA_FILE.
  DATA(metadata)  = create_meta_ca( ).
  DATA(data_conv) = create_data_ca( data ).

  CONCATENATE metadata cl_abap_char_utilities=>newline data_conv INTO om_data.
ENDMETHOD.


METHOD create_cc_file.
  DATA(metadata)  = create_meta_cc( ).
  DATA(data_conv) = create_data_cc( data ).

  CONCATENATE metadata cl_abap_char_utilities=>newline data_conv INTO om_data.
ENDMETHOD.


METHOD create_data.

  DATA: seq_number      TYPE string,
        src_id          TYPE string,
        count_s         TYPE string,
        department_name TYPE string,
        leg_code        TYPE string,
        value_tmp       TYPE zmhp_dd_value,
        count           TYPE i VALUE 1,
        sys_id          TYPE string.

  sys_id = 'EBS-HR'.

  LOOP AT data ASSIGNING FIELD-SYMBOL(<data>).

    count_s = CONV #( count ).
    CONDENSE count_s.
    CONCATENATE <data>-integration_key org_info_prefix INTO src_id.

    "Process department Mapping
    value_tmp = CONV #( <data>-integration_key ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>orgeh
        field_oracle   = zmhp_cl_mig_utils=>departmentname
        mapping_fields = CONV #( mapping_fields_department )
        mapping_values = CONV #( mapping_values_department )
      CHANGING
        value          = value_tmp ).

    department_name = value_tmp.

    CHECK department_name IS NOT INITIAL.

    CONCATENATE zmhp_cl_mig_utils=>merge
                org_info
                per_org_info
                department_code
                <data>-effective_start_date
                <data>-effective_end_date
                department_name
                department_code
                per_org_info
                department_class
                leg_code
                seq_number
                sys_id
                src_id
                <data>-oracleid
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    ADD 1 TO count.

    CONCATENATE om_data cl_abap_char_utilities=>newline data_tmp INTO om_data.

  ENDLOOP.
ENDMETHOD.


METHOD create_data_ca.

  DATA: department_name TYPE string,
        leg_code        TYPE string,
        value_tmp       TYPE zmhp_dd_value.

  LOOP AT data ASSIGNING FIELD-SYMBOL(<data>).

    "Process department Mapping
    value_tmp = CONV #( <data>-integration_key ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>orgeh
        field_oracle   = zmhp_cl_mig_utils=>departmentname
        mapping_fields = CONV #( mapping_fields_department )
        mapping_values = CONV #( mapping_values_department )
      CHANGING
        value          = value_tmp ).

    department_name = value_tmp.

    CHECK department_name IS NOT INITIAL.

    CONCATENATE <data>-country leg_data_group_name INTO leg_code SEPARATED BY SPACE.

    CONCATENATE zmhp_cl_mig_utils=>merge
                cost_allocation_data
                <data>-effective_end_date
                <data>-effective_start_date
                org_source_type
                department_name
                leg_code
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE om_data cl_abap_char_utilities=>newline data_tmp INTO om_data.
    CLEAR: leg_code.

  ENDLOOP.
ENDMETHOD.


METHOD create_data_cc.

  DATA: department_name TYPE string,
        leg_code        TYPE string,
        conc_segment    TYPE string,
        segment2        TYPE string,
        segment3        TYPE string,
        segment4        TYPE string,
        segment5        TYPE string,
        value_tmp       TYPE zmhp_dd_value,
        cc_orgid        TYPE /iwbep/t_mgw_name_value_pair.

  LOOP AT data ASSIGNING FIELD-SYMBOL(<data>). "WHERE cost_center IS NOT INITIAL. "JMB20210721 D - Proceed even if no cost center was found

    READ TABLE cc_orgid ASSIGNING FIELD-SYMBOL(<cc_orgid>) WITH TABLE KEY name  = <data>-integration_key
                                                                          value = <data>-cost_center.

    CHECK sy-subrc NE 0.

    "Process department Mapping
    value_tmp = CONV #( <data>-integration_key ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>orgeh
        field_oracle   = zmhp_cl_mig_utils=>departmentname
        mapping_fields = CONV #( mapping_fields_department )
        mapping_values = CONV #( mapping_values_department )
      CHANGING
        value          = value_tmp ).

    department_name = value_tmp.

    CHECK department_name IS NOT INITIAL.

    IF <data>-cost_center IS NOT INITIAL.
      CONCATENATE <data>-company '-' <data>-cost_center INTO segment2.
    ENDIF.
    CONCATENATE <data>-country leg_data_group_name INTO leg_code SEPARATED BY space.

    CONCATENATE zmhp_cl_mig_utils=>merge
                cost_account_data
                org_source_type
                department_name
                leg_code
                <data>-effective_start_date
                cost
                '1'
                '1'
                conc_segment
                <data>-company
                segment2
                segment3
                segment4
                segment5
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE om_data cl_abap_char_utilities=>newline data_tmp INTO om_data.
    CLEAR: segment2, leg_code.
    APPEND VALUE #( name = <data>-integration_key value = <data>-cost_center ) TO cc_orgid.
  ENDLOOP.
ENDMETHOD.


METHOD create_metadata.

  CONCATENATE zmhp_cl_mig_utils=>metadata
              'OrgInformation'
              'FLEX:PER_ORGANIZATION_INFORMATION_EFF'
              'EFF_CATEGORY_CODE'
              'EffectiveStartDate'
              'EffectiveEndDate'
              'OrganizationName'
              'ClassificationCode'
              'OrgInformationContext'
              'ClassificationName'
              'LegislationCode'
              'SequenceNumber'
              'SourceSystemOwner'
              'SourceSystemId'
              '_MANAGER(PER_ORGANIZATION_INFORMATION_EFF=PER_ORG_MANAGER_INFO)'
   INTO metadata SEPARATED BY zmhp_cl_mig_utils=>separator.

ENDMETHOD.


METHOD create_meta_ca.

  CONCATENATE zmhp_cl_mig_utils=>metadata
              cost_allocation_data
              'EffectiveEndDate'
              'EffectiveStartDate'
              'SourceType'
              'DepartmentName'
              'LegislativeDataGroupName'
   INTO metadata SEPARATED BY zmhp_cl_mig_utils=>separator.

ENDMETHOD.


METHOD create_meta_cc.

  CONCATENATE zmhp_cl_mig_utils=>metadata
              cost_account_data
              'SourceType'
              'DepartmentName'
              'LegislativeDataGroupName'
              'EffectiveDate'
              'SourceSubType'
              'Proportion'
              'SubTypeSequence'
              'ConcatenatedSegment'
              'Segment1'
              'Segment2'
              'Segment3'
              'Segment4'
              'Segment5'
  INTO metadata SEPARATED BY zmhp_cl_mig_utils=>separator.

ENDMETHOD.


METHOD create_mig_file.
  DATA(metadata)  = create_metadata( ).
  DATA(data_conv) = create_data( data ).

  CONCATENATE metadata cl_abap_char_utilities=>newline data_conv INTO om_data.
ENDMETHOD.


METHOD download_files.

  DATA: content   TYPE stringtab,
        filename  TYPE string,
        content_x TYPE xstring,
        zip_file  TYPE string,
        zip_tab   TYPE swxmlcont.

  CASE al11.
    WHEN abap_true.
      DATA(zip) = NEW cl_abap_zip( ).

      LOOP AT files ASSIGNING FIELD-SYMBOL(<files_zip>).
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = <files_zip>-value
          IMPORTING
            buffer = content_x.

        zip->add( name    = <files_zip>-name
                  content = content_x ).
      ENDLOOP.

      DATA(zip_xstring) =  zip->save( ).

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = zip_xstring
        TABLES
          binary_tab = zip_tab.

      zip_file = SWITCH #( sy-mandt
                           WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN fr_file
                           WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN de_file
                           WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN nl_file ).
      "Austria and Italy are on the same mandant
      CASE sy-mandt.
        WHEN zmhp_cl_int_constants=>cofu_mandant-italy.
          IF molga IS NOT INITIAL.
            "Austria
            IF     '03' IN molga.
              zip_file = at_file.
            "Italy
            ELSEIF '15' IN molga.
              zip_file = it_file.
            ENDIF.
          ENDIF.
      ENDCASE.

      CHECK zip_file IS NOT INITIAL.

      DATA(logical_filename) = CONV filename-fileintern('ZMHP_HCM_ORACLE_EXTRACTS_COGU').
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = logical_filename
          parameter_1      = zip_file
        IMPORTING
          file_name        = filename
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.

      CHECK filename IS NOT INITIAL.

      OPEN DATASET filename FOR OUTPUT IN BINARY MODE.
      CHECK sy-subrc IS INITIAL.

      LOOP AT zip_tab ASSIGNING FIELD-SYMBOL(<zip_tab>).
        TRANSFER <zip_tab> TO filename.
      ENDLOOP.
      CLOSE DATASET filename.
    WHEN abap_false.

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
  ENDCASE.

ENDMETHOD.


METHOD get_account_assignment.

  DATA: struc     TYPE struc_t.

  "relevant otypes
  DATA(otype_rel) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'IC' )
                                        ( sign = 'I' option = 'EQ' low = 'I1' ) ).

  LOOP AT objec ASSIGNING FIELD-SYMBOL(<objec>).
    "get account assignment of orgunit
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = <objec>-otype
        act_objid      = <objec>-objid
        act_wegid      = 'OMACC_U'
        act_plvar      = <objec>-plvar
        act_begda      = <objec>-begda
        act_endda      = <objec>-endda
      TABLES
        result_struc   = struc
      EXCEPTIONS
        no_entry_found = 1
        no_plvar_found = 2.

    "get company code and personnel subareas
    DELETE struc WHERE otype NOT IN otype_rel.
    SORT struc BY otype objid vbegda vendda.
    DELETE ADJACENT DUPLICATES FROM struc COMPARING otype objid vbegda vendda.

    "collect all personnel areas
    APPEND LINES OF VALUE p1008_tab( FOR <struc_br> IN struc WHERE ( otype EQ 'I1' ) ( objid = <objec>-objid
                                                                                       begda = <struc_br>-vbegda
                                                                                       endda = <struc_br>-vendda
                                                                                       btrtl = <struc_br>-objid+4(4) ) ) TO hrp1008_br.

    "collect all company codes
    APPEND LINES OF VALUE p1008_tab( FOR <struc_br> IN struc WHERE ( otype EQ 'IC' ) ( objid = <objec>-objid
                                                                                       begda = <struc_br>-vbegda
                                                                                       endda = <struc_br>-vendda
                                                                                       bukrs = <struc_br>-objid ) ) to hrp1008_com.

    CLEAR: struc.
  ENDLOOP.

ENDMETHOD.


METHOD get_bukrs_txt.
  DATA(bukrs) = VALUE rsdsselopt_t( FOR <bukrs> IN bukrs_txt ( sign = 'I' option = 'EQ' low = <bukrs>-bukrs ) ).

  CLEAR bukrs_txt.

  SELECT bukrs, butxt FROM t001 INTO CORRESPONDING FIELDS OF TABLE @bukrs_txt WHERE bukrs IN @bukrs.
ENDMETHOD.


METHOD get_country_to_org_unit.

  DATA: org_unit_id           TYPE pd_objid_r,
        results               TYPE tswhactor,
        main_org_country_code TYPE zmhp_tt_mig_orgid_cc,
        country_code          TYPE intca,
        country_codes         TYPE rsdsselopt_t.

  LOOP AT objec ASSIGNING FIELD-SYMBOL(<objec>).
    DATA(objid) = <objec>-objid.
    CALL FUNCTION 'ZMHP_HROMFB_GET_ABTEILUNG_NEW'
      EXPORTING
        plvar          = <objec>-plvar
        otype          = <objec>-otype
        objid          = <objec>-objid
        begda          = <objec>-begda
        endda          = <objec>-endda
      IMPORTING
        geschaeftsfuehrung = org_unit_id
      EXCEPTIONS
        objid_initial  = 1
        not_found      = 2
        err_infty1003  = 3
        internal_error = 4
        no_plvar       = 5.

    "check if main org unit already was readed
    LOOP AT main_org_country_code ASSIGNING FIELD-SYMBOL(<main_org_unit>) WHERE objid EQ org_unit_id   AND
                                                                                begda LE <objec>-endda AND
                                                                                endda GE <objec>-begda.
      country_code = <main_org_unit>-land1.
      EXIT.
    ENDLOOP.

    IF <main_org_unit> IS NOT ASSIGNED.

      "get account assignment details of main orgunit
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = <objec>-otype
          act_objid      = org_unit_id
          act_wegid      = 'OMACC_U'
          act_plvar      = <objec>-plvar
          act_begda      = <objec>-begda
          act_endda      = <objec>-endda
        TABLES
          result_tab     = results
        EXCEPTIONS
          no_entry_found = 1
          no_plvar_found = 2.

      "get company code
      READ TABLE results INTO DATA(result) WITH KEY otype = 'IC'.

      CALL FUNCTION 'BBP_GET_COMPANY_COUNTRY'
        EXPORTING
          iv_company = CONV bukrs( result-objid )
        IMPORTING
          ev_country = country_code.

      APPEND VALUE #( objid = org_unit_id
                      begda = <objec>-begda
                      endda = <objec>-endda
                      land1 = CONV #( country_code ) ) TO main_org_country_code.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = country_code ) TO country_codes.
      APPEND VALUE #( bukrs = result-objid ) TO bukrs_txt.

    ENDIF.

    APPEND VALUE #( objid = <objec>-objid
                    begda = <objec>-begda
                    endda = <objec>-endda
                    land1 = CONV #( country_code ) ) TO orgid_country_code.

    CLEAR: country_code.
    UNASSIGN: <main_org_unit>.
  ENDLOOP.

  "get molga according to country code
  SORT country_codes BY low.
  DELETE ADJACENT DUPLICATES FROM country_codes COMPARING low.

  SELECT molga, intca FROM t500l INTO TABLE @molga_country_codes WHERE intca IN @country_codes.

  me->molga = VALUE rsdsselopt_t( FOR <molga> IN molga_country_codes ( sign = 'I' option = 'EQ' low = <molga>-molga ) ).
ENDMETHOD.


  METHOD get_header_line.

    CONCATENATE   'Integration Key'
                  'Effective Start Date'
                  'Effective End Date'
                  'Department Set'
                  'Name'
                  'Status'
                  'Action Reason'
                  'Internal Address Line'
                  'Location'
                  'Name Short'
                  'Manager'
                  'OracleId'
                  'Company'
                  'Company Name'
                  'Cost Center'
                  'Parent Integration Key'
                  'Country'
    INTO header SEPARATED BY ';'.

  ENDMETHOD.


METHOD get_hr_periods.
  "default time range
  APPEND VALUE #( begda = begda
                  endda = endda ) TO hr_periods.

  LOOP AT pa0001 ASSIGNING FIELD-SYMBOL(<pa0001>) WHERE orgeh EQ objid.
    APPEND VALUE #( begda = <pa0001>-begda
                    endda = <pa0001>-endda ) TO hr_periods.
    APPEND <pa0001> TO pa0001_tmp.
  ENDLOOP.

  LOOP AT hrp1001_up ASSIGNING FIELD-SYMBOL(<hrp1001_up>) WHERE objid EQ objid.
    <hrp1001_up>-endda = COND #( WHEN <hrp1001_up>-endda GT endda
                                 THEN endda
                                 ELSE <hrp1001_up>-endda ).
    APPEND VALUE #( begda = <hrp1001_up>-begda
                    endda = <hrp1001_up>-endda ) TO hr_periods.
    APPEND <hrp1001_up> TO hrp1001_up_tmp.
  ENDLOOP.

  LOOP AT hrp1008_br ASSIGNING FIELD-SYMBOL(<hrp1008_br>) WHERE objid EQ objid.
    <hrp1008_br>-endda = COND #( WHEN <hrp1008_br>-endda GT endda
                                 THEN endda
                                 ELSE <hrp1008_br>-endda ).
    APPEND VALUE #( begda = <hrp1008_br>-begda
                    endda = <hrp1008_br>-endda ) TO hr_periods.
    APPEND <hrp1008_br> TO hrp1008_br_tmp.
  ENDLOOP.

  LOOP AT hrp1008_com ASSIGNING FIELD-SYMBOL(<hrp1008_com>) WHERE objid EQ objid.
    <hrp1008_com>-endda = COND #( WHEN <hrp1008_com>-endda GT endda
                                  THEN endda
                                  ELSE <hrp1008_com>-endda ).
    APPEND VALUE #( begda = <hrp1008_com>-begda
                    endda = <hrp1008_com>-endda ) TO hr_periods.
    APPEND <hrp1008_com> TO hrp1008_com_tmp.
  ENDLOOP.

  LOOP AT cost_centers ASSIGNING FIELD-SYMBOL(<cost_center>) WHERE objid EQ objid.
    <cost_center>-endda = COND #( WHEN <cost_center>-endda GT endda
                                  THEN endda
                                  ELSE <cost_center>-endda ).
    APPEND VALUE #( begda = <cost_center>-begda
                    endda = <cost_center>-endda ) TO hr_periods.
    APPEND <cost_center> TO cost_centers_tmp.
  ENDLOOP.

  SORT hr_periods.
  DELETE ADJACENT DUPLICATES FROM hr_periods.

  CALL FUNCTION 'RHXPROVIDE_PERIODS'
    TABLES
      provide_tab = hr_periods.

  DELETE hr_periods WHERE endda LT me->begda. "JMB20210706 I - Keep only the actual and future entries

ENDMETHOD.


METHOD get_hr_periods_ca.

  LOOP AT hrp1008_com ASSIGNING FIELD-SYMBOL(<hrp1008_com>) WHERE objid EQ objid.
    <hrp1008_com>-endda = COND #( WHEN <hrp1008_com>-endda GT endda
                                  THEN endda
                                  ELSE <hrp1008_com>-endda ).
    APPEND VALUE #( begda = <hrp1008_com>-begda
                    endda = <hrp1008_com>-endda ) TO hr_periods.
    APPEND <hrp1008_com> TO hrp1008_com_tmp.
  ENDLOOP.

  LOOP AT cost_centers ASSIGNING FIELD-SYMBOL(<cost_center>) WHERE objid EQ objid.
    <cost_center>-endda = COND #( WHEN <cost_center>-endda GT endda
                                  THEN endda
                                  ELSE <cost_center>-endda ).
    APPEND VALUE #( begda = <cost_center>-begda
                    endda = <cost_center>-endda ) TO hr_periods.
    APPEND <cost_center> TO cost_centers_tmp.
  ENDLOOP.

  SORT hr_periods.
  DELETE ADJACENT DUPLICATES FROM hr_periods.

  CALL FUNCTION 'RHXPROVIDE_PERIODS'
    TABLES
      provide_tab = hr_periods.

  "delete passed periods
  DELETE hr_periods WHERE endda LT sy-datum.

ENDMETHOD.


METHOD get_kostl_of_first_emp.
  DATA: plvar        TYPE plvar,
        result_objec TYPE objec_t.

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  "get only employees in same OrgUnit
  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype      = 'O'
      act_objid      = objid
      act_wegid      = 'O-P'
      act_plvar      = plvar
      act_begda      = begda
      act_endda      = endda
    TABLES
      result_objec   = result_objec
    EXCEPTIONS
      no_plvar_found = 1
      no_entry_found = 2
      OTHERS         = 3.

  "keep only employees
  DELETE result_objec WHERE otype NE 'P'.

  CHECK result_objec IS NOT INITIAL.

  "get first employee
  READ TABLE result_objec ASSIGNING FIELD-SYMBOL(<employee>) INDEX 1.

  CHECK <employee> IS ASSIGNED.

  "get cost center of employee
  SELECT SINGLE kostl INTO @kostl FROM pa0001 WHERE pernr EQ @<employee>-objid AND
                                                    begda LE @endda            AND
                                                    endda GE @begda.
  ENDMETHOD.


METHOD get_mapping_fields.

  "get mapping fields for company code
  SELECT * FROM zmhp_int_mapp_fi INTO TABLE @mapping_fields_bukrs WHERE molga        IN @molga   AND
                                                                        infty        = '1008'    AND
                                                                        field_sap    = 'BUKRS'   AND
                                                                        field_oracle = 'COMPANY' AND
                                                                        export       = @abap_true.

  "get mapping fields for departmentname
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>orgeh
                                                   oracle_field = zmhp_cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_department ).

ENDMETHOD.


METHOD get_mapping_values.

  SELECT * FROM zmhp_int_mapping INTO TABLE @mapping_values_bukrs WHERE molga        IN @molga   AND
                                                                        infty        = '1008'    AND
                                                                        field_sap    = 'BUKRS'   AND
                                                                        field_oracle = 'COMPANY' AND
                                                                        export       = @abap_true.

  "get mapping values for departmentname
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>orgeh
                                                   oracle_field = zmhp_cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_department ).
ENDMETHOD.


METHOD get_om_data.
  DATA: plvar        TYPE plvar.

  "get all objid
  DATA(objid) = VALUE rsdsselopt_t( FOR <objec> IN objec ( sign = 'I' option = 'EQ' low = <objec>-objid ) ).

  "get upper assigned orgunit
  SELECT objid, begda, endda, sobid INTO CORRESPONDING FIELDS OF TABLE @hrp1001_up FROM hrp1001 WHERE objid IN @objid AND
                                                                                                      otype EQ 'O'    AND
                                                                                                      rsign EQ 'A'    AND
                                                                                                      relat EQ '002'  AND
                                                                                                      sclas EQ 'O'    AND
                                                                                                      begda LE @endda AND
                                                                                                      endda GE @begda.
  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  DATA(orgid) = VALUE hrrootob_t( FOR <objid> IN objid ( otype = 'O' objid = <objid>-low ) ).

  "get assigned cost center
  CALL FUNCTION 'RH_COSTCENTER_OF_OBJECT_GET'
    EXPORTING
      plvar                        = plvar
      begda                        = cl_hcp_global_constants=>c_lowdate
      endda                        = cl_hcp_global_constants=>c_highdate
      active                       = 'X'
    TABLES
      in_objects                   = orgid
      main_costcenters             = cost_centers
    EXCEPTIONS
      given_p0001_tab_not_complete = 1
      no_authorization_1018        = 2
      OTHERS                       = 3.

  "delete all cost center assignments which are older than begda
  DELETE cost_centers WHERE endda LT begda.

**JMB20210819 start insert - remove company code from cost center id
*
  LOOP AT cost_centers ASSIGNING FIELD-SYMBOL(<cc>).
    DATA(l) = strlen( <cc>-sobid ).
    DATA(max_l) = l - 4.

    CHECK max_l GT 0.
    <cc>-sobid = <cc>-sobid+0(max_l).
  ENDLOOP.
*JMB20210819

  "get managers in time range of object
  pa0001 = VALUE p0001_tab( FOR <objec> IN objec ( begda = <objec>-begda endda = <objec>-endda orgeh = <objec>-objid ) ).
  zmhp_cl_mig_utils=>check_assign_supervisor( IMPORTING manager_pernr = DATA(manager_pernr)
                                               CHANGING p0001         = pa0001 ).
  IF manager_pernr IS NOT INITIAL.
    SELECT pernr, begda, endda, oracleid INTO CORRESPONDING FIELDS OF TABLE @pa9400_pernr FROM pa9400 WHERE pernr IN @manager_pernr AND
                                                                                                            begda LE @endda         AND
                                                                                                            endda GE @begda.
  ENDIF.

  "get location
  get_account_assignment( ).

  "get molga to org unit
  get_country_to_org_unit( ).

  "get mapping values
  get_mapping_fields( ).

  "get mapping values
  get_mapping_values( ).

  "get bukrs text
  get_bukrs_txt( ).
ENDMETHOD.


METHOD map_hr_periods.
  DATA: value      TYPE zmhp_dd_value,
        plvar      TYPE plvar,
        manager_id TYPE realo.

  "get mapping fields for locationcode
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = CONV #( VALUE #( ( sign = 'I' option = 'EQ' low = '*' ) ) )
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>btrtl
                                                   oracle_field = zmhp_cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = DATA(mapping_fields_btrtl) ).

**JMB20210706 start insert - store exception departments
*
  DATA(company_0021) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '00050379' )
                                           ( sign = 'I' option = 'EQ' low = '00000003' )
                                           ( sign = 'I' option = 'EQ' low = '00000002' )
                                           ( sign = 'I' option = 'EQ' low = '00000004' )
                                           ( sign = 'I' option = 'EQ' low = '00071312' )
                                           ( sign = 'I' option = 'EQ' low = '00071313' )
                                           ( sign = 'I' option = 'EQ' low = '00000005' )
                                           ( sign = 'I' option = 'EQ' low = '00000011' )
                                           ( sign = 'I' option = 'EQ' low = '00074183' ) ).

  DATA(company_0028) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '00000012' )
                                           ( sign = 'I' option = 'EQ' low = '00069298' )
                                           ( sign = 'I' option = 'EQ' low = '00069299' ) ).
*JMB20210706 end insert

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<hr_periods>).
    om_data_entry-effective_start_date = zmhp_cl_mig_utils=>convert_date( <hr_periods>-begda ).
    om_data_entry-effective_end_date   = zmhp_cl_mig_utils=>convert_date( <hr_periods>-endda ).

    LOOP AT pa0001_tmp ASSIGNING FIELD-SYMBOL(<pa0001>) WHERE begda LE <hr_periods>-endda AND
                                                              endda GE <hr_periods>-begda.

      CALL FUNCTION 'RH_GET_LEADER'
        EXPORTING
          plvar                     = plvar
          keydate                   = <hr_periods>-begda
          otype                     = 'O'
          objid                     = CONV realo( <pa0001>-orgeh )
          consider_vac_pos          = 'X'
        IMPORTING
          leader_id                 = manager_id
        EXCEPTIONS
          no_leader_found           = 1
          no_leading_position_found = 2
          OTHERS                    = 3.

      IF manager_id IS NOT INITIAL.
        om_data_entry-manager              = manager_id.
        LOOP AT pa9400_pernr ASSIGNING FIELD-SYMBOL(<p9400>) WHERE pernr EQ manager_id         AND
                                                                   begda LE <hr_periods>-endda AND
                                                                   endda GE <hr_periods>-begda.
          om_data_entry-oracleid = <p9400>-oracleid.
          EXIT.
        ENDLOOP.
      ENDIF.
      CLEAR: manager_id.
      EXIT.
    ENDLOOP.

    LOOP AT hrp1001_up_tmp ASSIGNING FIELD-SYMBOL(<hrp1001_up>) WHERE begda LE <hr_periods>-endda AND
                                                                      endda GE <hr_periods>-begda.
      om_data_entry-parent_integration_key = <hrp1001_up>-sobid.
      EXIT.
    ENDLOOP.

**JMB20210819 start insert - in case no parent integration key was found, pass entry
*
    IF om_data_entry-parent_integration_key IS NOT INITIAL.
*JMB20210819 end insert

**JMB20210706 start insert - check mandant and departments for CoFu countries and set Department_SET
*
      CASE sy-mandt.
        WHEN '005'. "France
          om_data_entry-company_name = '0005'.
          om_data_entry-company      = '0005'.
        WHEN '122'. "New Zealand
          om_data_entry-company_name = '2200'.
          om_data_entry-company      = '2200'.
        WHEN '120'. "Australia
          om_data_entry-company_name = '2000'.
          om_data_entry-company      = '2000'.
        WHEN '190'. "Netherlands
          om_data_entry-company_name = '9000'.
          om_data_entry-company      = '9000'.
        WHEN '102'. "Germany
          om_data_entry-company_name = '0001'.
          om_data_entry-company      = '0001'.
      ENDCASE.

      IF om_data_entry-integration_key IN company_0021.
        om_data_entry-company_name = '0021'.
        om_data_entry-company      = '0021'.
      ENDIF.

      IF om_data_entry-integration_key IN company_0028.
        om_data_entry-company_name = '0028'.
        om_data_entry-company      = '0028'.
      ENDIF.

*JMB20210706 end insert

      IF om_data_entry-company IS INITIAL.
        LOOP AT hrp1008_com_tmp ASSIGNING FIELD-SYMBOL(<hrp1008_com>) WHERE begda LE <hr_periods>-endda AND
                                                                            endda GE <hr_periods>-begda.
          "get bukrs text
          READ TABLE bukrs_txt INTO DATA(butxt) WITH KEY bukrs = <hrp1008_com>-bukrs.
          om_data_entry-company_name = butxt-butxt.
          om_data_entry-company = <hrp1008_com>-bukrs.    "JMB20210526 I

          EXIT.
        ENDLOOP.
      ENDIF.

      LOOP AT cost_centers_tmp ASSIGNING FIELD-SYMBOL(<cost_center>) WHERE begda LE <hr_periods>-endda AND
                                                                           endda GE <hr_periods>-begda.

**JMB20210939 start insert - in case of cost center 0018070000, pass company code 0021
*
        IF <cost_center>-sobid EQ '0018070000'.
          om_data_entry-company_name = '0021'.
          om_data_entry-company      = '0021'.
        ENDIF.
*JMB20210939 insert end

        om_data_entry-cost_center = om_data_entry-company && '-' && <cost_center>-sobid.
        EXIT.
      ENDLOOP.

**JMB20210526 start insert - in case no cost center was found
*
      IF  om_data_entry-cost_center IS INITIAL.

        DATA(kostl) = get_kostl_of_first_emp( objid = om_data_entry-integration_key
                                              begda = <hr_periods>-begda
                                              endda = <hr_periods>-endda ).

        IF kostl IS NOT INITIAL.
**JMB20210939 start insert - in case of cost center 0018070000, pass company code 0021
*
          IF kostl EQ '0018070000'.
            om_data_entry-company_name = '0021'.
            om_data_entry-company      = '0021'.
          ENDIF.
*JMB20210939 insert end

          om_data_entry-cost_center = om_data_entry-company && '-' && kostl.
          CLEAR: kostl.
        ENDIF.
      ENDIF.
*JMB20210526 end insert

      LOOP AT hrp1008_br_tmp ASSIGNING FIELD-SYMBOL(<hrp1008_br>) WHERE begda LE <hr_periods>-endda AND
                                                                        endda GE <hr_periods>-begda.
**JMB20210805 start insert - check complex mapping
*
        DATA(fields) = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty = zmhp_cl_mig_utils=>it0001
                                                                     field_sap = zmhp_cl_mig_utils=>btrtl
                                                                     value = <hrp1008_br>-btrtl )
                                                                   ( infty = zmhp_cl_mig_utils=>it0001
                                                                     field_sap = zmhp_cl_mig_utils=>werks
                                                                     value = om_data_entry-company ) ).
        "Process WERKS/BTRTL mapping (LocationCode)
        zmhp_cl_int_mapping=>process_mapping(
          EXPORTING
            import         = abap_false
            export         = abap_true
            infty          = zmhp_cl_mig_utils=>it0001
            field_sap      = zmhp_cl_mig_utils=>btrtl
            field_oracle   = zmhp_cl_mig_utils=>locationcode
            mapping_fields = CONV #( mapping_fields_btrtl )
            fields         = fields
          CHANGING
            value          = value ).

        CHECK value IS NOT INITIAL.
        om_data_entry-location = value.
        CLEAR: value.
*JMB20210805 insert end
*      om_data_entry-location               = <hrp1008_br>-btrtl. "JMB20210805 D
        EXIT.
      ENDLOOP.

**JMB20211031 start insert - in case of PowerSystem company code (0028) pass specific set code
*
      IF om_data_entry-company EQ '0028' and
         sy-mandt              EQ zmhp_cl_int_constants=>cofu_mandant-germany.
        om_data_entry-department_set = 'DE_PS' && set.
      ENDIF.
*JMB20211031 insert end

      APPEND om_data_entry TO om_data.
      APPEND om_data_entry TO om_data_fut.
    ENDIF.

    CLEAR: om_data_entry-manager,
           om_data_entry-parent_integration_key,
           om_data_entry-location,
           om_data_entry-company_name,
           om_data_entry-company,
           om_data_entry-cost_center.
  ENDLOOP.
  CLEAR: cost_centers_tmp, hrp1008_br_tmp, hrp1008_com_tmp, hrp1001_pr_tmp, hrp1001_up_tmp, pa0001_tmp.
ENDMETHOD.


METHOD map_hr_periods_ca.
  DATA: value      TYPE zmhp_dd_value,
        plvar      TYPE plvar,
        manager_id TYPE realo.

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<hr_periods>).
    om_data_entry-effective_start_date = zmhp_cl_mig_utils=>convert_date( <hr_periods>-begda ).
    om_data_entry-effective_end_date   = zmhp_cl_mig_utils=>convert_date( <hr_periods>-endda ).

    LOOP AT hrp1008_com_tmp ASSIGNING FIELD-SYMBOL(<hrp1008_com>) WHERE begda LE <hr_periods>-endda AND
                                                                        endda GE <hr_periods>-begda.
      "get bukrs text
      READ TABLE bukrs_txt INTO DATA(butxt) WITH KEY bukrs = <hrp1008_com>-bukrs.
      om_data_entry-company_name = butxt-butxt.
      om_data_entry-company = <hrp1008_com>-bukrs.    "JMB20210526 I
      EXIT.
    ENDLOOP.

    LOOP AT cost_centers_tmp ASSIGNING FIELD-SYMBOL(<cost_center>) WHERE begda LE <hr_periods>-endda AND
                                                                         endda GE <hr_periods>-begda.
      om_data_entry-cost_center            = <cost_center>-sobid.
      EXIT.
    ENDLOOP.

**JMB20210526 start insert - in case no cost center was found
*
    IF  om_data_entry-cost_center IS INITIAL.
      om_data_entry-cost_center = get_kostl_of_first_emp( objid = om_data_entry-integration_key
                                                          begda = <hr_periods>-begda
                                                          endda = <hr_periods>-endda ).
    ENDIF.
*JMB20210526 end insert

    APPEND om_data_entry TO om_data.

    CLEAR: om_data_entry-manager,
           om_data_entry-parent_integration_key,
           om_data_entry-location,
           om_data_entry-company_name,
           om_data_entry-company,
           om_data_entry-cost_center.
  ENDLOOP.
  CLEAR: cost_centers_tmp, hrp1008_br_tmp, hrp1008_com_tmp, hrp1001_pr_tmp, hrp1001_up_tmp, pa0001_tmp.
ENDMETHOD.


METHOD map_om_data.

  DATA: om_data          TYPE zmhp_tt_mig_om_data,
        om_data_imp      TYPE zmhp_tt_mig_om_data,
        om_data_ca_tmp   TYPE zmhp_tt_mig_om_data,
        om_data_fut      TYPE zmhp_tt_mig_om_data,
        om_entry         TYPE zmhp_s_mig_om_data,
        om_data_conv_tmp TYPE truxs_t_text_data,
        value_tmp        TYPE zmhp_dd_value.

  LOOP AT objec ASSIGNING FIELD-SYMBOL(<objec>).
    om_entry-integration_key = <objec>-objid.
    om_entry-name_short      = <objec>-short.
    om_entry-status          = 'Active'.

    READ TABLE orgid_country_code INTO DATA(orgid_cc_entry) WITH KEY objid = <objec>-objid
                                                                     begda = <objec>-begda
                                                                     endda = <objec>-endda.

**JMB20210706 start insert - check mandant for CoFu countries and set Department_SET
*
    CASE sy-mandt.
      WHEN '005'. "France
        orgid_cc_entry-land1 = 'FR'.
      WHEN '122'. "New Zealand
        orgid_cc_entry-land1 = 'NZ'.
      WHEN '120'. "Australia
        orgid_cc_entry-land1 = 'AU'.
      WHEN '190'. "Netherlands
        orgid_cc_entry-land1 = 'NL'.
      WHEN '102'. "Germany
        orgid_cc_entry-land1 = 'DE'.
    ENDCASE.
*JMB20210706 end insert

    "set country of main org unit
    om_entry-country = orgid_cc_entry-land1.
    CONCATENATE orgid_cc_entry-land1 set INTO DATA(department_set).

    om_entry-department_set = COND #( WHEN om_entry-country IS NOT INITIAL
                                      THEN department_set
                                      ELSE '' ).

**JMB20211008 start deletion - pass names from HRP1000 for CoGu
*
*    "Process department Mapping
*    CLEAR value_tmp.
*    value_tmp = CONV #( <objec>-objid ).
*    zmhp_cl_int_mapping=>process_mapping(
*      EXPORTING
*        import         = abap_false
*        export         = abap_true
*        infty          = zmhp_cl_mig_utils=>it0001
*        field_sap      = zmhp_cl_mig_utils=>orgeh
*        field_oracle   = zmhp_cl_mig_utils=>departmentname
*        mapping_fields = CONV #( mapping_fields_department )
*        mapping_values = CONV #( mapping_values_department )
*      CHANGING
*        value          = value_tmp ).
*
*    om_entry-name = value_tmp.
**JMB20211008 start deletion - pass names from HRP1000
*
*    IF om_entry-name IS INITIAL OR
*       om_entry-name EQ CONV string( <objec>-objid ).
      om_entry-name            = <objec>-stext.
*      COND #( WHEN om_entry-country IS NOT INITIAL
*              THEN <objec>-stext    && '-' && om_entry-country
*              ELSE <objec>-stext ).
*    ENDIF.
*JMB20211008 deletion end

    DATA(om_data_tmp) = map_hr_periods( EXPORTING hr_periods    = get_hr_periods( EXPORTING objid = <objec>-objid
                                                                                            begda = <objec>-begda
                                                                                            endda = <objec>-endda )
                                        IMPORTING om_data_fut   = om_data_fut
                                        CHANGING  om_data_entry = om_entry ).

    APPEND LINES OF om_data_tmp TO om_data.

    "pass only actual and future sentence to mig file
    APPEND LINES OF om_data_fut TO om_data_imp.

**JMB20210528 start insert- create Costing data
*
    APPEND LINES OF map_hr_periods_ca( EXPORTING hr_periods    = get_hr_periods_ca( EXPORTING objid = <objec>-objid
                                                                                              begda = <objec>-begda
                                                                                              endda = <objec>-endda )
                                       CHANGING  om_data_entry = om_entry ) TO om_data_ca_tmp.
*JMB20210528 end insert

    CLEAR: om_entry, orgid_cc_entry, om_data_tmp.
  ENDLOOP.

  om_data_mig = create_mig_file( om_data_fut ).
  om_data_ca  = create_ca_file( om_data_ca_tmp ).
  om_data_cc  = create_cc_file( om_data_ca_tmp ).

  CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
    EXPORTING
      i_line_header        = 'X'
    TABLES
      i_tab_sap_data       = om_data
    CHANGING
      i_tab_converted_data = om_data_conv_tmp.

  APPEND get_header_line( ) TO om_data_conv.
  APPEND LINES OF om_data_conv_tmp TO om_data_conv.
ENDMETHOD.


METHOD proceed_om_data.
  get_om_data( ).
  om_data = map_om_data( IMPORTING om_data_mig = om_data_mig
                                   om_data_cc  = om_data_cc
                                   om_data_ca  = om_data_ca ).
ENDMETHOD.
ENDCLASS.
