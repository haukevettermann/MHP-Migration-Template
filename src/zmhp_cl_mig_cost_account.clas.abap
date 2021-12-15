class ZMHP_CL_MIG_COST_ACCOUNT definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_COST_ACCOUNT_DATA type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants COST_ACCOUNT_DATA type STRING value 'CostAllocationAccountV3' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_COST_ACCOUNT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_COST_ACCOUNT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
protected section.
private section.

  data MAPPING_FIELDS_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  constants SEGMENT4 type ZMHP_DD_FIELD value 'SEGMENT4' ##NO_TEXT.
  constants SOURCE_SUB_TYPE type STRING value 'COST' ##NO_TEXT.
  constants LEG_DATA_GROUP_NAME type STRING value 'LDG' ##NO_TEXT.
  constants SOURCE_TYPE type STRING value 'ASG' ##NO_TEXT.
  constants ACCOUNT type STRING value 'Account' ##NO_TEXT.
  data P0001 type P0001_TAB .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data COGU type BOOLEAN .

  methods MAP_MIG_VALUES
    importing
      !P0001 type P0001
    exporting
      !COMPANY_CODE type ZMHP_DD_VALUE .
  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods MAP_COGL_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS ZMHP_CL_MIG_COST_ACCOUNT IMPLEMENTATION.


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
    vp_cost_account_data = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                    ( name = 2  value = cost_account_data )
                                    ( name = 3  value = 'SourceSubType' )
                                    ( name = 4  value = 'SourceSystemId' )
                                    ( name = 5  value = 'SourceSystemOwner' )
                                    ( name = 6  value = 'SubTypeSequence' )
                                    ( name = 6  value = 'LegislativeDataGroupName' )
                                    ( name = 7  value = 'EffectiveDate' )
                                    ( name = 8  value = 'SourceType' )
                                    ( name = 9  value = 'AssignmentId(SourceSystemId)' )
                                    ( name = 10 value = 'Proportion' )
                                    ( name = 11 value = 'Segment1' )
                                    ( name = 12 value = 'Segment2' )
                                    ( name = 13 value = 'Segment3' )
                                    ( name = 14 value = 'Segment4' )
                                    ( name = 15 value = 'Segment5' ) ).
  ELSEIF cofu EQ abap_true.
    vp_cost_account_data = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                    ( name = 2  value = cost_account_data )
                                    ( name = 3  value = 'SourceSubType' )
                                    ( name = 4  value = 'SourceSystemId' )
                                    ( name = 5  value = 'SourceSystemOwner' )
                                    ( name = 6  value = 'SubTypeSequence' )
                                    ( name = 6  value = 'LegislativeDataGroupName' )
                                    ( name = 7  value = 'EffectiveDate' )
                                    ( name = 8  value = 'SourceType' )
                                    ( name = 9  value = 'AssignmentId(SourceSystemId)' )
                                    ( name = 10 value = 'Proportion' )
                                    ( name = 11 value = 'Segment1' )
                                    ( name = 12 value = 'Segment2' )
                                    ( name = 13 value = 'Segment3' )
                                    ( name = 14 value = 'Segment4' )
                                    ( name = 15 value = 'Segment5' ) ).
  ENDIF.

ENDMETHOD.


METHOD create_metadata.
  DESCRIBE TABLE vp_cost_account_data LINES DATA(length).

  LOOP AT vp_cost_account_data ASSIGNING FIELD-SYMBOL(<cost_account_data>).

    "set METADATA title
    CASE <cost_account_data>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <cost_account_data>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.
ENDMETHOD.


METHOD GET_COFU_DATA.

  "Get BUKRS for LegislationCode
  SELECT pernr,
         begda,
         endda,
         bukrs,
         kostl INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs by low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).
ENDMETHOD.


METHOD GET_COGL_DATA.

  "Get BUKRS for LegislationCode
  SELECT pernr,
         begda,
         endda,
         bukrs,
         kostl INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs by low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).
ENDMETHOD.


METHOD GET_MAPPING_FIELDS.

  "get mapping fields for sex
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>bukrs
                                                   oracle_field = segment4
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_bukrs ).
ENDMETHOD.


METHOD GET_MAPPING_VALUES.

  "get mapping values for sex
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>bukrs
                                                   oracle_field = segment4
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id         TYPE string,
        sys_id         TYPE string,
        leg_grp_name   TYPE string,
        pernr_tmp      TYPE pernr_d,
        pernr_sequence TYPE string,
        src_asn_id     TYPE string,
        count          TYPE int8,
        kostl          TYPE string,
        land1          TYPE /iwbep/s_mgw_name_value_pair.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).

    count = count + 1.
    IF pernr_tmp IS INITIAL OR
       pernr_tmp NE <p0001>-pernr.
      count = 1.
      pernr_tmp = <p0001>-pernr.
    ENDIF.

**JMB20210811 start insetr - check for worker entry
*
    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I
*JMB20210811 end insert

    "get legislationcode
    CLEAR: land1, leg_grp_name.
    READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.

    CONCATENATE land1-value leg_data_group_name INTO leg_grp_name SEPARATED BY space.

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).

    map_mig_values( EXPORTING p0001 = <p0001>
                    IMPORTING company_code = DATA(company_code) ).

    "get source id
    CONCATENATE zmhp_cl_mig_utils=>assign <p0001>-pernr INTO src_asn_id.

    CONCATENATE src_asn_id '_' account INTO src_id.
    pernr_sequence = CONV #( count ).
    CONDENSE pernr_sequence.

    CLEAR kostl.
    IF <p0001>-kostl IS NOT INITIAL.
      CONCATENATE <p0001>-bukrs '-' <p0001>-kostl INTO kostl.
    ENDIF.

    CONCATENATE zmhp_cl_mig_utils=>merge
                cost_account_data
                source_sub_type
                src_id
                sys_id
                pernr_sequence
                leg_grp_name
                begda_tmp
                source_type
                src_asn_id
                '1'
                <p0001>-bukrs
                kostl
                ''
                company_code
                ''
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: src_id         TYPE string,
        sys_id         TYPE string,
        leg_grp_name   TYPE string,
        pernr_tmp      TYPE pernr_d,
        pernr_sequence TYPE string,
        src_asn_id     TYPE string,
        pernr_old      TYPE rsdsselopt_t,
        count          TYPE int8,
        kostl          TYPE string,
        land1          TYPE /iwbep/s_mgw_name_value_pair.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
    "JMB20210811 I - Pass only one cost center, due to not date-effective
    CHECK <p0001>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    count = count + 1.
    IF pernr_tmp IS INITIAL OR
       pernr_tmp NE <p0001>-pernr.
      count = 1.
      pernr_tmp = <p0001>-pernr.
    ENDIF.

    "get legislationcode
    CLEAR: land1, leg_grp_name.
    READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.

    CONCATENATE land1-value leg_data_group_name INTO leg_grp_name SEPARATED BY space.

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).

    map_mig_values( EXPORTING p0001 = <p0001>
                    IMPORTING company_code = DATA(company_code) ).

    "get source id
    CONCATENATE zmhp_cl_mig_utils=>assign <p0001>-pernr INTO src_asn_id.

    CONCATENATE src_asn_id '_' account INTO src_id.
    pernr_sequence = CONV #( count ).
    CONDENSE pernr_sequence.

    CLEAR kostl.
    IF <p0001>-kostl IS NOT INITIAL.
      CONCATENATE <p0001>-bukrs '-' <p0001>-kostl INTO kostl.
    ENDIF.

    CONCATENATE zmhp_cl_mig_utils=>merge
                cost_account_data
                source_sub_type
                src_id
                sys_id
                pernr_sequence
                leg_grp_name
                begda_tmp
                source_type
                src_asn_id
                '1'
                <p0001>-bukrs
                kostl
                ''
                company_code
                ''
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_old.
  ENDLOOP.
ENDMETHOD.


METHOD MAP_MIG_VALUES.
  DATA: value_tmp TYPE zmhp_dd_value.

  "Process GESCH mapping
  value_tmp = CONV #( p0001-bukrs ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0001
      field_sap      = zmhp_cl_mig_utils=>bukrs
      field_oracle   = segment4
      mapping_fields = CONV #( mapping_fields_bukrs )
      mapping_values = CONV #( mapping_values_bukrs )
   CHANGING
     value           = value_tmp ).

  company_code       = value_tmp.
ENDMETHOD.


METHOD PROCEED_COFU_COST_ACCOUNT.
  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD PROCEED_COGL_COST_ACCOUNT.
  get_cogl_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  data = map_cogl_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
