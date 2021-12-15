class ZMHP_CL_MIG_PERSON_LEG_DATA definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0002 type P0002_TAB .
  data VP_PERSON_LEG_DATA type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_LEG_DATA type STRING value 'PersonLegislativeData' ##NO_TEXT.
  constants LEG type STRING value 'LEG_' ##NO_TEXT.
  constants GESCH type ZMHP_DD_FIELD value 'GESCH' ##NO_TEXT.
  constants SEX type ZMHP_DD_FIELD value 'SEX' ##NO_TEXT.
  data P0022 type ZMHP_P0022_TAB .
  constants PER type STRING value 'PER_' ##NO_TEXT.
  constants MARITALSTATUS type ZMHP_DD_FIELD value 'MARITALSTATUS' ##NO_TEXT.

  methods PROCEED_COFU_PERSON_LEG_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_PERSON_LEG_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  PROTECTED SECTION.
private section.

  data MAPPING_FIELDS_GESCH type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_GESCH type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data P0001 type P0001_TAB .
  data COGU type BOOLEAN .
  data MAPPING_FIELDS_FAMST type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_FAMST type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .

  methods MAP_MIG_VALUES
    importing
      !P0002 type P0002
    exporting
      !GESCH type ZMHP_DD_VALUE
      !FAMST type ZMHP_DD_VALUE .
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



CLASS ZMHP_CL_MIG_PERSON_LEG_DATA IMPLEMENTATION.


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
      vp_person_leg_data = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                    ( name = 2  value = person_leg_data )
                                    ( name = 3  value = 'SourceSystemId' )
                                    ( name = 4  value = 'SourceSystemOwner' )
                                    ( name = 5  value = 'PersonId(SourceSystemId)' )
                                    ( name = 6  value = 'EffectiveStartDate' )
                                    ( name = 7  value = 'EffectiveEndDate' )
                                    ( name = 8  value = 'LegislationCode' )
                                    ( name = 10 value = 'Sex' ) ).
    ELSEIF cofu EQ abap_true.
      vp_person_leg_data = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                    ( name = 2  value = person_leg_data )
                                    ( name = 3  value = 'SourceSystemOwner' )
                                    ( name = 4  value = 'SourceSystemId' )
                                    ( name = 5  value = 'EffectiveStartDate' )
                                    ( name = 6  value = 'EffectiveEndDate' )
                                    ( name = 7  value = 'PersonId(SourceSystemId)' )
                                    ( name = 8  value = 'LegislationCode' )
                                    ( name = 9  value = 'HighestEducationLevel' )
                                    ( name = 10  value = 'MaritalStatus' )
                                    ( name = 11 value = 'MaritalStatusDate' )
                                    ( name = 12 value = 'Sex') ).
    ENDIF.

  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_person_leg_data LINES DATA(length).

    LOOP AT vp_person_leg_data ASSIGNING FIELD-SYMBOL(<person_leg_data>).

      "set METADATA title
      CASE <person_leg_data>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_leg_data>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


METHOD GET_COFU_DATA.

  "Get IT0002
  SELECT pernr,
         begda,
         endda,
         sprsl,
         gesch,
         famst,
         famdt INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get BUKRS for LegislationCode
  SELECT pernr,
         begda,
         endda,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  SELECT
         pernr,
         begda,
         endda,
         sland,
         slart INTO CORRESPONDING FIELDS OF TABLE @p0022 FROM pa0022 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs by low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).
ENDMETHOD.


METHOD get_cogl_data.

  "Get IT0002
  SELECT pernr,
         begda,
         endda,
         sprsl,
         gesch INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get BUKRS for LegislationCode
  SELECT pernr,
         begda,
         endda,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs by low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).
ENDMETHOD.


METHOD get_mapping_fields.

  "get mapping fields for sex
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = gesch
                                                   oracle_field = sex
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_gesch ).

  "get mapping fields for sex
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = 'FAMST'
                                                   oracle_field = maritalstatus
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_famst ).
ENDMETHOD.


METHOD get_mapping_values.

  "get mapping values for sex
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = gesch
                                                   oracle_field = sex
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_gesch ).

  "get mapping values for famst
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = 'FAMST'
                                                   oracle_field = maritalstatus
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_famst ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: language          TYPE string,
        src_id            TYPE string,
        sys_id            TYPE string,
        gender            TYPE string,
        land1             TYPE /iwbep/s_mgw_name_value_pair,
        highest_edu_level TYPE string.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

    "get legislationcode
    CLEAR land1.
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0002>-pernr AND
                                                        begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda.

      READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
      EXIT.
    ENDLOOP.

    "BS20211027 start insert
    LOOP AT p0022 ASSIGNING FIELD-SYMBOL(<p0022>) WHERE pernr EQ <p0002>-pernr AND
                                                        begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda.

      DATA(sland) = <p0022>-sland.
      DATA(slart) = <p0022>-slart.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CONCATENATE sland '_' slart INTO highest_edu_level.
    ELSE.
      highest_edu_level = ''.
    ENDIF.
    "BS20211027 end insert

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0002>-begda ).
    DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0002>-endda ).
    DATA(famdt_tmp) = COND string( WHEN <p0002>-famdt IS NOT INITIAL THEN zmhp_cl_mig_utils=>convert_date( <p0002>-famdt )
                                   ELSE '').

    map_mig_values( EXPORTING p0002 = <p0002>
                    IMPORTING gesch = DATA(gesch_tmp)
                              famst = DATA(famst) ).

    CONCATENATE leg <p0002>-pernr '_' land1-value INTO src_id.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0002>-pernr
                                                      begda = <p0002>-begda
                                                      endda = <p0002>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                person_leg_data
                sys_id
                src_id
                begda_tmp
                endda_tmp
                src_sys_id
                land1-value
                highest_edu_level
                famst
                famdt_tmp
                gesch_tmp
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: language TYPE string,
        src_id   TYPE string,
        sys_id   TYPE string,
        gender   TYPE string,
        land1    TYPE /iwbep/s_mgw_name_value_pair.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

    "get legislationcode
    CLEAR land1.
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0002>-pernr AND
                                                        begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda.

      READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
      EXIT.
    ENDLOOP.

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0002>-begda ).
    DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0002>-endda ).

    map_mig_values( EXPORTING p0002 = <p0002>
                    IMPORTING gesch = DATA(gesch_tmp) ).

    CONCATENATE leg <p0002>-pernr '_' land1-value INTO src_id.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0002>-pernr
                                                      begda = <p0002>-begda
                                                      endda = <p0002>-endda
                                                      vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I

    CONCATENATE zmhp_cl_mig_utils=>merge
                person_leg_data
                src_id
                sys_id
                src_sys_id
                begda_tmp
                endda_tmp
                land1-value
                gesch_tmp
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD map_mig_values.
  DATA: value_tmp TYPE zmhp_dd_value.

  "Process GESCH mapping
  value_tmp = CONV #( p0002-gesch ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0002
      field_sap      = zmhp_cl_mig_person_leg_data=>gesch
      field_oracle   = sex
      mapping_fields = CONV #( mapping_fields_gesch )
      mapping_values = CONV #( mapping_values_gesch )
   CHANGING
     value           = value_tmp ).

  gesch              = value_tmp.

  "Process GESCH mapping
  value_tmp = CONV #( p0002-famst ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0002
      field_sap      = 'FAMST'
      field_oracle   = maritalstatus
      mapping_fields = CONV #( mapping_fields_famst )
      mapping_values = CONV #( mapping_values_famst )
   CHANGING
     value           = value_tmp ).

  famst              = value_tmp.
ENDMETHOD.


METHOD PROCEED_COFU_PERSON_LEG_DATA.
  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0001 = p0001
                                                  p0002 = p0002 ).
  zmhp_cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD proceed_cogl_person_leg_data.
  get_cogl_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0001 = p0001
                                                  p0002 = p0002 ).
  zmhp_cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cogl_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
