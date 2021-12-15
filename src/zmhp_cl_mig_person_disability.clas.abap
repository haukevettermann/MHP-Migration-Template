class ZMHP_CL_MIG_PERSON_DISABILITY definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data COGU type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0002 type P0002_TAB .
  data VP_PERSON_DISABILITY_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data P0004 type P0004_TAB .
  constants PERSON_DISABILITY type STRING value 'PersonDisability' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_PER_DISABILITY
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

  data MAPPING_FIELDS_SBGRU type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_SBGRU type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods GET_MAPPING_COFU_FIELDS .
  methods GET_MAPPING_COFU_VALUES .
  methods MAP_MIG_VALUES
    importing
      !P0004 type P0004
    exporting
      !SBGRU type ZMHP_DD_VALUE .
ENDCLASS.



CLASS ZMHP_CL_MIG_PERSON_DISABILITY IMPLEMENTATION.


  METHOD constructor.

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cogl EQ abap_true.

    ELSEIF cofu EQ abap_true.
      vp_person_disability_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge  )
                                                ( name = 2  value = person_disability )
                                                ( name = 3  value = 'DisabilityCode' )
                                                ( name = 4  value = 'EffectiveStartDate' )
                                                ( name = 5  value = 'EffectiveEndDate' )
                                                ( name = 6  value = 'Category' )
                                                ( name = 7  value = 'QuotaFTE' )
                                                ( name = 8  value = 'LegislationCode' )
                                                ( name = 9  value = 'PersonNumber' )
                                                ( name = 10 value = 'Reason' )
                                                ( name = 11 value = 'SelfDisclosedType' )
                                                ( name = 12 value = 'Status' ) ).
    ELSEIF cogu EQ abap_true.

    ENDIF.
  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_person_disability_structure LINES DATA(length).

    LOOP AT vp_person_disability_structure ASSIGNING FIELD-SYMBOL(<person_disability_struc>).

      "set METADATA title
      CASE <person_disability_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_disability_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_cofu_data.
    " read infotype 0002
    SELECT pernr,
           begda,
           endda,
           natio
           INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                                   AND begda LE @endda
                                                                   AND endda GE @begda.
    " read infotype 0004
    SELECT pernr,
           begda,
           endda,
           sbgru
           INTO CORRESPONDING FIELDS OF TABLE @p0004 FROM pa0004 WHERE pernr IN @pernr
                                                                   AND begda LE @endda
                                                                   AND endda GE @begda.

  ENDMETHOD.


  METHOD get_mapping_cofu_fields.

    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga          = molga
                                                     infty          = zmhp_cl_mig_utils=>it0004
                                                     sap_field      = zmhp_cl_mig_utils=>sbgru
                                                     oracle_field   = zmhp_cl_mig_utils=>disability_category
                                                     export         = abap_true
                                           IMPORTING mapping_fields = mapping_fields_sbgru ).


  ENDMETHOD.


  METHOD get_mapping_cofu_values.

    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga          = molga
                                                     infty          = zmhp_cl_mig_utils=>it0021
                                                     sap_field      = zmhp_cl_mig_utils=>sbgru
                                                     oracle_field   = zmhp_cl_mig_utils=>disability_category
                                                     export         = abap_true
                                           IMPORTING mapping_values = mapping_values_sbgru ).




  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT p0004 ASSIGNING FIELD-SYMBOL(<p0004>).
      LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p0004>-pernr
                                                      AND begda LE endda
                                                      AND endda GE begda.
        EXIT.
      ENDLOOP.

      CHECK <p0002> IS ASSIGNED.

      CHECK sy-subrc IS INITIAL.

      DATA(start_date) = zmhp_cl_mig_utils=>convert_date( <p0004>-begda ).
      DATA(end_date)   = zmhp_cl_mig_utils=>convert_date( <p0004>-endda ).


      DATA(natio) = COND string( WHEN <p0002>-natio IS INITIAL THEN ''
                                 ELSE <p0002>-natio ).


      map_mig_values( EXPORTING p0004 = <p0004>
                      IMPORTING sbgru = DATA(sbgru) ).


      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_disability
                  ''                        " DisabilityCode MIGRATION TODO
                  start_date                " EffectiveStartDate
                  ''                        " EffectiveEndDate
                  sbgru                     " Category
                  '1'                       " Quota FTE
                  natio                     " Legislation Code
                  <p0004>-pernr             " PersonNumber  "oraclepernr
                  ''                        " Reason MIGRATION TODO
                  ''                        " SelfDisclosedType MIGRATION TODO
                  'A'                       " Status MIGRATION TODO after clarification
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.


  ENDMETHOD.


  METHOD map_mig_values.

    DATA: value_tmp TYPE zmhp_dd_value.

    value_tmp = CONV #( p0004-sbgru ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0004
        field_sap      = zmhp_cl_mig_utils=>sbgru
        field_oracle   = zmhp_cl_mig_utils=>disability_category
        mapping_fields = CONV #( mapping_fields_sbgru )
        mapping_values = CONV #( mapping_values_sbgru )
      CHANGING
        value          = value_tmp ).

    sbgru = value_tmp.


  ENDMETHOD.


  METHOD proceed_cofu_per_disability.

    get_cofu_data( ).
    get_mapping_cofu_values( ).
    get_mapping_cofu_fields( ).
*    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
*                                           CHANGING p0002 = p0002 ).
    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
