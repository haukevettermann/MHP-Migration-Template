class ZMHP_CL_MIG_PERSON_VISA definition
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
  data VP_PERSON_VISA_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_VISA type STRING value 'PersonVisa' ##NO_TEXT.
  data P0094 type ZMHP_P0094_TAB .
  data VISA type STRING value 'VISA' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants VISAPERMITTYPE type ZMHP_DD_FIELD value 'VISAPERMITTYPE' ##NO_TEXT.

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
  methods PROCEED_COFU_PER_VISA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
protected section.
private section.

  data MAPPING_FIELDS_FPNCD type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_FPNCD type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_MIG_VALUES
    importing
      !P0094 type PA0094
    exporting
      !FPNCD type ZMHP_DD_VALUE .
ENDCLASS.



CLASS ZMHP_CL_MIG_PERSON_VISA IMPLEMENTATION.


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

      vp_person_visa_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                          ( name = 2  value = person_visa )
                                          ( name = 3  value = 'SourceSystemOwner' )
                                          ( name = 4  value = 'SourceSystemId' )
                                          ( name = 5  value = 'EffectiveStartDate' )
                                          ( name = 6  value = 'EffectiveEndDate' )
                                          ( name = 7  value = 'PersonId(SourceSystemId)' )
                                          ( name = 8  value = 'LegislationCode' )
                                          ( name = 9  value = 'VisaPermitType' )
                                          ( name = 10 value = 'EntryDate' )
                                          ( name = 11 value = 'ExpirationDate' )
                                          ( name = 12 value = 'CurrentVisaPermit' )
                                          ( name = 13 value = 'IssueDate' )
                                          ( name = 14 value = 'IssuingAuthority' )
                                          ( name = 15 value = 'IssuingCountry' )
                                          ( name = 16 value = 'IssuingLocation' )
                                          ( name = 17 value = 'Profession' )
                                          ( name = 18 value = 'VisaPermitCategory' )
                                          ( name = 19 value = 'VisaPermitNumber' )
                                          ( name = 20 value = 'VisaPermitStatus' )
                                          ( name = 21 value = 'VisaPermitStatusDate' ) ).

    ELSEIF cogu EQ abap_true.

    ENDIF.
  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_person_visa_structure LINES DATA(length).

    LOOP AT vp_person_visa_structure ASSIGNING FIELD-SYMBOL(<person_visa_struc>).

      "set METADATA title
      CASE <person_visa_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_visa_struc>-value INTO metadata.

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
           natio INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                                         AND begda LE @endda
                                                                         AND endda GE @begda.

    " read infotype 0094
    SELECT pernr,
           begda,
           endda,
           fpncd,
           expid,
           date2 INTO CORRESPONDING FIELDS OF TABLE @p0094 FROM pa0094 WHERE pernr IN @pernr
                                                                         AND begda LE @endda
                                                                         AND endda GE @begda
                                                                         AND fpncd EQ '12'. "only VISA are relevant
  ENDMETHOD.


METHOD get_mapping_fields.

  "get mapping fields for anssa
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = '0094'
                                                   sap_field    = 'FPNCD'
                                                   oracle_field = visapermittype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_fpncd ).

ENDMETHOD.


METHOD get_mapping_values.

  "get mapping values for anssa
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0094'
                                                   sap_field    = 'FPNCD'
                                                   oracle_field = visapermittype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_fpncd ).


ENDMETHOD.


METHOD map_cofu_data.

  DATA: sys_id TYPE string,
        src_id TYPE string.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0094 ASSIGNING FIELD-SYMBOL(<p0094>).

    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p0094>-pernr
                                                    AND begda LE endda
                                                    AND endda GE begda.
      EXIT.
    ENDLOOP.

    CHECK <p0002> IS ASSIGNED.

    CONCATENATE visa          <p0002>-pernr INTO src_id              SEPARATED BY '_'.
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr     = <p0002>-pernr
                                                      begda     = <p0002>-begda
                                                      endda     = <p0002>-endda
                                                      vp_src_id = vp_src_id ).

    map_mig_values( EXPORTING p0094 = <p0094>
                    IMPORTING fpncd = DATA(visa_per_type) ).

    DATA(expiration_date) = COND string( WHEN <p0094>-expdt IS NOT INITIAL
                                         THEN zmhp_cl_mig_utils=>convert_date( <p0094>-expdt )
                                         ELSE '' ).

    DATA(issue_date) = COND string( WHEN <p0094>-date2 IS NOT INITIAL
                                    THEN zmhp_cl_mig_utils=>convert_date( <p0094>-date2 )
                                    ELSE '' ).

    DATA(eff_start_date) = COND string( WHEN <p0094>-begda IS NOT INITIAL
                                        THEN zmhp_cl_mig_utils=>convert_date( <p0094>-begda )
                                        ELSE '' ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                person_visa
                sys_id
                src_id
                eff_start_date
                '' " EffectiveEndDate
                src_sys_id
                <p0002>-natio
                visa_per_type
                '' " EntryDate
                expiration_date " ExpirationDate
                'Y' " CurrentVisaPermit
                issue_date "IssueDate
                '' " IssuingAuthority
                '' " IssuingCountry
                '' " IssuingLocation
                '' " Profession
                '' " VisaPermitCategory
                '' " VisaPermitNumber
                '' " VisaPermitStatus FUSTION LOOKUP " TODO MIGRATION
                '' " VisaPermitStatusDate
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.

ENDMETHOD.


METHOD MAP_MIG_VALUES.

  DATA: value_tmp TYPE zmhp_dd_value.

  "Process SUBTY mapping
  value_tmp = CONV #( p0094-fpncd ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = '0094'
      field_sap      = 'FPNCD'
      field_oracle   = visapermittype
      mapping_fields = CONV #( mapping_fields_fpncd )
      mapping_values = CONV #( mapping_values_fpncd )
    CHANGING
      value          = value_tmp ).
  fpncd = value_tmp.
ENDMETHOD.


METHOD proceed_cofu_per_visa.
  get_cofu_data( ).

  get_mapping_fields( ).
  get_mapping_values( ).

  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0002 = p0002 ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
