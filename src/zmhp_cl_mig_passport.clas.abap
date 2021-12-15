class ZMHP_CL_MIG_PASSPORT definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_PASSPORT_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_PASSPORT type STRING value 'PersonPassport' ##NO_TEXT.
  data P0094 type ZMHP_P0094_TAB .
  constants PASS type STRING value 'Pass_' ##NO_TEXT.
  data P0002 type P0002_TAB .
  constants PASSPORTTYPE type ZMHP_DD_FIELD value 'PASSPORTTYPE' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_PASSPORT
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

  data COGU type BOOLEAN .
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



CLASS ZMHP_CL_MIG_PASSPORT IMPLEMENTATION.


method CONSTRUCTOR.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogl  = cogl.
  me->cogu  = cogu.
  me->molga = molga.

  IF cogl EQ abap_true.
    " add later?
  ELSEIF cogu EQ abap_true.
    " add later?
  ELSEIF cofu EQ abap_true.

    vp_passport_structure = VALUE #( ( name = 1	  value = zmhp_cl_mig_utils=>merge )
                                     ( name = 2	  value = person_passport )
                                     ( name = 3	  value = 'SourceSystemOwner' )
                                     ( name = 4	  value = 'SourceSystemId' )
                                     ( name = 5	  value = 'PersonId(SourceSystemId)' )
                                     ( name = 6	  value = 'LegislationCode' )
                                     ( name = 7	  value = 'PassportType' )
                                     ( name = 8	  value = 'PassportNumber' )
                                     ( name = 9	  value = 'IssueDate' )
                                     ( name = 10  value = 'ExpirationDate' )
                                     ( name = 11  value = 'IssuingAuthority' )
                                     ( name = 12  value = 'IssuingCountry' )
                                     ( name = 13  value = 'IssuingLocation' ) ).
  ENDIF.

endmethod.


  method CREATE_METADATA.

    DESCRIBE TABLE vp_passport_structure LINES DATA(length).

    LOOP AT vp_passport_structure ASSIGNING FIELD-SYMBOL(<passport_structure>).

      "set METADATA title
      CASE <passport_structure>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <passport_structure>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.

    ENDLOOP.

  endmethod.


  method GET_COFU_DATA.

    "get IT0002
    SELECT pernr,
           begda,
           endda,
           natio INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                                         AND begda LE @endda
                                                                         AND endda GE @begda
                                                                         AND natio IS NOT NULL.

    "get IT0094
    SELECT pernr,
           begda,
           endda,
           fpncd,
           docn1,
           date1,
           auth1,
           expid FROM pa0094 INTO CORRESPONDING FIELDS OF TABLE @p0094 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda AND
                                                                             fpncd EQ '02'. "Only Passport relevant

  endmethod.


METHOD GET_MAPPING_FIELDS.

  "get mapping fields for anssa
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = '0094'
                                                   sap_field    = 'FPNCD'
                                                   oracle_field = passporttype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_fpncd ).

ENDMETHOD.


METHOD GET_MAPPING_VALUES.

  "get mapping values for anssa
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0094'
                                                   sap_field    = 'FPNCD'
                                                   oracle_field = passporttype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_fpncd ).

ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT p0094 ASSIGNING FIELD-SYMBOL(<p0094>).
      LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p0094>-pernr AND
                                                          begda LE <p0094>-endda AND
                                                          endda GE <p0094>-begda.
        EXIT.
      ENDLOOP.

      CONCATENATE pass <p0094>-pernr INTO src_id.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0094>-pernr
                                                        begda = <p0094>-begda
                                                        endda = <p0094>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(issue_date) = COND string( WHEN <p0094>-date1 IS NOT INITIAL
                                      THEN zmhp_cl_mig_utils=>convert_date( <p0094>-date1 )
                                      ELSE '' ).

      DATA(expiration_date) = COND string( WHEN <p0094>-expid IS NOT INITIAL
                                           THEN zmhp_cl_mig_utils=>convert_date( <p0094>-expid )
                                           ELSE '' ).

      map_mig_values( EXPORTING p0094 = <p0094>
                      IMPORTING fpncd = DATA(fpncd) ).

      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_passport
                  sys_id
                  src_id
                  src_sys_id
                  <p0002>-natio "LegislationCode
                  fpncd         "PassportType
                  <p0094>-docn1 "PassportNumber
                  issue_date
                  expiration_date
                  <p0094>-auth1 "IssuingAuthority
                  '' "IssuingCountry
                  '' "IssuingLocation
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
      field_oracle   = passporttype
      mapping_fields = CONV #( mapping_fields_fpncd )
      mapping_values = CONV #( mapping_values_fpncd )
    CHANGING
      value          = value_tmp ).
  fpncd = value_tmp.
ENDMETHOD.


METHOD proceed_cofu_passport.
  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
