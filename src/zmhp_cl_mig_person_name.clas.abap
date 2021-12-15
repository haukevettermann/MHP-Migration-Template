class ZMHP_CL_MIG_PERSON_NAME definition
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
  data VP_PERSON_NAME_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_NAME type STRING value 'PersonName' ##NO_TEXT.
  constants NAM type STRING value 'NAM_' ##NO_TEXT.
  constants GLOBAL type STRING value 'GLOBAL' ##NO_TEXT.
  constants TITLE type ZMHP_DD_FIELD value 'TITLE' ##NO_TEXT.
  constants NAMEINFORMATION1 type ZMHP_DD_FIELD value 'NAMEINFORMATION1' ##NO_TEXT.
  constants SAP_TITEL type ZMHP_DD_FIELD value 'TITEL' ##NO_TEXT.
  constants ANRED type ZMHP_DD_FIELD value 'ANRED' ##NO_TEXT.

  methods PROCEED_COFU_PERSON_NAME
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_PERSON_NAME
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

  data MAPPING_FIELDS_TITLE type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_FIELDS_NAMEINFO1 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_TITLE type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_VALUES_NAMEINFO1 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data P0001 type P0001_TAB .
  data COGU type BOOLEAN .
  data MAPPING_FIELDS_NAMEINFO7 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_NAMEINFO7 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .

  methods MAP_MIG_VALUES
    importing
      !P0002 type P0002
    exporting
      !TITLE type ZMHP_DD_VALUE
      !ANRED type ZMHP_DD_VALUE
      !NAMEINFO7 type ZMHP_DD_VALUE .
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



CLASS ZMHP_CL_MIG_PERSON_NAME IMPLEMENTATION.


METHOD constructor.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.

  IF cogl EQ abap_true.
    vp_person_name_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                        ( name = 2  value = person_name )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'PersonId(SourceSystemId)' )
                                        ( name = 6  value = 'EffectiveStartDate' )
                                        ( name = 7  value = 'EffectiveEndDate' )
                                        ( name = 8  value = 'LegislationCode' )
                                        ( name = 9  value = 'NameType' )
                                        ( name = 10 value = 'FirstName' )
                                        ( name = 11 value = 'LastName' )
                                        ( name = 12 value = 'KnownAs' )
                                        ( name = 13 value = 'Suffix' )
                                        ( name = 14 value = 'Title' )
                                        ( name = 15 value = 'NameInformation1' )
                                        ( name = 16 value = 'CharSetContext' ) ).
  ELSEIF cofu EQ abap_true OR
         cogu EQ abap_true.
    vp_person_name_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                        ( name = 2  value = person_name )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'PersonId(SourceSystemId)' )
                                        ( name = 6  value = 'EffectiveStartDate' )
                                        ( name = 7  value = 'EffectiveEndDate' )
                                        ( name = 8  value = 'LegislationCode' )
                                        ( name = 9  value = 'NameType' )
                                        ( name = 10 value = 'FirstName' )
                                        ( name = 11 value = 'LastName' )
                                        ( name = 12 value = 'KnownAs' )
                                        ( name = 13 value = 'Suffix' )
                                        ( name = 14 value = 'Title' )
                                        ( name = 15 value = 'CharSetContext' )
                                        ( name = 16 value = 'NameInformation1' )
                                        ( name = 17 value = 'NameInformation2' )
                                        ( name = 18 value = 'NameInformation3' )
                                        ( name = 19 value = 'NameInformation4' )
                                        ( name = 20 value = 'NameInformation5' )
                                        ( name = 21 value = 'NameInformation6' )
                                        ( name = 22 value = 'NameInformation7' )
                                        ( name = 23 value = 'PreNameAdjunct' ) ).
  ENDIF.
ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_person_name_structure LINES DATA(length).

    LOOP AT vp_person_name_structure ASSIGNING FIELD-SYMBOL(<person_name_struc>).

      "set METADATA title
      CASE <person_name_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_name_struc>-value INTO metadata.

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
         nachn,
         vorna,
         name2,
         rufnm,
         anred,
         titel,
         fnamr,
         lnamr,
         vorsw,
         inits,
         vors2,
         knznm,
         titl2 INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "get BUKRS for LegislationCode
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


METHOD get_cogl_data.
  "Get IT0002
  SELECT pernr,
         begda,
         endda,
         sprsl,
         nachn,
         vorna,
         rufnm,
         anred,
         titel,
         fnamr,
         lnamr INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "get BUKRS for LegislationCode
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

  "get mapping fields for title
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = anred
                                                   oracle_field = title
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_title ).

  "get mapping fields for anred
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = sap_titel
                                                   oracle_field = nameinformation1
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_nameinfo1 ).

  "get mapping fields for anred
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = 'KNZNM'
                                                   oracle_field = 'NAMEINFORMATION7'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_nameinfo7 ).
ENDMETHOD.


METHOD get_mapping_values.

  "get mapping values for anred
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = anred
                                                   oracle_field = title
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_title ).

  "get mapping values for titel
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = sap_titel
                                                   oracle_field = nameinformation1
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_nameinfo1 ).

  "get mapping fields for anred
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0002
                                                   sap_field    = 'KNZNM'
                                                   oracle_field = 'NAMEINFORMATION7'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_nameinfo7 ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: language    TYPE string,
        src_id      TYPE string,
        sys_id      TYPE string,
        suffix      TYPE string,
        nameinfo1   TYPE string,
        nameinfo2   TYPE string,
        nameinfo3   TYPE string,
        nameinfo4   TYPE string,
        nameinfo5   TYPE string,
        nameinfo6   TYPE string,
        nameinfo7   TYPE string,
        nameadjunct TYPE string,
        land1       TYPE /iwbep/s_mgw_name_value_pair.

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

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0002>-pernr
                                                      begda = <p0002>-begda
                                                      endda = <p0002>-endda
                                                      vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I

    CONCATENATE nam <p0002>-pernr '_' global INTO src_id.

    map_mig_values( EXPORTING p0002 = <p0002>
                    IMPORTING title = DATA(title_tmp)
                              anred = DATA(anred_tmp)
                              nameinfo7 = DATA(nameinfo7_tmp) ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                person_name
                src_id
                sys_id
                src_sys_id
                begda_tmp
                endda_tmp
                land1-value
                global
                <p0002>-vorna
                <p0002>-nachn
                <p0002>-rufnm
                suffix
                anred_tmp
                ''
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    "check for SAP mandant
    CASE sy-mandt.
      WHEN zmhp_cl_int_constants=>cofu_mandant-germany.
        nameinfo3 = <p0002>-name2.
        nameinfo1 = title_tmp.
      WHEN zmhp_cl_int_constants=>cofu_mandant-france.
        nameinfo1 = <p0002>-name2.
      WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands.
        nameadjunct = <p0002>-vorsw.
        nameinfo1   = <p0002>-inits.
        nameinfo5   = <p0002>-name2.
        nameinfo6   = <p0002>-vors2.
        nameinfo7   = nameinfo7_tmp.
        nameinfo2   = title_tmp.
        TRANSLATE nameadjunct TO UPPER CASE. "JMB20211011 I - C400129651-5856
        TRANSLATE nameinfo6   TO UPPER CASE. "JMB20211015 I - C400129651-5856
      WHEN zmhp_cl_int_constants=>cofu_mandant-austria OR
           zmhp_cl_int_constants=>cofu_mandant-italy.
        nameinfo1   = title_tmp.

        "in case of austria pass second title
        IF '03' IN molga.
          nameinfo2   = <p0002>-titl2.
        ENDIF.
    ENDCASE.

    CONCATENATE data_tmp
                nameinfo1
                nameinfo2
                nameinfo3
                nameinfo4
                nameinfo5
                nameinfo6
                nameinfo7
                nameadjunct
    INTO        data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    CLEAR: nameinfo1, nameinfo2, nameinfo3, nameinfo4, nameinfo5, nameinfo6, nameinfo7, nameadjunct.

    "local names
    IF <p0002>-fnamr IS NOT INITIAL OR
       <p0002>-lnamr IS NOT INITIAL.
      CONCATENATE nam <p0002>-pernr '_' land1-value INTO src_id.
      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_name
                  src_id
                  sys_id
                  src_sys_id
                  begda_tmp
                  endda_tmp
                  land1-value
                  land1-value
                  <p0002>-fnamr
                  <p0002>-lnamr
                  <p0002>-rufnm
                  suffix
                  anred_tmp
                  land1-value
                  nameinfo1
                  nameinfo2
                  nameinfo3
                  nameinfo4
                  nameinfo5
                  nameinfo6
                  nameinfo7
                  nameadjunct
      INTO DATA(data_lclname) SEPARATED BY zmhp_cl_mig_utils=>separator.
      CONCATENATE data_tmp
                  cl_abap_char_utilities=>newline
                  data_lclname
      INTO        data_tmp.
    ENDIF.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


  METHOD map_cogl_data.

    DATA: language TYPE string,
          src_id   TYPE string,
          sys_id   TYPE string,
          suffix   TYPE string,
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

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0002>-pernr
                                                        begda = <p0002>-begda
                                                        endda = <p0002>-endda
                                                        vp_src_id = vp_src_id ).

      CONCATENATE nam <p0002>-pernr '_' global INTO src_id.

      map_mig_values( EXPORTING p0002 = <p0002>
                      IMPORTING title = DATA(title_tmp)
                                anred = DATA(anred_tmp) ).

      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_name
                  src_id
                  sys_id
                  src_sys_id
                  begda_tmp
                  endda_tmp
                  land1-value
                  global
                  <p0002>-vorna
                  <p0002>-nachn
                  <p0002>-rufnm
                  suffix
                  anred_tmp
                  title_tmp
                  ''
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      "local names
      IF <p0002>-fnamr IS NOT INITIAL OR
         <p0002>-lnamr IS NOT INITIAL.
        CONCATENATE nam <p0002>-pernr '_' land1-value INTO src_id.

        "set CharSetContext for Korea, Japan, China and Ukraine
        DATA(charset) = land1-value.
        CASE charset.
          WHEN 'KR'.
            charset = 'KO'.
          WHEN 'JP'.
            charset = 'ZHS'.
          WHEN 'CN'.
            charset = 'ZHS'.
          WHEN 'UA'.
            charset = 'RU'.
        ENDCASE.

        CONCATENATE zmhp_cl_mig_utils=>merge
                    person_name
                    src_id
                    sys_id
                    src_sys_id
                    begda_tmp
                    endda_tmp
                    land1-value
                    land1-value
                    <p0002>-fnamr
                    <p0002>-lnamr
                    <p0002>-rufnm
                    suffix
                    anred_tmp
                    title_tmp
                    charset
        INTO DATA(data_lclname) SEPARATED BY zmhp_cl_mig_utils=>separator.
        CONCATENATE data_tmp
                    cl_abap_char_utilities=>newline
                    data_lclname
        INTO        data_tmp.
      ENDIF.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    ENDLOOP.
  ENDMETHOD.


METHOD map_mig_values.
  DATA: value_tmp TYPE zmhp_dd_value.

  "Process TITLE mapping
  value_tmp = CONV #( p0002-anred ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0002
      field_sap      = zmhp_cl_mig_person_name=>anred
      field_oracle   = zmhp_cl_mig_person_name=>title
      mapping_fields = CONV #( mapping_fields_title )
      mapping_values = CONV #( mapping_values_title )
    CHANGING
      value          = value_tmp ).

  anred              = value_tmp.

  "Process ANRED mapping
  value_tmp = CONV #( p0002-titel ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0002
      field_sap      = zmhp_cl_mig_person_name=>sap_titel
      field_oracle   = nameinformation1
      mapping_fields = CONV #( mapping_fields_nameinfo1 )
      mapping_values = CONV #( mapping_values_nameinfo1 )
    CHANGING
      value          = value_tmp ).

  title              = value_tmp.

  "Process ANRED mapping
  value_tmp = CONV #( p0002-knznm ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0002
      field_sap      = 'KNZNM'
      field_oracle   = 'NAMEINFORMATION7'
      mapping_fields = CONV #( mapping_fields_nameinfo7 )
      mapping_values = CONV #( mapping_values_nameinfo7 )
    CHANGING
      value          = value_tmp ).

  nameinfo7 = value_tmp.
ENDMETHOD.


METHOD PROCEED_COFU_PERSON_NAME.
  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0001 = p0001
                                                  p0002 = p0002 ).
  zmhp_cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD proceed_cogl_person_name.
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
