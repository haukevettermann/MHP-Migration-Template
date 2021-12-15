class ZMHP_CL_MIG_PERSON_ADDRESS definition
  public
  create public .

public section.

  constants ADDRESSTYPE type ZMHP_DD_FIELD value 'ADDRESSTYPE' ##NO_TEXT.
  constants ANSSA type ZMHP_DD_FIELD value 'ANSSA' ##NO_TEXT.
  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0006 type P0006_TAB .
  data VP_PERSON_ADDRESS_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_ADDRESS type STRING value 'PersonAddress' ##NO_TEXT.
  constants ADDRESS type STRING value 'ADDR_' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_PERSON_ADDRESS
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
  data MAPPING_FIELDS_ANSSA type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_ANSSA type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_SUBTY type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_SUBTY type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .

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
      !P0006 type P0006
    exporting
      !SUBTY type ZMHP_DD_VALUE .
ENDCLASS.



CLASS ZMHP_CL_MIG_PERSON_ADDRESS IMPLEMENTATION.


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

    vp_person_address_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                          ( name = 2  value = person_address )
                                          ( name = 3  value = 'SourceSystemOwner' )
                                          ( name = 4  value = 'SourceSystemId' )
                                          ( name = 5  value = 'EffectiveStartDate' )
                                          ( name = 6  value = 'EffectiveEndDate' )
                                          ( name = 7  value = 'PersonId(SourceSystemId)' )
                                          ( name = 8  value = 'AddressType' )
                                          ( name = 9  value = 'AddressLine1' )
                                          ( name = 10 value = 'AddressLine2' )
                                          ( name = 11 value = 'AddressLine3' )
                                          ( name = 12 value = 'AddressLine4' )
                                          ( name = 13 value = 'TownOrCity' )
                                          ( name = 14 value = 'Country' )
                                          ( name = 15 value = 'Region3' )
                                          ( name = 16 value = 'PostalCode' )
                                          ( name = 17 value = 'PrimaryFlag' )
                                          ( name = 18 value = 'AddlAddressAttribute1' ) ).
  ENDIF.

  endmethod.


  method CREATE_METADATA.

    DESCRIBE TABLE vp_person_address_structure LINES DATA(length).

    LOOP AT vp_person_address_structure ASSIGNING FIELD-SYMBOL(<person_address_struc>).

      "set METADATA title
      CASE <person_address_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_address_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.

    ENDLOOP.

  endmethod.


  METHOD get_cofu_data.
    "get IT0006
    SELECT pernr,
           subty,
           begda,
           endda,
           anssa,
           stras,
           ort01,
           land1,
           state,
           pstlz,
           zzentkm
      FROM pa0006 INTO CORRESPONDING FIELDS OF TABLE @p0006 WHERE pernr IN @pernr AND
                                                                  begda LE @endda AND
                                                                  endda GE @begda.

  ENDMETHOD.


  METHOD get_mapping_fields.

    "get mapping fields for anssa
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0006
                                                     sap_field    = 'SUBTY'
                                                     oracle_field = addresstype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_subty ).

  ENDMETHOD.


  METHOD get_mapping_values.

    "get mapping values for anssa
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0006
                                                     sap_field    = 'SUBTY'
                                                     oracle_field = addresstype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_subty ).

  ENDMETHOD.


  method MAP_COFU_DATA.

    DATA: src_id  TYPE string,
          sys_id  TYPE string.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT p0006 ASSIGNING FIELD-SYMBOL(<p0006>).

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0006>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0006>-endda ).

      CONCATENATE address <p0006>-pernr INTO src_id.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0006>-pernr
                                                        begda = <p0006>-begda
                                                        endda = <p0006>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(entkm) = CONV string( <p0006>-zzentkm ).
      CONDENSE entkm.

      map_mig_values( EXPORTING p0006 = <p0006>
                      IMPORTING subty = DATA(subty) ).

      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_address
                  sys_id
                  src_id
                  begda_tmp
                  endda_tmp
                  src_sys_id
                  subty
                  <p0006>-stras
                  <p0006>-adr03
                  <p0006>-adr04
                  '' "AddressLine4 is NULL
                  <p0006>-ort01
                  <p0006>-land1
                  <p0006>-state
                  <p0006>-pstlz
                  'Y' "primary flag is Y
                  entkm
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.

  endmethod.


METHOD map_mig_values.

  DATA: value_tmp TYPE zmhp_dd_value.

  "Process SUBTY mapping
  value_tmp = CONV #( p0006-subty ).
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0006
      field_sap      = 'SUBTY'
      field_oracle   = addresstype
      mapping_fields = CONV #( mapping_fields_subty )
      mapping_values = CONV #( mapping_values_subty )
    CHANGING
      value          = value_tmp ).
  subty = value_tmp.
ENDMETHOD.


  method PROCEED_COFU_PERSON_ADDRESS.

    get_cofu_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    data = map_cofu_data( vp_src_id ).

  endmethod.
ENDCLASS.
