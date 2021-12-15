class ZMHP_CL_MIG_CONTACT_RELATION definition
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
  data VP_CON_RELATIONSHIP_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_RELATIONSHIP type STRING value 'ContactRelationship' ##NO_TEXT.
  data P0021 type P0021_TAB .
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.
  constants REL type STRING value 'REL' ##NO_TEXT.
  constants FAMSA type ZMHP_DD_FIELD value 'FAMSA' ##NO_TEXT.
  data MAPPING_FIELDS_FAMSA type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_FAMSA type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_CON_RELATIONSHIP
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

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods UPDATE_BEGIN_DATE
    importing
      !P0002 type P0002_TAB
    changing
      !P0021 type P0021_TAB .
  methods GET_MAPPING_COFU_FIELDS .
  methods GET_MAPPING_COFU_VALUES .
  methods MAP_MIG_VALUES
    importing
      !P0021 type P0021
    exporting
      !FAMSA type ZMHP_DD_VALUE .
ENDCLASS.



CLASS ZMHP_CL_MIG_CONTACT_RELATION IMPLEMENTATION.


  METHOD constructor.

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogl  = cogl.
    me->cogu  = cogu.
    me->molga = molga.

    IF cogu EQ abap_true.

    ELSEIF cofu EQ abap_true.
      vp_con_relationship_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                               ( name = 2  value = contact_relationship )
                                               ( name = 3  value = 'EffectiveStartDate' )
                                               ( name = 4  value = 'EffectiveEndDate' )
                                               ( name = 5  value = 'PersonId(SourceSystemId)' )
                                               ( name = 6  value = 'PersonNumber' )
                                               ( name = 7  value = 'RelatedPersonId(SourceSystemId)' )
                                               ( name = 8  value = 'RelatedPersonNumber' )
                                               ( name = 9  value = 'ContactType' )
                                               ( name = 10 value = 'StatutoryDependent' )
                                               ( name = 11 value = 'EmergencyContactFlag' )
                                               ( name = 12 value = 'ExistingPerson' )
                                               ( name = 13 value = 'PersonalFlag' )
                                               ( name = 14 value = 'PrimaryContactFlag' )
                                               ( name = 15 value = 'SequenceNumber' )
                                               ( name = 16 value = 'SourceSystemOwner' )
                                               ( name = 17 value = 'SourceSystemId' ) ).
    ELSEIF cogl EQ abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_con_relationship_structure LINES DATA(length).

    LOOP AT vp_con_relationship_structure ASSIGNING FIELD-SYMBOL(<contact_relation_data>).

      "set METADATA title
      CASE <contact_relation_data>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_relation_data>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_cofu_data.


    " Read Infotype 0002
    SELECT pernr,
           begda,
           endda
    INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                            AND begda LE @endda
                                                            AND endda GE @begda.


    " Read Infotype 0021
    SELECT pernr,
           begda,
           endda,
           famsa,
           seqnr,
           emrgn,
           subty,
           objps,
           fgbdt,
           zz_telnr,
           zz_telnr2,
           zz_art
    INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                            AND begda LE @endda
                                                            AND endda GE @begda.

  ENDMETHOD.


  METHOD get_mapping_cofu_fields.


    " get mapping fields for ContactRelation
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga          = molga
                                                     infty          = zmhp_cl_mig_utils=>it0021
                                                     sap_field      = zmhp_cl_mig_contact_relation=>famsa
                                                     oracle_field   = zmhp_cl_mig_utils=>contact_type
                                                     export         = abap_true
                                           IMPORTING mapping_fields = mapping_fields_famsa ).


  ENDMETHOD.


  METHOD get_mapping_cofu_values.

    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga          = molga
                                                     infty          = zmhp_cl_mig_utils=>it0021
                                                     sap_field      = zmhp_cl_mig_contact_relation=>famsa
                                                     oracle_field   = zmhp_cl_mig_utils=>contact_type
                                                     export         = abap_true
                                           IMPORTING mapping_values = mapping_values_famsa ).


  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string.


    DATA(tmp_pernr) = p0021[ 1 ]-pernr.
    DATA(count) = 0.
    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

*      IF <p0021>-pernr NE tmp_pernr. " multiple entries for one pernr possible, reset counter if pernr changes
*        count = 0.
*      ELSE.
*        count = count + 1.
*      ENDIF.

      DATA(eff_start_date) = zmhp_cl_mig_utils=>convert_date( <p0021>-begda ).
*      DATA(count_str) = COND string( WHEN count EQ 0 THEN ''
*                                     ELSE CONV string( count ) ).

      CONCATENATE per
                  cont
                  <p0021>-pernr
*                  count_str
      INTO DATA(per_id) SEPARATED BY '_'.

      DATA(rel_per_id) = per && '_' && <p0021>-pernr.

      DATA(emergency_contact) = COND #( WHEN <p0021>-emrgn IS NOT INITIAL THEN 'Y'
                                        ELSE 'N' ).

      DATA(existing_person) = 'Y'. " TODO

      map_mig_values( EXPORTING p0021 = <p0021>
                      IMPORTING famsa  = DATA(contact_type) ).


      sys_id = 'SAP_' && sy-mandt.
      CONCATENATE per
                  cont
                  rel
                  <p0021>-pernr
      INTO src_id SEPARATED BY '_'.


      CONCATENATE zmhp_cl_mig_utils=>merge
                  contact_relationship
                  eff_start_date
                  ''                      " EffectiveEndDate
                  per_id
                  ''                      "
                  rel_per_id
                  ''                      " RelatedPersonNumber
                  contact_type            " Contact Type
                  ''                      " StatuoryDependent
                  emergency_contact
                  existing_person
                  'Y'
                  'N'
                  <p0021>-seqnr " TODO or count?
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.


      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.


*      tmp_pernr = <p0021>-pernr.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_mig_values.

    DATA: value_tmp TYPE zmhp_dd_value.

    " Process FAMSA mapping
    value_tmp = CONV #( p0021-famsa ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0021
        field_sap      = zmhp_cl_mig_contact_relation=>famsa
        field_oracle   = zmhp_cl_mig_utils=>contact_type
        mapping_fields = CONV #( mapping_fields_famsa )
        mapping_values = CONV #( mapping_values_famsa )
      CHANGING
        value          = value_tmp ).

    famsa = value_tmp.



  ENDMETHOD.


  METHOD proceed_cofu_con_relationship.

    get_cofu_data( ).
    update_begin_date( EXPORTING p0002 = p0002
                       CHANGING  p0021 = p0021 ).
    get_mapping_cofu_fields( ).
    get_mapping_cofu_values( ).

    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.


  METHOD update_begin_date.

    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).
      LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p0021>-pernr
                                                      AND begda LE endda
                                                      AND endda GE begda.
        EXIT.
      ENDLOOP.
      CHECK <p0002> IS ASSIGNED.

      <p0021>-begda = <p0002>-begda.
      UNASSIGN <p0002>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
