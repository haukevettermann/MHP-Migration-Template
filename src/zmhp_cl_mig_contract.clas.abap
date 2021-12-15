class ZMHP_CL_MIG_CONTRACT definition
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
  constants CONTRACT type STRING value 'Contract' ##NO_TEXT.
  data CONTRACT_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .

  class-methods MAP_COGL_DATA
    importing
      !ASSIGNMENT_SOURCE_ID type STRING
      !BEGDA type STRING
      !PERSON_SOURCE_ID type STRING
      !SOURCE_SYSTEM_OWNER type STRING
      !PERNR type PERNR_D
    changing
      !CONTRACT type STRING .
  methods CONSTRUCTOR
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !PERNR type RSDSSELOPT_T .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZMHP_CL_MIG_CONTRACT IMPLEMENTATION.


METHOD constructor.
  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.

  IF cogl EQ abap_true.
    contract_structure = VALUE #( ).

  ELSEIF cofu EQ abap_true OR
         cogu EQ abap_true.

    contract_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                  ( name = 2  value = contract )
                                  ( name = 3  value = 'SourceSystemId' )
                                  ( name = 4  value = 'SourceSystemOwner' )
                                  ( name = 5  value = 'AssignmentId(SourceSystemId)' )
                                  ( name = 6  value = 'PersonId(SourceSystemId)' )
                                  ( name = 7  value = 'EffectiveStartDate' )
                                  ( name = 8  value = 'EffectiveEndDate' )
                                  ( name = 9  value = 'ContractNumber' )
                                  ( name = 10 value = 'ContractType' )
                                  ( name = 11 value = 'Description' )
                                  ( name = 12 value = 'Duration' )
                                  ( name = 13 value = 'DurationUnits' ) ).
  ENDIF.

ENDMETHOD.


METHOD CREATE_METADATA.

  DESCRIBE TABLE contract_structure LINES DATA(length).

  LOOP AT contract_structure ASSIGNING FIELD-SYMBOL(<contract_struc>).

    "set METADATA title
    CASE <contract_struc>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <contract_struc>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD map_cogl_data.

  CONCATENATE 'CON_' pernr INTO DATA(src_id).

  CONCATENATE zmhp_cl_mig_utils=>merge
              zmhp_cl_mig_contract=>contract
              src_id
              source_system_owner
              assignment_source_id
              person_source_id
              begda
              ''
              ''
              ''
              ''
              ''
              ''
  INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

  CONCATENATE contract cl_abap_char_utilities=>newline data_tmp INTO contract.

ENDMETHOD.
ENDCLASS.
