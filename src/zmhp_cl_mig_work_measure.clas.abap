class ZMHP_CL_MIG_WORK_MEASURE definition
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
  constants ASSIGN_WORK_MEASURE type STRING value 'AssignmentWorkMeasure' ##NO_TEXT.
  data WORK_MEASURE_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .

  class-methods MAP_COGL_DATA
    importing
      !ASSIGNMENT_SOURCE_ID type STRING
      !BEGDA type STRING
      !ACTION type STRING
      !SOURCE_SYSTEM_OWNER type STRING
    changing
      !WORK_MEASURE type STRING .
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



CLASS ZMHP_CL_MIG_WORK_MEASURE IMPLEMENTATION.


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
    work_measure_structure = VALUE #( ( name = 1 value = zmhp_cl_mig_utils=>merge )
                                      ( name = 2 value = assign_work_measure )
                                      ( name = 3 value = 'AssignmentId(SourceSystemId)' )
                                      ( name = 4 value = 'EffectiveStartDate' )
                                      ( name = 5 value = 'Unit' )
                                      ( name = 6 value = 'ActionCode' )
                                      ( name = 7 value = 'SourceSystemOwner' )
                                      ( name = 8 value = 'SourceSystemId' )
                                      ( name = 9 value = 'Value' ) ).
  ELSEIF cofu EQ abap_true.
    work_measure_structure = VALUE #( ( name = 1 value = zmhp_cl_mig_utils=>merge )
                                      ( name = 2 value = assign_work_measure )
                                      ( name = 3 value = 'AssignmentId(SourceSystemId)' )
                                      ( name = 4 value = 'EffectiveStartDate' )
                                      ( name = 5 value = 'Unit' )
                                      ( name = 6 value = 'ActionCode' )
                                      ( name = 7 value = 'SourceSystemOwner' )
                                      ( name = 8 value = 'SourceSystemId' )
                                      ( name = 9 value = 'Value' ) ).

  ENDIF.

ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE work_measure_structure LINES DATA(length).

  LOOP AT work_measure_structure ASSIGNING FIELD-SYMBOL(<work_measure_struc>).

    "set METADATA title
    CASE <work_measure_struc>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <work_measure_struc>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD map_cogl_data.

  CONCATENATE assignment_source_id '_WorkMeasure_Headcount' INTO DATA(src_id).

  CONCATENATE zmhp_cl_mig_utils=>merge
              assign_work_measure
              assignment_source_id
              begda
              'HEAD'
              action
              source_system_owner
              src_id
              '1'
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

  CONCATENATE work_measure cl_abap_char_utilities=>newline data_tmp INTO work_measure.

ENDMETHOD.
ENDCLASS.
