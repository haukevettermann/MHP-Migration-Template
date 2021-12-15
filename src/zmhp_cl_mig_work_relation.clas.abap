class ZMHP_CL_MIG_WORK_RELATION definition
  public
  create public .

public section.

  types:
    BEGIN OF vp_wkr_id ,
            pernr TYPE pernr ,
            begda TYPE begda ,
            endda TYPE endda ,
            wkr_id TYPE string,
           END OF vp_wkr_id .
  types:
    vp_wkr_id_t type STANDARD TABLE OF vp_wkr_id .

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COGL type BOOLEAN .
  data COFU type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0001 type P0001_TAB .
  data VP_WORK_REL_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants WORK_RELATIONSHIP type STRING value 'WorkRelationship' ##NO_TEXT.
  constants WR type STRING value 'WR_' ##NO_TEXT.
  data P0000 type P0000_TAB .
  data P0701 type ZMHP_P0701_TAB .
  data P0041 type P0041_TAB .
  data VP_WRK_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T .
  data P0016 type P0016_TAB .
  data PN_BEGDA type BEGDA .

  methods PROCEED_COFU_WORK_RELATION
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    exporting
      !VP_WKR_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !DATA_TERM type STRING
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_WORK_RELATION
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    exporting
      !VP_WKR_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !DATA_TERM type STRING
    returning
      value(DATA) type STRING .
  methods PROCEED_COGU_WORK_RELATION
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    exporting
      !VP_WKR_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T
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
      !COGU type BOOLEAN
      !PN_BEGDA type BEGDA optional .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  PROTECTED SECTION.
private section.

  data MAPPING_FIELDS_PERSG type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSG type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_MASSN type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_MASSN type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_MASSG type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_MASSG type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data COGU type BOOLEAN .

  methods GET_DATE_START
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(DATUM) type DATUM .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !DATA_TERM type STRING
    returning
      value(DATA) type STRING .
  methods MAP_MIG_COGU_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
    exporting
      !MASSN type ZMHP_DD_VALUE
      !MASSG type ZMHP_DD_VALUE
      !WORKER_TYPE type ZMHP_DD_VALUE
      !LEGAL_EMP_NAME type ZMHP_DD_VALUE .
  methods MAP_MIG_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
    exporting
      !MASSN type ZMHP_DD_VALUE
      !MASSG type ZMHP_DD_VALUE
      !WORKER_TYPE type ZMHP_DD_VALUE
      !LEGAL_EMP_NAME type ZMHP_DD_VALUE .
  methods SUMMARIZE_OLD_TERMINATION_COGL .
  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
  methods GET_MAPPING_COGU_FIELDS .
  methods GET_MAPPING_COGU_VALUES .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_COGL_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      value(DATA_TERM) type STRING
    returning
      value(DATA) type STRING .
  methods MAP_COGU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      value(DATA_TERM) type STRING
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS ZMHP_CL_MIG_WORK_RELATION IMPLEMENTATION.


METHOD constructor.

  me->pernr = pernr.
  me->begda = begda.
  me->pn_begda = pn_begda. "JMB20211011 I - C400129651-5882
  me->endda = endda.
  me->cofu = cofu.
  me->cogu = cogu.
  me->cogl = cogl.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    vp_work_rel_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                     ( name = 2  value = work_relationship )
                                     ( name = 3  value = 'SourceSystemOwner' )
                                     ( name = 4  value = 'SourceSystemId' )
                                     ( name = 5  value = 'PersonId(SourceSystemId)' )
                                     ( name = 6  value = 'LegalEmployerName' )
                                     ( name = 7  value = 'DateStart' )
                                     ( name = 8  value = 'WorkerType' )
                                     ( name = 9  value = 'LegalEmployerSeniorityDate' )
                                     ( name = 10 value = 'ActualTerminationDate' )
                                     ( name = 16 value = 'LastWorkingDate' )
                                     ( name = 17 value = 'TerminateWorkRelationshipFlag' )
                                     ( name = 11 value = 'EnterpriseSeniorityDate' )
                                     ( name = 12 value = 'PrimaryFlag' )
                                     ( name = 13 value = 'WorkerNumber' )
                                     ( name = 14 value = 'ActionCode' )
                                     ( name = 15 value = 'ReasonCode' ) ).
  ELSEIF cofu EQ abap_true.
    vp_work_rel_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                     ( name = 2  value = work_relationship )
                                     ( name = 3  value = 'SourceSystemOwner' )
                                     ( name = 4  value = 'SourceSystemId' )
                                     ( name = 5  value = 'PersonId(SourceSystemId)' )
                                     ( name = 6  value = 'LegalEmployerName' )
                                     ( name = 7  value = 'DateStart' )
                                     ( name = 8  value = 'WorkerType' )
                                     ( name = 9  value = 'LegalEmployerSeniorityDate' )
                                     ( name = 10 value = 'ActualTerminationDate' )
                                     ( name = 16 value = 'LastWorkingDate' )
                                     ( name = 17 value = 'TerminateWorkRelationshipFlag' )
                                     ( name = 11 value = 'EnterpriseSeniorityDate' )
                                     ( name = 12 value = 'PrimaryFlag' )
                                     ( name = 13 value = 'WorkerNumber' )
                                     ( name = 14 value = 'ActionCode' )
                                     ( name = 15 value = 'ReasonCode' ) ).
  ENDIF.
ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_work_rel_structure LINES DATA(length).

    LOOP AT vp_work_rel_structure ASSIGNING FIELD-SYMBOL(<work_rela_struc>).

      "set METADATA title
      CASE <work_rela_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <work_rela_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


METHOD get_cofu_data.

  "Get IT0000
  IF p0000 IS INITIAL.
    SELECT pernr,
           begda,
           endda,
           massn,
           massg INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDIF.

  "Get all IT0001
  SELECT pernr,
         begda,
         endda,
         bukrs,
         persg,
         persk,
         ansvh INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get all IT0016
  SELECT pernr,
         begda,
         endda,
         kondt,
         eindt,
         persg INTO CORRESPONDING FIELDS OF TABLE @p0016 FROM pa0016 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get IT0701
  SELECT pernr,
         begda,
         endda,
         tdate INTO CORRESPONDING FIELDS OF TABLE @p0701 FROM pa0701 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD get_cogl_data.

  "Get IT0000
  IF p0000 IS INITIAL.
    SELECT pernr,
           begda,
           endda,
           massn,
           massg INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDIF.

  "Get all IT0001
  SELECT pernr,
         begda,
         endda,
         bukrs,
         persg,
         persk INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get IT0701
  SELECT pernr,
         begda,
         endda,
         tdate INTO CORRESPONDING FIELDS OF TABLE @p0701 FROM pa0701 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD get_date_start.

  LOOP AT p0016 ASSIGNING FIELD-SYMBOL(<p0016>) WHERE pernr EQ pernr AND
                                                      begda LE endda AND
                                                      endda GE begda.
    datum = <p0016>-eindt.
    EXIT.
  ENDLOOP.

  CHECK datum is INITIAL.
  datum = begda.
ENDMETHOD.


METHOD get_mapping_cogu_fields.

  "get mapping fields for actioncode
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massn
                                                   oracle_field = zmhp_cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_fields = mapping_fields_massn ).


  "get mapping fields for reasoncode
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massg
                                                   oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_massg ).

  DATA(worker_sap) = zmhp_cl_mig_work_terms=>persk.
  IF '03' IN molga.
    worker_sap = zmhp_cl_mig_utils=>persg.
  ENDIF.

  "get mapping fields for workertype
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = zmhp_cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg ).

  "get mapping fields for legal employer
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>bukrs
                                                   oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_bukrs ).
ENDMETHOD.


METHOD get_mapping_cogu_values.

  "get mapping values for actioncode
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massn
                                                   oracle_field = zmhp_cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_values = mapping_values_massn ).


  "get mapping values for reasoncode
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massg
                                                   oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_massg ).

  DATA(worker_sap) = zmhp_cl_mig_work_terms=>persk.
  IF '03' IN molga.
    worker_sap = zmhp_cl_mig_utils=>persg.
  ENDIF.

  "get mapping values for workertype
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = zmhp_cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg ).

  "get mapping values for legal employer
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>bukrs
                                                   oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs ).
ENDMETHOD.


METHOD get_mapping_fields.

    "get mapping fields for actioncode
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massn
                                                   oracle_field = zmhp_cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_fields = mapping_fields_massn ).


  "get mapping fields for reasoncode
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massg
                                                   oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_massg ).

  "get mapping fields for workertype
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>persg
                                                   oracle_field = zmhp_cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg ).

  "get mapping fields for legal employer
  zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>bukrs
                                                   oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_bukrs ).
ENDMETHOD.


METHOD get_mapping_values.

  "get mapping values for actioncode
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massn
                                                   oracle_field = zmhp_cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_values = mapping_values_massn ).


  "get mapping values for reasoncode
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0000
                                                   sap_field    = zmhp_cl_mig_utils=>massg
                                                   oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_massg ).

  "get mapping values for workertype
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>persg
                                                   oracle_field = zmhp_cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg ).

  "get mapping values for legal employer
  zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = zmhp_cl_mig_utils=>it0001
                                                   sap_field    = zmhp_cl_mig_utils=>bukrs
                                                   oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id         TYPE string,
        sys_id         TYPE string,
        seniority_date TYPE string,
        tdate          TYPE string,
        hire_date      TYPE string,
        pernr_old      TYPE rsdsselopt_t.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>).

    DATA(term) = abap_false.
    "30.03.2021: terminations in future needs to be passed in separate file
    IF begda         GT sy-datum   AND
       <p0000>-massn IN massn_term.
      term = abap_true.
    ELSE.
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "set hire date
      seniority_date = hire_date = zmhp_cl_mig_utils=>convert_date( <p0000>-begda ).
    ENDIF.

    DATA(term_flag) = zmhp_cl_mig_utils=>no.

    "in case of termination
    IF term EQ abap_true.
      DATA(last_date) = CONV datum( <p0000>-begda - 1 ).
      tdate = zmhp_cl_mig_utils=>convert_date( last_date ).
      term_flag = zmhp_cl_mig_utils=>yes.
    ENDIF.

    "get relevant P0000
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE sy-datum AND
                                                        endda GE sy-datum AND
                                                        pernr EQ <p0000>-pernr.
      EXIT.
    ENDLOOP.

    "check entries in future
    IF sy-subrc NE 0.
      LOOP AT p0001 ASSIGNING <p0001> WHERE begda LE <p0000>-begda AND
                                            endda GE <p0000>-begda AND
                                            pernr EQ <p0000>-pernr.
        EXIT.
      ENDLOOP.
    ENDIF.

    CHECK sy-subrc EQ 0.

    map_mig_values( EXPORTING p0000       = <p0000>
                              p0001       = <p0001>
                    IMPORTING massg       = DATA(massg)
                              massn       = DATA(massn)
                              worker_type = DATA(worker_type)
                              legal_emp_name = DATA(legal_emp_name) ).

    CONCATENATE wr <p0000>-pernr INTO src_id.

    "store work relationship for assignment entity
    APPEND VALUE #( pernr  = <p0000>-pernr
                    begda  = <p0000>-begda
                    endda  = <p0000>-endda
                    wkr_id = src_id ) TO vp_wrk_id.

    "get relevant P0701
    LOOP AT p0701 ASSIGNING FIELD-SYMBOL(<p0701>) WHERE begda LE <p0000>-endda AND
                                                        endda GE <p0000>-begda AND
                                                        pernr EQ <p0000>-pernr.
      tdate = zmhp_cl_mig_utils=>convert_date( <p0701>-tdate ).
      EXIT.
    ENDLOOP.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0000>-pernr
                                                      begda = <p0000>-begda
                                                      endda = <p0000>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                work_relationship
                sys_id
                src_id
                src_sys_id
                legal_emp_name
                hire_date
                worker_type
                seniority_date
                tdate
                tdate
                term_flag
                seniority_date
                zmhp_cl_mig_utils=>yes
                '' "<p0000>-pernr "JMB20210407 D: WorkerNumber doesn´t exist during migration
                massn
                massg
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    IF term EQ abap_true.
      CONCATENATE data_term cl_abap_char_utilities=>newline data_tmp INTO data_term.
    ELSE.
      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    ENDIF.

    CLEAR: tdate, seniority_date, term_flag.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: src_id         TYPE string,
        sys_id         TYPE string,
        seniority_date TYPE string,
        tdate          TYPE string,
        hire_date      TYPE string,
        pernr_old      TYPE rsdsselopt_t.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>).

    DATA(term) = abap_false.
    "30.03.2021: terminations in future needs to be passed in separate file
    IF <p0000>-begda GT sy-datum   AND
       <p0000>-massn IN massn_term.
      term = abap_true.
    ELSE.
      "30.03.2021: for cogl we only migrate the first hire entry
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "set hire date
      seniority_date = hire_date = zmhp_cl_mig_utils=>convert_date( <p0000>-begda ).
    ENDIF.

    DATA(term_flag) = zmhp_cl_mig_utils=>no.

    "in case of termination
    IF term EQ abap_true.
      DATA(last_date) = CONV datum( <p0000>-begda - 1 ).
      tdate = zmhp_cl_mig_utils=>convert_date( last_date ).
      term_flag = zmhp_cl_mig_utils=>yes.
    ENDIF.

    "get relevant P0001
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE sy-datum AND "<p0000>-endda AND "JMB20210416 D: Only pass the actual worker type (Poland)
                                                        endda GE sy-datum AND "<p0000>-begda AND "JMB20210416 D: Only pass the actual worker type (Poland)
                                                        pernr EQ <p0000>-pernr.
      EXIT.
    ENDLOOP.

    "check entries in future
    IF sy-subrc NE 0.
      LOOP AT p0001 ASSIGNING <p0001> WHERE begda LE <p0000>-begda AND
                                            endda GE <p0000>-begda AND
                                            pernr EQ <p0000>-pernr.
        EXIT.
      ENDLOOP.
    ENDIF.

    CHECK sy-subrc EQ 0.

    map_mig_values( EXPORTING p0000       = <p0000>
                              p0001       = <p0001>
                    IMPORTING massg       = DATA(massg)
                              massn       = DATA(massn)
                              worker_type = DATA(worker_type)
                              legal_emp_name = DATA(legal_emp_name) ).

    CONCATENATE wr <p0000>-pernr INTO src_id.

    "store work relationship for assignment entity
    APPEND VALUE #( pernr  = <p0000>-pernr
                    begda  = <p0000>-begda
                    endda  = cl_hcp_global_constants=>c_highdate "JMB20210408 I - pass highdate instead of <p0000>-endda, due to that onyl one WR can exist
                    wkr_id = src_id ) TO vp_wrk_id.

    "get relevant P0701
    LOOP AT p0701 ASSIGNING FIELD-SYMBOL(<p0701>) WHERE begda LE <p0000>-endda AND
                                                        endda GE <p0000>-begda AND
                                                        pernr EQ <p0000>-pernr.
      tdate = zmhp_cl_mig_utils=>convert_date( <p0701>-tdate ).
      EXIT.
    ENDLOOP.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0000>-pernr
                                                      begda = <p0000>-begda
                                                      endda = <p0000>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                work_relationship
                sys_id
                src_id
                src_sys_id
                legal_emp_name
                hire_date
                worker_type
                seniority_date
                tdate
                tdate
                term_flag
                seniority_date
                zmhp_cl_mig_utils=>yes
                '' "<p0000>-pernr "JMB20210407 D: WorkerNumber doesn´t exist during migration
                massn
                massg
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    IF term EQ abap_true.
      CONCATENATE data_term cl_abap_char_utilities=>newline data_tmp INTO data_term.
    ELSE.
      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    ENDIF.

    CLEAR: tdate, seniority_date, term_flag.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogu_data.

  DATA: src_id         TYPE string,
        sys_id         TYPE string,
        seniority_date TYPE string,
        tdate          TYPE string,
        hire_date      TYPE string,
        pernr_old      TYPE rsdsselopt_t.

  DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                        WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                           ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                        WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                           ( sign = 'I' option = 'EQ' low = '34' ) )
                                        WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                        "default Italy/Austria
                                        ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                      ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>).

    DATA(term) = abap_false.
    "30.03.2021: terminations in future needs to be passed in separate file
*    IF <p0000>-begda GT sy-datum   AND "JMB20210929 D - C400129651-5694
    IF <p0000>-begda GT pn_begda   AND "JMB20210929 I - C400129651-5882
       <p0000>-massn IN massn_term.
      term = abap_true.
    ELSE.
      "30.03.2021: for cogl we only migrate the first hire entry
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "set hire date
      seniority_date = hire_date = zmhp_cl_mig_utils=>convert_date( <p0000>-begda ).
    ENDIF.

    DATA(term_flag) = zmhp_cl_mig_utils=>no.

    "in case of termination
    IF term EQ abap_true.
      DATA(last_date) = CONV datum( <p0000>-begda - 1 ).
      tdate = zmhp_cl_mig_utils=>convert_date( last_date ).
      term_flag = zmhp_cl_mig_utils=>yes.
    ENDIF.

    "get relevant P0001
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE sy-datum AND "<p0000>-endda AND "JMB20210416 D: Only pass the actual worker type (Poland)
                                                        endda GE sy-datum AND "<p0000>-begda AND "JMB20210416 D: Only pass the actual worker type (Poland)
                                                        pernr EQ <p0000>-pernr.
      EXIT.
    ENDLOOP.

    "check entries in future
    IF sy-subrc NE 0.
      LOOP AT p0001 ASSIGNING <p0001> WHERE begda LE <p0000>-begda AND
                                            endda GE <p0000>-begda AND
                                            pernr EQ <p0000>-pernr.
        EXIT.
      ENDLOOP.
    ENDIF.

    CHECK sy-subrc EQ 0.

    map_mig_cogu_values( EXPORTING p0000       = <p0000>
                                   p0001       = <p0001>
                         IMPORTING massg       = DATA(massg)
                                   massn       = DATA(massn)
                                   worker_type = DATA(worker_type)
                                   legal_emp_name = DATA(legal_emp_name) ).

    CONCATENATE wr <p0000>-pernr INTO src_id.

    "store work relationship for assignment entity
    APPEND VALUE #( pernr  = <p0000>-pernr
                    begda  = <p0000>-begda
                    endda  = cl_hcp_global_constants=>c_highdate "JMB20210408 I - pass highdate instead of <p0000>-endda, due to that onyl one WR can exist
                    wkr_id = src_id ) TO vp_wrk_id.

    "get relevant P0701
    LOOP AT p0701 ASSIGNING FIELD-SYMBOL(<p0701>) WHERE begda LE <p0000>-endda AND
                                                        endda GE <p0000>-begda AND
                                                        pernr EQ <p0000>-pernr.
      tdate = zmhp_cl_mig_utils=>convert_date( <p0701>-tdate ).
      EXIT.
    ENDLOOP.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0000>-pernr
                                                      begda = <p0000>-begda
                                                      endda = <p0000>-endda
                                                      vp_src_id = vp_src_id ).

    "JMB20210921 I - pass IT0016 as seniority date instead of IT0000 (C400129651-5698)
    seniority_date = SWITCH string( sy-mandt
                                    WHEN zmhp_cl_int_constants=>cofu_mandant-germany
                                    THEN zmhp_cl_mig_utils=>convert_date( get_date_start( pernr = <p0000>-pernr
                                                                                          begda = <p0000>-begda
                                                                                          endda = <p0000>-endda ) )
                                    ELSE seniority_date ).

    DATA(legal_emp_date) = SWITCH string( sy-mandt
                                          WHEN zmhp_cl_int_constants=>cofu_mandant-germany
                                          THEN ''
                                          ELSE seniority_date ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                work_relationship
                sys_id
                src_id
                src_sys_id
                legal_emp_name
                hire_date
                worker_type
                legal_emp_date
                tdate
                tdate
                term_flag
                seniority_date
                zmhp_cl_mig_utils=>yes
                '' "<p0000>-pernr "JMB20210407 D: WorkerNumber doesn´t exist during migration
                massn
                massg
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    IF term EQ abap_true.
      CONCATENATE data_term cl_abap_char_utilities=>newline data_tmp INTO data_term.
    ELSE.
      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    ENDIF.

    CLEAR: tdate, seniority_date, term_flag.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
  ENDLOOP.
ENDMETHOD.


METHOD map_mig_cogu_values.
  DATA: value_tmp TYPE zmhp_dd_value.

  "Process MASSN mapping
  value_tmp = p0000-massn.
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0000
      field_sap      = zmhp_cl_mig_utils=>massn
      field_oracle   = zmhp_cl_mig_utils=>actioncode
      mapping_fields = CONV #( mapping_fields_massn )
      mapping_values = CONV #( mapping_values_massn )
    CHANGING
      value          = value_tmp ).

  massn = value_tmp.

  "Process MASSG mapping
  value_tmp = p0000-massg.
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0000
      field_sap      = zmhp_cl_mig_utils=>massg
      field_oracle   = zmhp_cl_mig_utils=>reasoncode
      mapping_fields = CONV #( mapping_fields_massg )
      mapping_values = CONV #( mapping_values_massg )
    CHANGING
      value          = value_tmp ).

  massg = value_tmp.

  READ TABLE molga INTO DATA(molga_entry) INDEX 1.
  DATA(fields_gk) = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty     = zmhp_cl_mig_utils=>it0001
                                                                field_sap = zmhp_cl_mig_utils=>persg
                                                                value     = p0001-persg )
                                                              ( infty     = zmhp_cl_mig_utils=>it0001
                                                                field_sap = zmhp_cl_mig_work_terms=>persk
                                                                value     = p0001-persk )
                                                              ( infty     = zmhp_cl_mig_utils=>it0001
                                                                field_sap = 'MOLGA'
                                                                value     = molga_entry-low )
                                                              ( infty     = zmhp_cl_mig_utils=>it0001
                                                                field_sap = 'ANSVH'
                                                                value     = p0001-ansvh ) ).
  "in case of Austria Employee group is relevant
  DATA(worker_sap) = zmhp_cl_mig_work_terms=>persk.
  value_tmp = p0001-persk.
  IF '03' IN molga.
    worker_sap = zmhp_cl_mig_utils=>persg.
    value_tmp = CONV #( p0001-persg ).
  ENDIF.

  "Process workertype mapping
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0001
      field_sap      = worker_sap
      field_oracle   = zmhp_cl_mig_utils=>workertype
      mapping_fields = CONV #( mapping_fields_persg )
      mapping_values = CONV #( mapping_values_persg )
      fields         = fields_gk
    CHANGING
      value          = value_tmp ).

  worker_type = value_tmp.

  "Process BUKRS Mapping
  value_tmp = p0001-bukrs.
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0001
      field_sap      = zmhp_cl_mig_utils=>bukrs
      field_oracle   = zmhp_cl_mig_utils=>legalemployername
      mapping_fields = CONV #( mapping_fields_bukrs )
      mapping_values = CONV #( mapping_values_bukrs )
    CHANGING
      value          = value_tmp ).

  legal_emp_name = value_tmp.
ENDMETHOD.


METHOD map_mig_values.
  DATA: value_tmp TYPE zmhp_dd_value.

  "Process MASSN mapping
  value_tmp = p0000-massn.
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0000
      field_sap      = zmhp_cl_mig_utils=>massn
      field_oracle   = zmhp_cl_mig_utils=>actioncode
      mapping_fields = CONV #( mapping_fields_massn )
      mapping_values = CONV #( mapping_values_massn )
    CHANGING
      value          = value_tmp ).

  massn = value_tmp.

  "Process MASSG mapping
  value_tmp = p0000-massg.
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0000
      field_sap      = zmhp_cl_mig_utils=>massg
      field_oracle   = zmhp_cl_mig_utils=>reasoncode
      mapping_fields = CONV #( mapping_fields_massg )
      mapping_values = CONV #( mapping_values_massg )
    CHANGING
      value          = value_tmp ).

  massg = value_tmp.

  "Process PERSG mapping
  value_tmp = p0001-persg.
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0001
      field_sap      = zmhp_cl_mig_utils=>persg
      field_oracle   = zmhp_cl_mig_utils=>workertype
      mapping_fields = CONV #( mapping_fields_persg )
      mapping_values = CONV #( mapping_values_persg )
    CHANGING
      value          = value_tmp ).

  worker_type = value_tmp.

  "Process BUKRS Mapping
  value_tmp = p0001-bukrs.
  zmhp_cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0001
      field_sap      = zmhp_cl_mig_utils=>bukrs
      field_oracle   = zmhp_cl_mig_utils=>legalemployername
      mapping_fields = CONV #( mapping_fields_bukrs )
      mapping_values = CONV #( mapping_values_bukrs )
    CHANGING
      value          = value_tmp ).

  legal_emp_name = value_tmp.
ENDMETHOD.


METHOD PROCEED_COFU_WORK_RELATION.
  p0000 = worker->p0000.

  get_cofu_data( ).
  get_mapping_cogu_fields( ).
  get_mapping_cogu_values( ).
  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000       = worker->p0000
                                                  create_hire = abap_true
                                         CHANGING p0001 = p0001 ).

  data = map_cogu_data( EXPORTING vp_src_id = vp_src_id
                        IMPORTING data_term = data_term ).
  vp_wkr_id = me->vp_wrk_id.
ENDMETHOD.


METHOD proceed_cogl_work_relation.
  p0000 = worker->p0000.

  get_cogl_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000       = worker->p0000
                                                  create_hire = abap_true
                                         CHANGING p0001 = p0001 ).

  data = map_cogl_data( EXPORTING vp_src_id = vp_src_id
                        IMPORTING data_term = data_term ).
  vp_wkr_id = me->vp_wrk_id.
ENDMETHOD.


METHOD PROCEED_COGU_WORK_RELATION.
  DATA: data_term type string.
  p0000 = worker->p0000.

  get_cofu_data( ).
  get_mapping_cogu_fields( ).
  get_mapping_cogu_values( ).
  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000       = worker->p0000
                                                  create_hire = abap_true
                                         CHANGING p0001 = p0001 ).

  data = map_cogu_data( EXPORTING vp_src_id = vp_src_id
                        IMPORTING data_term = data_term ).

  "only one file can be proceed
  CONCATENATE data cl_abap_char_utilities=>newline data_term INTO data.
  vp_wkr_id = me->vp_wrk_id.
ENDMETHOD.


METHOD summarize_old_termination_cogl.
  DATA: pernr_tmp   TYPE pernr_d,
        p0000_tmp   TYPE p0000,
        p0000_coll  TYPE p0000_tab,
        p0000_pernr TYPE p0000_tab.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  DATA(p0000_actual) = p0000.
  DELETE p0000_actual WHERE endda LT sy-datum.

  LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE endda LT sy-datum.
    IF pernr_tmp NE <p0000>-pernr.
      pernr_tmp = <p0000>-pernr.

      IF p0000_tmp IS NOT INITIAL.
        APPEND p0000_tmp TO p0000_coll.
      ENDIF.
      p0000_tmp = <p0000>.
      CONTINUE.
    ENDIF.

    "in case of old termination
    IF <p0000>-massn IN massn_term.
      <p0000>-begda = p0000_tmp-begda.
      APPEND <p0000> TO p0000_coll.
      CLEAR: p0000_tmp.
      CONTINUE.
    ELSE.
      APPEND p0000_tmp TO p0000_coll.
      p0000_tmp = <p0000>.
    ENDIF.
  ENDLOOP.

  p0000 = p0000_coll.
  APPEND LINES OF p0000_actual TO p0000.

  SORT p0000 BY pernr begda ASCENDING.
ENDMETHOD.
ENDCLASS.
