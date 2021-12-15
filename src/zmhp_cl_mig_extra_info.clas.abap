class ZMHP_CL_MIG_EXTRA_INFO definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0105 type P0105_TB .
  data VP_EXTRA_INFO type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants WORKER_EXTRA_INFO type STRING value 'WorkerExtraInfo' ##NO_TEXT.
  constants PER_EIT type STRING value 'PER_EIT' ##NO_TEXT.
  data P0050 type PTT_P0050 .
  constants SEW_EXTERNAL_IDS type STRING value 'SEW External IDs' ##NO_TEXT.
  constants SRC_ID_PREFIX type STRING value 'SEW_EFF_' ##NO_TEXT.

  methods PROCEED_COFU_EXTRA_INFO
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_EXTRA_INFO
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

  data VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data COGU type BOOLEAN .

  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
  methods MAP_COFU_DATA
    returning
      value(DATA) type STRING .
  methods MAP_COGL_DATA
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS ZMHP_CL_MIG_EXTRA_INFO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cogl EQ abap_true OR
       cogu EQ abap_true.
      vp_extra_info = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                               ( name = 2  value = worker_extra_info )
                               ( name = 3  value = 'FLEX:PER_PERSON_EIT_EFF' )
                               ( name = 4  value = 'EFF_CATEGORY_CODE' )
                               ( name = 5  value = 'InformationType' )
                               ( name = 6  value = 'PeiInformationCategory' )
                               ( name = 7  value = 'externalIdentifier(PER_PERSON_EIT_EFF=SEW External IDs)' )
                               ( name = 8  value = 'identifierType(PER_PERSON_EIT_EFF=SEW External IDs)' )
                               ( name = 9  value = 'endDate(PER_PERSON_EIT_EFF=SEW External IDs)' )
                               ( name = 10 value = 'startDate(PER_PERSON_EIT_EFF=SEW External IDs)' )
                               ( name = 11 value = 'PersonId(SourceSystemId)' )
                               ( name = 12 value = 'SourceSystemOwner' )
                               ( name = 13 value = 'SourceSystemId' )
                               ( name = 14 value = 'EffectiveStartDate' )
                               ( name = 15 value = 'EffectiveEndDate' ) ).
    ELSEIF cofu EQ abap_true.
      vp_extra_info = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                               ( name = 2  value = worker_extra_info )
                               ( name = 3  value = 'SourceSystemId' )
                               ( name = 4  value = 'SourceSystemOwner' )
                               ( name = 5  value = 'PersonId(SourceSystemId)' )
                               ( name = 6  value = 'ExternalIdentifierSequence' )
                               ( name = 7  value = 'ExternalIdentifierNumber' )
                               ( name = 8  value = 'ExternalIdentifierType' )
                               ( name = 9  value = 'DateFrom' )
                               ( name = 10 value = 'DateTo' )
                               ( name = 11 value = 'Comments' ) ).
    ENDIF.
  ENDMETHOD.


METHOD CREATE_METADATA.
  DESCRIBE TABLE vp_extra_info LINES DATA(length).

  LOOP AT vp_extra_info ASSIGNING FIELD-SYMBOL(<ext_info_data>).

    "set METADATA title
    CASE <ext_info_data>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <ext_info_data>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.
ENDMETHOD.


METHOD GET_COFU_DATA.

  "Get relevant IT0105 entries
  DATA(subty) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9998 )    "Get IT0105 Subty 9998 - CRM Pernr
                                    ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9900 ) ). "Get IT0105 Subty 9900 - Pernr local payroll system

  "Get IT0105
  SELECT pernr,
         begda,
         endda,
         subty,
         usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda AND
                                                                           subty IN @subty.

  "Get IT0050
  SELECT pernr,
         begda,
         endda,
         zausw FROM pa0050 INTO CORRESPONDING FIELDS OF TABLE @p0050 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD GET_COGL_DATA.

  "Get relevant IT0105 entries
  DATA(subty) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9998 )    "Get IT0105 Subty 9998 - CRM Pernr
                                    ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9900 ) ). "Get IT0105 Subty 9900 - Pernr local payroll system

  "Get IT0105
  SELECT pernr,
         begda,
         endda,
         subty,
         usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda AND
                                                                           subty IN @subty.

  "Get IT0050
  SELECT pernr,
         begda,
         endda,
         zausw FROM pa0050 INTO CORRESPONDING FIELDS OF TABLE @p0050 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id      TYPE string,
        sys_id      TYPE string,
        src_sys_id  TYPE string,
        ext_id_type TYPE string,
        begda_tmp   TYPE string,
        endda_tmp   TYPE string,
        data_tmp    TYPE string,
        old_9998    TYPE rsdsselopt_t,
        old_9900    TYPE rsdsselopt_t,
        old_0050    TYPE rsdsselopt_t.

  FIELD-SYMBOLS: <p0001> TYPE p0001.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).
    CHECK <p0105>-usrid IS NOT INITIAL.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).

    "Get id
    CASE <p0105>-subty.
      WHEN 9998.
        CHECK <p0105>-pernr NOT IN old_9998 OR
              old_9998      IS INITIAL.
        CONCATENATE per_eit '_' <p0105>-pernr '_' 'A' INTO src_id.
        ext_id_type = 'CRM Business Partner'.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO old_9998.
      WHEN 9900.
        CHECK <p0105>-pernr NOT IN old_9900 OR
              old_9900      IS INITIAL.
        CONCATENATE per_eit <p0105>-pernr '_' 'C' INTO src_id.
        ext_id_type = 'Third-Party Payroll ID'.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO old_9900.
    ENDCASE.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                begda = <p0105>-begda
                                                endda = <p0105>-endda
                                                vp_src_id = vp_src_id ).

    "ID needs to be unique
    CONCATENATE src_id_prefix src_id INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                worker_extra_info
                sew_external_ids
                per_eit
                sew_external_ids
                sew_external_ids
                <p0105>-usrid
                ext_id_type
                endda_tmp
                begda_tmp
                src_sys_id
                sys_id
                src_id
                begda_tmp
                endda_tmp
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.

  LOOP AT p0050 ASSIGNING FIELD-SYMBOL(<p0050>).
    CHECK <p0050>-zausw IS NOT INITIAL.

    CHECK <p0050>-pernr NOT IN old_0050 OR
          old_0050      IS INITIAL.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-endda ).

    ext_id_type = 'Time Device Badge ID'.
    CONCATENATE per_eit <p0050>-pernr '_' 'B' INTO src_id.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0050>-pernr
                                                begda = <p0050>-begda
                                                endda = <p0050>-endda
                                                vp_src_id = vp_src_id ).

    "ID needs to be unique
    CONCATENATE src_id_prefix src_id INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                worker_extra_info
                sew_external_ids
                per_eit
                sew_external_ids
                sew_external_ids
                <p0050>-zausw
                ext_id_type
                endda_tmp
                begda_tmp
                src_sys_id
                sys_id
                src_id
                begda_tmp
                endda_tmp
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0050>-pernr ) TO old_0050.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: src_id      TYPE string,
        sys_id      TYPE string,
        src_sys_id  TYPE string,
        ext_id_type TYPE string,
        begda_tmp   TYPE string,
        endda_tmp   TYPE string,
        data_tmp    TYPE string,
        old_9998    TYPE rsdsselopt_t,
        old_9900    TYPE rsdsselopt_t,
        old_0050    TYPE rsdsselopt_t.

  FIELD-SYMBOLS: <p0001> TYPE p0001.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).
    CHECK <p0105>-usrid IS NOT INITIAL.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).

    "Get id
    CASE <p0105>-subty.
      WHEN 9998.
        CHECK <p0105>-pernr NOT IN old_9998 OR
              old_9998      IS INITIAL.
        CONCATENATE per_eit '_' <p0105>-pernr '_' 'A' INTO src_id.
        ext_id_type = 'CRM Business Partner'.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO old_9998.
      WHEN 9900.
        CHECK <p0105>-pernr NOT IN old_9900 OR
              old_9900      IS INITIAL.
        CONCATENATE per_eit <p0105>-pernr '_' 'C' INTO src_id.
        ext_id_type = 'Third-Party Payroll ID'.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO old_9900.
    ENDCASE.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                begda = <p0105>-begda
                                                endda = <p0105>-endda
                                                vp_src_id = vp_src_id ).

    "ID needs to be unique
    CONCATENATE src_id_prefix src_id INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                worker_extra_info
                sew_external_ids
                per_eit
                sew_external_ids
                sew_external_ids
                <p0105>-usrid
                ext_id_type
                endda_tmp
                begda_tmp
                src_sys_id
                sys_id
                src_id
                begda_tmp
                endda_tmp
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.

  LOOP AT p0050 ASSIGNING FIELD-SYMBOL(<p0050>).
    CHECK <p0050>-zausw IS NOT INITIAL.

    CHECK <p0050>-pernr NOT IN old_0050 OR
          old_0050      IS INITIAL.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-endda ).

    ext_id_type = 'Time Device Badge ID'.
    CONCATENATE per_eit <p0050>-pernr '_' 'B' INTO src_id.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0050>-pernr
                                                begda = <p0050>-begda
                                                endda = <p0050>-endda
                                                vp_src_id = vp_src_id ).

    "ID needs to be unique
    CONCATENATE src_id_prefix src_id INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                worker_extra_info
                sew_external_ids
                per_eit
                sew_external_ids
                sew_external_ids
                <p0050>-zausw
                ext_id_type
                endda_tmp
                begda_tmp
                src_sys_id
                sys_id
                src_id
                begda_tmp
                endda_tmp
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0050>-pernr ) TO old_0050.
  ENDLOOP.
ENDMETHOD.


METHOD PROCEED_COFU_EXTRA_INFO.
  me->vp_src_id    = vp_src_id.
  get_cofu_data( ).
  data = map_cofu_data( ).
ENDMETHOD.


METHOD PROCEED_COGL_EXTRA_INFO.
  me->vp_src_id = vp_src_id.
  get_cogl_data( ).
  data = map_cogl_data( ).
ENDMETHOD.
ENDCLASS.
