class ZMHP_CL_MIG_EXTERNAL_IDENT definition
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
  data VP_EXTERNAL_IDENT_DATA type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants EXTERNAL_IDENT_DATA type STRING value 'ExternalIdentifier' ##NO_TEXT.
  constants EXT type STRING value 'EXT_' ##NO_TEXT.
  data P0050 type PTT_P0050 .
  data P0001 type P0001_TAB .

  methods PROCEED_COFU_EXTERNAL_IDENT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_LCL_PERNR type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_EXTERNAL_IDENT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_LCL_PERNR type /IWBEP/T_MGW_NAME_VALUE_PAIR
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
  data VP_LCL_PERNR type /IWBEP/T_MGW_NAME_VALUE_PAIR .
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



CLASS ZMHP_CL_MIG_EXTERNAL_IDENT IMPLEMENTATION.


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
      vp_external_ident_data = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                        ( name = 2  value = external_ident_data )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'PersonId(SourceSystemId)' )
                                        ( name = 6  value = 'ExternalIdentifierSequence' )
                                        ( name = 7  value = 'ExternalIdentifierNumber' )
                                        ( name = 8  value = 'ExternalIdentifierType' )
                                        ( name = 9  value = 'DateFrom' )
                                        ( name = 10 value = 'DateTo' )
                                        ( name = 11 value = 'Comments' ) ).
    ELSEIF cofu EQ abap_true.
      vp_external_ident_data = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                        ( name = 2  value = external_ident_data )
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


METHOD create_metadata.
  DESCRIBE TABLE vp_external_ident_data LINES DATA(length).

  LOOP AT vp_external_ident_data ASSIGNING FIELD-SYMBOL(<ext_ident_data>).

    "set METADATA title
    CASE <ext_ident_data>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <ext_ident_data>-value INTO metadata.

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

  "Get IT0001
  SELECT pernr,
         begda,
         endda,
         bukrs FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE @p0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD get_cogl_data.

**JMB20210422 delete start - This kind of information will be migrated via Extra Info entity
*
*  "Get relevant IT0105 entries
*  DATA(subty) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9998 )    "Get IT0105 Subty 9998 - CRM Pernr
*                                    ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9900 ) ). "Get IT0105 Subty 9900 - Pernr local payroll system
*
*  "Get IT0105
*  SELECT pernr,
*         begda,
*         endda,
*         subty,
*         usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
*                                                                           begda LE @endda AND
*                                                                           endda GE @begda AND
*                                                                           subty IN @subty.
*
*  "Get IT0050
*  SELECT pernr,
*         begda,
*         endda,
*         zausw FROM pa0050 INTO CORRESPONDING FIELDS OF TABLE @p0050 WHERE pernr IN @pernr AND
*                                                                           begda LE @endda AND
*                                                                           endda GE @begda.
*
*  "Get IT0001
*  SELECT pernr,
*         begda,
*         endda,
*         bukrs FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE @p0001 WHERE pernr IN @pernr AND
*                                                                           begda LE @endda AND
*                                                                           endda GE @begda.
*JMB20210422 delete end
ENDMETHOD.


METHOD map_cofu_data.

  DATA: pernr_tmp   TYPE pernr_d,
        language    TYPE string,
        src_id      TYPE string,
        sys_id      TYPE string,
        src_sys_id  TYPE string,
        ext_id_seq  TYPE int8 VALUE 2,
        ext_id_type TYPE string,
        comments    TYPE string,
        begda_tmp   TYPE string,
        endda_tmp   TYPE string,
        data_tmp    TYPE string,
        vp_seq      TYPE /iwbep/t_mgw_name_value_pair.

  FIELD-SYMBOLS: <p0001> TYPE p0001.

  CONSTANTS: timestamp TYPE string VALUE '00:00:00'.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  "Add SAP Pernr as external Identifier
  LOOP AT vp_lcl_pernr ASSIGNING FIELD-SYMBOL(<lcl_pernr>).
    CONCATENATE data cl_abap_char_utilities=>newline <lcl_pernr>-value INTO data.
  ENDLOOP.

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).
    CHECK <p0105>-usrid IS NOT INITIAL.

    IF  pernr_tmp IS INITIAL.
      pernr_tmp = <p0105>-pernr.
    ENDIF.

    IF pernr_tmp NE <p0105>-pernr.
      APPEND VALUE #( name  = pernr_tmp
                      value = ext_id_seq ) TO vp_seq.
      pernr_tmp = <p0105>-pernr.
      ext_id_seq = 2.
    ENDIF.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).
    CONCATENATE begda_tmp timestamp INTO begda_tmp SEPARATED BY space.
    CONCATENATE endda_tmp timestamp INTO endda_tmp SEPARATED BY space.

    "Get id
    CASE <p0105>-subty.
      WHEN 9998.
        CONCATENATE ext <p0105>-pernr '_' 'A' INTO src_id.
        ext_id_type = 'SEW_CRM_BP'.
      WHEN 9900.
        CONCATENATE ext <p0105>-pernr '_' 'C' INTO src_id.
        ext_id_type = 'ORA_3RD_PARTY_PAY_ID'.
    ENDCASE.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                begda = <p0105>-begda
                                                endda = <p0105>-endda
                                                vp_src_id = vp_src_id ).

    DATA(ext_id_0105) = CONV string( ext_id_seq ).
    CONDENSE ext_id_0105.

    "get legal entity
    LOOP AT p0001 ASSIGNING <p0001> WHERE pernr EQ <p0105>-pernr AND
                                          begda LE <p0105>-endda AND
                                          endda GE <p0105>-begda.
      comments = <p0001>-bukrs.
      EXIT.
    ENDLOOP.

    "ID needs to be unique
    CONCATENATE src_id '_' ext_id_0105 INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                external_ident_data
                src_id
                sys_id
                src_sys_id
                ext_id_0105
                <p0105>-usrid
                ext_id_type
                begda_tmp
                endda_tmp
                comments
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    ADD 1 TO ext_id_seq.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    CLEAR: comments.
  ENDLOOP.

  "add last pernr
  IF pernr_tmp IS NOT INITIAL.
    APPEND VALUE #( name  = pernr_tmp
                    value = ext_id_seq ) TO vp_seq.
  ENDIF.

  CLEAR: pernr_tmp, ext_id_seq.

  LOOP AT p0050 ASSIGNING FIELD-SYMBOL(<p0050>).
    CHECK <p0050>-zausw IS NOT INITIAL.
    ADD 1 TO ext_id_seq.

    READ TABLE vp_seq INTO DATA(seq) WITH KEY name = <p0050>-pernr.

    IF pernr_tmp IS INITIAL.
      pernr_tmp = <p0050>-pernr.
      IF seq-value IS NOT INITIAL.
        ext_id_seq = seq-value.
      ELSE.
        ext_id_seq = 2.
      ENDIF.
    ENDIF.

    IF pernr_tmp NE <p0050>-pernr.
      pernr_tmp = <p0050>-pernr.

      IF seq-value IS NOT INITIAL.
        ext_id_seq = seq-value.
      ELSE.
        ext_id_seq = 2.
      ENDIF.
    ENDIF.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-endda ).

    CONCATENATE begda_tmp timestamp INTO begda_tmp SEPARATED BY space.
    CONCATENATE endda_tmp timestamp INTO endda_tmp SEPARATED BY space.

    ext_id_type = 'ORA_TCLOCK_BADGE_ID'.
    CONCATENATE ext <p0050>-pernr '_' 'B' INTO src_id.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0050>-pernr
                                                begda = <p0050>-begda
                                                endda = <p0050>-endda
                                                vp_src_id = vp_src_id ).

    DATA(ext_id_0050) = CONV string( ext_id_seq ).
    CONDENSE ext_id_0050.

    "get legal entity
    LOOP AT p0001 ASSIGNING <p0001> WHERE pernr EQ <p0050>-pernr AND
                                          begda LE <p0050>-endda AND
                                          endda GE <p0050>-begda.
      comments = <p0001>-bukrs.
      EXIT.
    ENDLOOP.

    "ID needs to be unique
    CONCATENATE src_id '_' ext_id_0050 INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                external_ident_data
                src_id
                sys_id
                src_sys_id
                ext_id_0050
                <p0050>-zausw
                ext_id_type
                begda_tmp
                endda_tmp
                comments
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    CLEAR: seq, comments.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: pernr_tmp   TYPE pernr_d,
        language    TYPE string,
        src_id      TYPE string,
        sys_id      TYPE string,
        src_sys_id  TYPE string,
        ext_id_seq  TYPE int8 VALUE 2,
        ext_id_type TYPE string,
        comments    TYPE string,
        begda_tmp   TYPE string,
        endda_tmp   TYPE string,
        data_tmp    TYPE string,
        vp_seq      TYPE /iwbep/t_mgw_name_value_pair.

  FIELD-SYMBOLS: <p0001> TYPE p0001.

  CONSTANTS: timestamp TYPE string VALUE '00:00:00'.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  "Add SAP Pernr as external Identifier
  LOOP AT vp_lcl_pernr ASSIGNING FIELD-SYMBOL(<lcl_pernr>).
    CONCATENATE data cl_abap_char_utilities=>newline <lcl_pernr>-value INTO data.
  ENDLOOP.

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).
    CHECK <p0105>-usrid IS NOT INITIAL.

    IF  pernr_tmp IS INITIAL.
      pernr_tmp = <p0105>-pernr.
    ENDIF.

    IF pernr_tmp NE <p0105>-pernr.
      APPEND VALUE #( name  = pernr_tmp
                      value = ext_id_seq ) TO vp_seq.
      pernr_tmp = <p0105>-pernr.
      ext_id_seq = 2.
    ENDIF.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).
    CONCATENATE begda_tmp timestamp INTO begda_tmp SEPARATED BY space.
    CONCATENATE endda_tmp timestamp INTO endda_tmp SEPARATED BY space.

    "Get id
    CASE <p0105>-subty.
      WHEN 9998.
        CONCATENATE ext <p0105>-pernr '_' 'A' INTO src_id.
        ext_id_type = 'SEW_CRM_BP'.
      WHEN 9900.
        CONCATENATE ext <p0105>-pernr '_' 'C' INTO src_id.
        ext_id_type = 'ORA_3RD_PARTY_PAY_ID'.
    ENDCASE.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                begda = <p0105>-begda
                                                endda = <p0105>-endda
                                                vp_src_id = vp_src_id ).

    DATA(ext_id_0105) = CONV string( ext_id_seq ).
    CONDENSE ext_id_0105.

    "get legal entity
    LOOP AT p0001 ASSIGNING <p0001> WHERE pernr EQ <p0105>-pernr AND
                                          begda LE <p0105>-endda AND
                                          endda GE <p0105>-begda.
      comments = <p0001>-bukrs.
      EXIT.
    ENDLOOP.

    "ID needs to be unique
    CONCATENATE src_id '_' ext_id_0105 INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                external_ident_data
                src_id
                sys_id
                src_sys_id
                ext_id_0105
                <p0105>-usrid
                ext_id_type
                begda_tmp
                endda_tmp
                comments
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    ADD 1 TO ext_id_seq.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    CLEAR: comments.
  ENDLOOP.

  "add last pernr
  IF pernr_tmp IS NOT INITIAL.
    APPEND VALUE #( name  = pernr_tmp
                    value = ext_id_seq ) TO vp_seq.
  ENDIF.

  CLEAR: pernr_tmp, ext_id_seq.

  LOOP AT p0050 ASSIGNING FIELD-SYMBOL(<p0050>).
    CHECK <p0050>-zausw IS NOT INITIAL.
    ADD 1 TO ext_id_seq.

    READ TABLE vp_seq INTO DATA(seq) WITH KEY name = <p0050>-pernr.

    IF pernr_tmp IS INITIAL.
      pernr_tmp = <p0050>-pernr.
      IF seq-value IS NOT INITIAL.
        ext_id_seq = seq-value.
      ELSE.
        ext_id_seq = 2.
      ENDIF.
    ENDIF.

    IF pernr_tmp NE <p0050>-pernr.
      pernr_tmp = <p0050>-pernr.

      IF seq-value IS NOT INITIAL.
        ext_id_seq = seq-value.
      ELSE.
        ext_id_seq = 2.
      ENDIF.
    ENDIF.

    begda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-begda ).
    endda_tmp = zmhp_cl_mig_utils=>convert_date( <p0050>-endda ).

    CONCATENATE begda_tmp timestamp INTO begda_tmp SEPARATED BY space.
    CONCATENATE endda_tmp timestamp INTO endda_tmp SEPARATED BY space.

    ext_id_type = 'ORA_TCLOCK_BADGE_ID'.
    CONCATENATE ext <p0050>-pernr '_' 'B' INTO src_id.

    "get source id
    src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = <p0050>-pernr
                                                begda = <p0050>-begda
                                                endda = <p0050>-endda
                                                vp_src_id = vp_src_id ).

    DATA(ext_id_0050) = CONV string( ext_id_seq ).
    CONDENSE ext_id_0050.

    "get legal entity
    LOOP AT p0001 ASSIGNING <p0001> WHERE pernr EQ <p0050>-pernr AND
                                          begda LE <p0050>-endda AND
                                          endda GE <p0050>-begda.
      comments = <p0001>-bukrs.
      EXIT.
    ENDLOOP.

    "ID needs to be unique
    CONCATENATE src_id '_' ext_id_0050 INTO src_id.

    CONCATENATE zmhp_cl_mig_utils=>merge
                external_ident_data
                src_id
                sys_id
                src_sys_id
                ext_id_0050
                <p0050>-zausw
                ext_id_type
                begda_tmp
                endda_tmp
                comments
    INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

    CLEAR: seq, comments.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD PROCEED_COFU_EXTERNAL_IDENT.
  me->vp_src_id    = vp_src_id.
  me->vp_lcl_pernr = vp_lcl_pernr.
  get_cofu_data( ).
  data = map_cofu_data( ).
ENDMETHOD.


METHOD proceed_cogl_external_ident.
  me->vp_src_id    = vp_src_id.
  me->vp_lcl_pernr = vp_lcl_pernr.
  get_cogl_data( ).
  data = map_cogl_data( ).
ENDMETHOD.
ENDCLASS.
