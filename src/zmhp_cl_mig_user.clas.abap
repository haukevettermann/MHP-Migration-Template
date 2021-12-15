class ZMHP_CL_MIG_USER definition
  public
  create public .

public section.

  data VP_USER_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants USER type STRING value 'User' ##NO_TEXT.
  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0105 type P0105_TB .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COGL type BOOLEAN
      !COFU type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_USER
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_USER
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

  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
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



CLASS ZMHP_CL_MIG_USER IMPLEMENTATION.


method CONSTRUCTOR.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    vp_user_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                 ( name = 2  value = user )
                                 ( name = 3  value = 'SourceSystemOwner' )
                                 ( name = 4  value = 'SourceSystemId' )
                                 ( name = 5  value = 'PersonId(SourceSystemId)' )
                                 ( name = 6  value = 'Username' )
                                 ( name = 7  value = 'GenerateUserAccount' )
                                 ( name = 8  value = 'CredentialsEmailSent' ) ).
  ELSEIF cofu EQ abap_true.
    vp_user_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                 ( name = 2  value = user )
                                 ( name = 3  value = 'SourceSystemOwner' )
                                 ( name = 4  value = 'SourceSystemId' )
                                 ( name = 5  value = 'PersonId(SourceSystemId)' )
                                 ( name = 6  value = 'Username' )
                                 ( name = 7  value = 'GenerateUserAccount' )
                                 ( name = 8  value = 'CredentialsEmailSent' ) ).
  ENDIF.

  endmethod.


METHOD create_metadata.

  DESCRIBE TABLE vp_user_structure LINES DATA(length).

  LOOP AT vp_user_structure ASSIGNING FIELD-SYMBOL(<user_struc>).

    "set METADATA title
    CASE <user_struc>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <user_struc>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD GET_COFU_DATA.

  "Get relevant IT0105 entries
  DATA(subty) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_0001 ) ). "Get IT0105 Subty 0001 - SAP User

  "Get IT0105
  SELECT pernr,
         begda,
         endda,
         subty,
         usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND "only actual entry
                                                                           endda GE @begda AND "only actual entry
                                                                           subty IN @subty.
ENDMETHOD.


METHOD GET_COGL_DATA.

  "Get relevant IT0105 entries
  DATA(subty) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_0001 ) ). "Get IT0105 Subty 0001 - SAP User

  "Get IT0105
  SELECT pernr,
         begda,
         endda,
         subty,
         usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND "only actual entry
                                                                           endda GE @begda AND "only actual entry
                                                                           subty IN @subty.
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id    TYPE string,
        pernr_old TYPE rsdsselopt_t.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).

    CHECK <p0105>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                      begda = <p0105>-begda
                                                      endda = <p0105>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE src_sys_id '_' user INTO src_id.

    CHECK src_id IS NOT INITIAL.

    CHECK <p0105>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    CONCATENATE zmhp_cl_mig_utils=>merge
                user
                sys_id
                src_id
                src_sys_id
                <p0105>-usrid
                zmhp_cl_mig_utils=>yes
                zmhp_cl_mig_utils=>no
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO pernr_old.

    CLEAR src_id.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: src_id    TYPE string,
        pernr_old TYPE rsdsselopt_t.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                      begda = <p0105>-begda
                                                      endda = <p0105>-endda
                                                      vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I

    CONCATENATE src_sys_id '_' user INTO src_id.

    CHECK <p0105>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    "pass dummy user for dev/test
    DATA(userid) = SWITCH string( sy-sysid
                                  WHEN 'D02' OR 'Q02' THEN 'DUMMY' && <p0105>-pernr
                                  ELSE <p0105>-usrid ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                user
                sys_id
                src_id
                src_sys_id
                userid
                zmhp_cl_mig_utils=>yes
                zmhp_cl_mig_utils=>no
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO pernr_old.

    CLEAR src_id.
  ENDLOOP.
ENDMETHOD.


METHOD PROCEED_COFU_USER.
  get_cofu_data( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD PROCEED_COGL_USER.
  get_cogl_data( ).
  data = map_cogl_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
