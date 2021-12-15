class ZMHP_CL_MIG_PERSON_EMAIL definition
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
  data VP_PERSON_EMAIL type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_EMAIL type STRING value 'PersonEmail' ##NO_TEXT.
  constants EML type STRING value 'EML_' ##NO_TEXT.
  constants W1 type STRING value 'W1' ##NO_TEXT.

  methods PROCEED_COFU_PERSON_EMAIL
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_PERSON_EMAIL
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



CLASS ZMHP_CL_MIG_PERSON_EMAIL IMPLEMENTATION.


METHOD constructor.

  me->pernr = pernr.
  me->begda = begda.

  "only one email entry can be passed
  IF sy-datum GT begda.
    me->begda = sy-datum.
  ENDIF.

  me->endda = endda.
  me->cofu = cofu.
  me->cogu = cogu.
  me->cogl = cogl.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    vp_person_email = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                               ( name = 2  value = person_email )
                               ( name = 3  value = 'SourceSystemId' )
                               ( name = 4  value = 'SourceSystemOwner' )
                               ( name = 5  value = 'PersonId(SourceSystemId)' )
                               ( name = 6  value = 'DateFrom' )
                               ( name = 7  value = 'DateTo' )
                               ( name = 8  value = 'EmailType' )
                               ( name = 9  value = 'EmailAddress' )
                               ( name = 10 value = 'PrimaryFlag' ) ).
  ELSEIF cofu EQ abap_true.
    vp_person_email = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                               ( name = 2  value = person_email )
                               ( name = 3  value = 'SourceSystemId' )
                               ( name = 4  value = 'SourceSystemOwner' )
                               ( name = 5  value = 'PersonId(SourceSystemId)' )
                               ( name = 6  value = 'DateFrom' )
                               ( name = 7  value = 'DateTo' )
                               ( name = 8  value = 'EmailType' )
                               ( name = 9  value = 'EmailAddress' )
                               ( name = 10 value = 'PrimaryFlag' ) ).
  ENDIF.
ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_person_email LINES DATA(length).

  LOOP AT vp_person_email ASSIGNING FIELD-SYMBOL(<person_mail>).

    "set METADATA title
    CASE <person_mail>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <person_mail>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD GET_COFU_DATA.

  SELECT pernr,
         begda,
         endda,
         subty,
         usrid_long FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                                begda LE @endda AND
                                                                                endda GE @begda AND "only actual entry
                                                                                subty EQ @zmhp_cl_mig_utils=>it0105_0010.
ENDMETHOD.


METHOD get_cogl_data.

  SELECT pernr,
         begda,
         endda,
         subty,
         usrid_long FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                                begda LE @endda AND "sy-datum "only actual entry
                                                                                endda GE @begda AND "sy-datum "only actual entry
                                                                                subty EQ @zmhp_cl_mig_utils=>it0105_0010.
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id    TYPE string,
        sys_id    TYPE string,
        pernr_old TYPE rsdsselopt_t.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).

    CHECK <p0105>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
    DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).
    CONCATENATE eml <p0105>-pernr '_' w1 INTO src_id.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                      begda = <p0105>-begda
                                                      endda = <p0105>-endda
                                                      vp_src_id = vp_src_id ).

    "in case of development system, pass dummy email
    DATA(email) = <p0105>-usrid_long.

    email = SWITCH #( sy-sysid
                      WHEN 'D02' OR 'Q02' THEN 'DUMMY' && <p0105>-pernr
                      ELSE email ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                person_email
                src_id
                sys_id
                src_sys_id
                begda_tmp
                endda_tmp
                w1
                email
                zmhp_cl_mig_utils=>yes
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO pernr_old.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: src_id    TYPE string,
        sys_id    TYPE string,
        pernr_old TYPE rsdsselopt_t.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).

    CHECK <p0105>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
    DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).
    CONCATENATE eml <p0105>-pernr '_' w1 INTO src_id.

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                      begda = <p0105>-begda
                                                      endda = <p0105>-endda
                                                      vp_src_id = vp_src_id ).

    "in case of development system, pass dummy email
    DATA(email) = <p0105>-usrid_long.

    email = SWITCH #( sy-sysid
                      WHEN 'D02' OR 'Q02' THEN 'DUMMY' && <p0105>-pernr
                      ELSE email ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                person_email
                src_id
                sys_id
                src_sys_id
                begda_tmp
                endda_tmp
                w1
                email
                zmhp_cl_mig_utils=>yes
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0105>-pernr ) TO pernr_old.
  ENDLOOP.
ENDMETHOD.


METHOD PROCEED_COFU_PERSON_EMAIL.
  get_cofu_data( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD proceed_cogl_person_email.
  get_cogl_data( ).
  data = map_cogl_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
