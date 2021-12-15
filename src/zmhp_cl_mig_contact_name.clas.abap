class ZMHP_CL_MIG_CONTACT_NAME definition
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
  data VP_CONTACT_NAME_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_NAME type STRING value 'ContactName' ##NO_TEXT.
  constants GLOBAL type STRING value 'GLOBAL' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.
  constants NAME type STRING value 'NAME' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_CON_NAME
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

  data P0002 type P0002_TAB .
  data P0001 type P0001_TAB .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS ZMHP_CL_MIG_CONTACT_NAME IMPLEMENTATION.


  METHOD constructor.
    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cogl EQ abap_true.

    ELSEIF cofu EQ abap_true.
      vp_contact_name_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                           ( name = 2  value = contact_name )
                                           ( name = 3  value = 'PersonId(SourceSystemId)' )
                                           ( name = 4  value = 'EffectiveStartDate' )
                                           ( name = 5  value = 'EffectiveEndDate' )
                                           ( name = 6  value = 'LegislationCode' )
                                           ( name = 7  value = 'NameType' )
                                           ( name = 8  value = 'FirstName' )
                                           ( name = 9  value = 'MiddleNames' )
                                           ( name = 10 value = 'LastName' )
                                           ( name = 11 value = 'SourceSystemOwner' )
                                           ( name = 12 value = 'SourceSystemId' ) ).
    ELSEIF cogu EQ abap_true.

    ENDIF.
  ENDMETHOD.


  method CREATE_METADATA.

    DESCRIBE TABLE vp_contact_name_structure LINES DATA(length).

    LOOP AT vp_contact_name_structure ASSIGNING FIELD-SYMBOL(<contact_name_struc>).

      "set METADATA title
      CASE <contact_name_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_name_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.


  endmethod.


  METHOD get_cofu_data.

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
           titl2,
           midnm INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
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
    SORT bukrs BY low.
    DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
    land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string,
          land1  TYPE  /iwbep/s_mgw_name_value_pair.



    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

      CLEAR land1.
      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0002>-pernr AND
                                                          begda LE <p0002>-endda AND
                                                          endda GE <p0002>-begda.

        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
        EXIT.
      ENDLOOP.

      CONCATENATE per
                  cont
                  <p0002>-pernr
      INTO DATA(per_id) SEPARATED BY '_'.

      DATA(eff_start_date) = zmhp_cl_mig_utils=>convert_date( <p0002>-begda ).

      sys_id = 'SAP_' && sy-mandt.
      src_id = per && '_' && cont && '_' && name && '_' &&  <p0002>-pernr.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  contact_name
                  per_id
                  eff_start_date
                  ''
                  land1-value
                  global
                  <p0002>-vorna " FirstName
                  <p0002>-midnm " MiddleNames
                  <p0002>-nachn " LastName
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.


      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.

  ENDMETHOD.


  METHOD proceed_cofu_con_name.

    get_cofu_data( ).
*    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
*                                           CHANGING p0002 = p0002 ).

    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
