class ZMHP_CL_MIG_DRIVERS_LICENCE definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_DRIVERS_LICENCE_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_DRIVERS_LICENCE type STRING value 'PersonDriversLicence' ##NO_TEXT.
  constants DL type STRING value 'DL_' ##NO_TEXT.
  data P0002 type P0002_TAB .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_DRIVERS_LICENCE
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
  data P9920 type ZMHP_P9920_TAB .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS ZMHP_CL_MIG_DRIVERS_LICENCE IMPLEMENTATION.


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

    vp_drivers_licence_structure = VALUE #( ( name = 1   value = zmhp_cl_mig_utils=>merge )
                                            ( name = 2   value = person_drivers_licence )
                                            ( name = 3   value = 'SourceSystemOwner' )
                                            ( name = 4   value = 'SourceSystemId' )
                                            ( name = 5   value = 'DateFrom' )
                                            ( name = 6   value = 'DateTo' )
                                            ( name = 7   value = 'PersonId(SourceSystemId)' )
                                            ( name = 8   value = 'LegislationCode' )
                                            ( name = 9   value = 'LicenseType' )
                                            ( name = 10  value = 'LicenseNumber' )
                                            ( name = 11  value = 'IssuingAuthority' )
                                            ( name = 12  value = 'IssuingCountry' )
                                            ( name = 13  value = 'LicenseSuspended' )
                                            ( name = 14  value = 'SuspendedFromDate' )
                                            ( name = 15  value = 'SuspendedToDate' )
                                            ( name = 16  value = 'NumberOfPoints' )
                                            ( name = 17  value = 'Violations' ) ).
  ENDIF.



  endmethod.


  method CREATE_METADATA.

    DESCRIBE TABLE vp_drivers_licence_structure LINES DATA(length).

    LOOP AT vp_drivers_licence_structure ASSIGNING FIELD-SYMBOL(<driver_licen_struc>).

      "set METADATA title
      CASE <driver_licen_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <driver_licen_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.

    ENDLOOP.

  endmethod.


  method GET_COFU_DATA.

    "get IT0002
    SELECT pernr,
           begda,
           endda,
           natio INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                                         AND begda LE @endda
                                                                         AND endda GE @begda
                                                                         AND natio IS NOT NULL.

    "get IT9920
    SELECT pernr,
           begda,
           endda,
           create_date,
           ablauf_date,
           nummer,
           sperre FROM pa9920 INTO CORRESPONDING FIELDS OF TABLE @p9920 WHERE pernr IN @pernr AND
                                                                         begda LE @endda AND
                                                                         endda GE @begda.

  endmethod.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT p9920 ASSIGNING FIELD-SYMBOL(<p9920>).

      LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p9920>-pernr
                                                    AND begda LE endda
                                                    AND endda GE begda.
        EXIT.
      ENDLOOP.


      CONCATENATE dl <p9920>-pernr INTO src_id.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p9920>-pernr
                                                        begda = <p9920>-begda
                                                        endda = <p9920>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(date_from) = COND string( WHEN <p9920>-create_date IS NOT INITIAL THEN zmhp_cl_mig_utils=>convert_date( <p9920>-create_date )
                                      ELSE '' ).

      DATA(date_to) = COND string( WHEN <p9920>-ablauf_date IS NOT INITIAL THEN zmhp_cl_mig_utils=>convert_date( <p9920>-ablauf_date )
                                      ELSE '' ).


      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_drivers_licence
                  sys_id
                  src_id
                  date_from
                  date_to
                  src_sys_id
                  <p0002>-natio "LegislationCode
                  '' "LicenceType
                  <p9920>-nummer "LicenceNumber
                  '' "IssuingAuthority
                  '' "IssuingCountry
                  <p9920>-sperre "LicenseSuspended
                  '' "SuspendedFromDate
                  '' "SuspendedToDate
                  '' "NumberOfPoints
                  '' "Violations
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.

  ENDMETHOD.


  method PROCEED_COFU_DRIVERS_LICENCE.

    get_cofu_data( ).
    data = map_cofu_data( vp_src_id ).

  endmethod.
ENDCLASS.
