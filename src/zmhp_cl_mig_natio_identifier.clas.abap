class ZMHP_CL_MIG_NATIO_IDENTIFIER definition
  public
  final
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data COGU type BOOLEAN .
  data VP_NATIO_IDENTIFIER_STRUC type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants NATIONAL_IDENTIFIER type STRING value 'PersonNationalIdentifier' ##NO_TEXT.
  data P0002 type P0002_TAB .
  data P0185 type P0185_TAB .
  constants NATIONAL_IDENTIFIER_ID type STRING value 'NID_' ##NO_TEXT.
  data P0064 type P0064_TAB .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_NATIO_IDENTIFIER
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
ENDCLASS.



CLASS ZMHP_CL_MIG_NATIO_IDENTIFIER IMPLEMENTATION.


METHOD constructor.
  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogl  = cogl.
  me->cogu  = cogu.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    " add later?
  ELSEIF cofu EQ abap_true.
    vp_natio_identifier_struc = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge  )
                                         ( name = 2  value = national_identifier )
                                         ( name = 3  value = 'SourceSystemOwner' )
                                         ( name = 4  value = 'SourceSystemId' )
                                         ( name = 5  value = 'PersonId(SourceSystemId)' )
                                         ( name = 6  value = 'LegislationCode' )
                                         ( name = 7  value = 'NationalIdentifierType' )
                                         ( name = 8  value = 'NationalIdentifierNumber' )
                                         ( name = 9  value = 'PrimaryFlag' ) ).
  ENDIF.

ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_natio_identifier_struc LINES DATA(length).

    LOOP AT vp_natio_identifier_struc ASSIGNING FIELD-SYMBOL(<natio_identi_struc>).

      CASE <natio_identi_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <natio_identi_struc>-value INTO metadata.
      CHECK length NE sy-tabix.

      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.

    ENDLOOP.

  ENDMETHOD.


METHOD get_cofu_data.

  SELECT pernr,
         begda,
         endda,
         natio,
         perid
    FROM pa0002 INTO CORRESPONDING FIELDS OF TABLE @p0002 WHERE pernr IN @pernr
                                                            AND begda LE @endda
                                                            AND endda GE @begda.
*  SORT p0002 BY pernr ASCENDING endda DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM p0002 COMPARING pernr.

  SELECT pernr,
         begda,
         endda,
         numss
    FROM pa0064 INTO CORRESPONDING FIELDS OF TABLE @p0064 WHERE pernr IN @pernr
                                                            AND begda LE @endda
                                                            AND endda GE @begda.

  SELECT pernr,
         begda,
         endda,
         fpdat,
         expid,
         isspl,
         ictyp
    FROM pa0185 INTO CORRESPONDING FIELDS OF TABLE @p0185 WHERE pernr IN @pernr
                                                            AND begda LE @endda
                                                            AND endda GE @begda.

ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id    TYPE string,
        sys_id    TYPE string,
        lega_code TYPE string,
        number    TYPE string,
        type      TYPE string.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).
    CLEAR: lega_code, type.

    CASE sy-mandt.
      WHEN zmhp_cl_int_constants=>cofu_mandant-france.
        lega_code = 'FR'.
        type      = 'ORA_NTT'.
        LOOP AT p0064 ASSIGNING FIELD-SYMBOL(<p0064>) WHERE pernr EQ <p0002>-pernr AND
                                                            begda LE <p0002>-endda AND
                                                            endda GE <p0002>-begda.
          EXIT.
        ENDLOOP.
        number = <p0002>-perid && <p0064>-numss.
      WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands.
        lega_code = 'NL'.
        type      = 'BSN_SOFI_NUMBER'.
        number = CONV #( <p0002>-perid ).
      WHEN zmhp_cl_int_constants=>cofu_mandant-italy.
        IF '15' IN molga.
          lega_code = 'IT'.
          type      = ''.
          number = CONV #( <p0002>-perid ).
        ELSE.
          CONTINUE.
        ENDIF.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    CONDENSE number.

    CONCATENATE national_identifier_id <p0002>-pernr INTO src_id.
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr     = <p0002>-pernr
                                                      begda     = <p0002>-begda
                                                      endda     = <p0002>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE zmhp_cl_mig_utils=>merge
                national_identifier
                sys_id
                src_id
                src_sys_id
                lega_code
                type
                number
                'Y'
           INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.

ENDMETHOD.


METHOD proceed_cofu_natio_identifier.
  get_cofu_data( ).
  zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0002 = p0002 ).
  zmhp_cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
