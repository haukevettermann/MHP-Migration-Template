class ZMHP_CL_MIG_PERSON_PHONE definition
  public
  create public .

public section.

  types:
    zmhp_vorw_udata_list TYPE TABLE OF zmhp_vorw_udata .

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VORW_UDATA type ZMHP_VORW_UDATA_LIST .
  data P0002 type P0002_TAB .
  data VP_PERSON_PHONE_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_PHONE type STRING value 'PersonPhone' ##NO_TEXT.
  constants PHONE type STRING value 'PHONE_' ##NO_TEXT.
  data P0105 type P0105_TB .

  methods GET_COFU_PHONE_TYPE
    importing
      !SUBTY type SUBTY
      !PERNR type PERNR_D
      !BEGDA type BEGDA
    changing
      !NUMBER type SYSID
    returning
      value(PHONE_TYPE) type STRING .
  methods SPLIT_PHONE_NUMBER_COFU
    importing
      !NUMBER type SYSID
      !LEG_CODE type LAND1
      !WERKS type PERSA
      !BTRTL type BTRTL
      !PHONE_TYPE type STRING
    exporting
      !COUNTRY_CODE type STRING
      !AREA_CODE type STRING
      !PHONE_NUMBER type STRING
      !EXTENSION type STRING .
  class-methods SPLIT_PHONE_NUMBER_COGL
    importing
      !NUMBER type SYSID
      !LEG_CODE type LAND1
    exporting
      !COUNTRY_CODE type STRING
      !AREA_CODE type STRING
      !PHONE_NUMBER type STRING
      !EXTENSION type STRING .
  methods PROCEED_COFU_PERSON_PHONE
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_PERSON_PHONE
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
  PRIVATE SECTION.

    DATA p0001 TYPE p0001_tab .
    DATA land1_map TYPE /iwbep/t_mgw_name_value_pair .
    DATA cogu TYPE boolean .

    METHODS update_begda_it0105 .
    METHODS get_cofu_data .
    METHODS get_cogl_data .
    METHODS map_cofu_data
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
      RETURNING
        VALUE(data) TYPE string .
    METHODS map_cogl_data
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
      RETURNING
        VALUE(data) TYPE string .
ENDCLASS.



CLASS ZMHP_CL_MIG_PERSON_PHONE IMPLEMENTATION.


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
      vp_person_phone_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                           ( name = 2  value = person_phone )
                                           ( name = 3  value = 'SourceSystemId' )
                                           ( name = 4  value = 'SourceSystemOwner' )
                                           ( name = 5  value = 'PersonId(SourceSystemId)' )
                                           ( name = 6  value = 'DateFrom' )
                                           ( name = 7  value = 'DateTo' )
                                           ( name = 8  value = 'PrimaryFlag' )
                                           ( name = 9  value = 'PhoneType' )
                                           ( name = 10 value = 'CountryCodeNumber' )
                                           ( name = 11 value = 'AreaCode' )
                                           ( name = 12 value = 'PhoneNumber' )
                                           ( name = 13 value = 'Extension' )
                                           ( name = 14 value = 'LegislationCode' ) ).
    ELSEIF cofu EQ abap_true.
      vp_person_phone_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                           ( name = 2  value = person_phone )
                                           ( name = 3  value = 'SourceSystemId' )
                                           ( name = 4  value = 'SourceSystemOwner' )
                                           ( name = 5  value = 'PersonId(SourceSystemId)' )
                                           ( name = 6  value = 'DateFrom' )
                                           ( name = 7  value = 'DateTo' )
                                           ( name = 8  value = 'PrimaryFlag' )
                                           ( name = 9  value = 'PhoneType' )
                                           ( name = 10 value = 'CountryCodeNumber' )
                                           ( name = 11 value = 'AreaCode' )
                                           ( name = 12 value = 'PhoneNumber' )
                                           ( name = 13 value = 'Extension' )
                                           ( name = 14 value = 'LegislationCode' ) ).
    ENDIF.
  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_person_phone_structure LINES DATA(length).

    LOOP AT vp_person_phone_structure ASSIGNING FIELD-SYMBOL(<person_phone_struc>).

      "set METADATA title
      CASE <person_phone_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_phone_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


METHOD get_cofu_data.
  DATA: subty TYPE rsdsselopt_t.

  "Get IT0001
  SELECT pernr,
         begda,
         endda,
         bukrs,
         werks,
         btrtl INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs by low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).

  CASE sy-mandt.
    WHEN zmhp_cl_int_constants=>cofu_mandant-germany.
      subty = VALUE #( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9002 )    "W1
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9004 )    "WF
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9005 )    "WM
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9003 ) ). "W3

      "get area code
      DATA(werks) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-werks ) ).
      DATA(btrtl) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-btrtl ) ).

      DELETE ADJACENT DUPLICATES FROM werks COMPARING low.
      DELETE ADJACENT DUPLICATES FROM btrtl COMPARING low.

      SELECT werks,
             btrtl,
             landesvorw,
             ortsvorw FROM zmhp_vorw_udata INTO CORRESPONDING FIELDS OF TABLE @vorw_udata WHERE werks IN @werks AND
                                                                                                btrtl IN @btrtl.

    WHEN zmhp_cl_int_constants=>cofu_mandant-france.
      subty = VALUE #( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9005 )    "W1
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9004 )    "WF
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9002 ) ). "WM

    WHEN zmhp_cl_int_constants=>cofu_mandant-italy   OR
         zmhp_cl_int_constants=>cofu_mandant-austria.

      subty = VALUE #( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9901 )    "Get IT0105 Subty 9901 - AD phone number
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9906 )    "Get IT0105 Subty 9906 - Phone internal only
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9905 )    "Get IT0105 Subty 9905 - AD mobil
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9902 ) ).
    WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands.

      subty = VALUE #( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9901 )    "Get IT0105 Subty 9901 - AD phone number
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9902 )    "WF
                       ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9905 ) ).    "Get IT0105 Subty 9905 - AD mobil
  ENDCASE.

  CHECK subty IS NOT INITIAL.

  "Get IT0105
  SELECT pernr,
         begda,
         endda,
         subty,
         usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr    AND
                                                                           begda LE @sy-datum AND "only actual entry
                                                                           endda GE @sy-datum AND "only actual entry
                                                                           subty IN @subty.
ENDMETHOD.


  METHOD get_cofu_phone_type.
    "differentiate by mandant
    CASE sy-mandt.
      WHEN zmhp_cl_int_constants=>cofu_mandant-germany.
        phone_type = SWITCH #( subty
                                WHEN 9002 THEN 'W1'
                                WHEN 9004 THEN 'WF'
                                WHEN 9005 THEN 'WM'
                                WHEN 9003 THEN 'W3' ).
        CHECK phone_type EQ 'WM'.

        DATA mobile TYPE zmhp_hr_char60.

        "get complete mobile number
        CALL FUNCTION 'ZMHP_HRPA_IDM_GET_MOBILE'
          EXPORTING
            im_pernr = pernr
            im_date  = begda
            im_subty = zmhp_cl_mig_utils=>it0105_9005
          IMPORTING
            ex_value = mobile.
        number = mobile.

      WHEN zmhp_cl_int_constants=>cofu_mandant-france.

        phone_type = SWITCH #( subty
                               WHEN 9005 THEN 'W1'
                               WHEN 9004 THEN 'WF'
                               WHEN 9002 THEN 'WM' ).

      WHEN zmhp_cl_int_constants=>cofu_mandant-italy  OR
           zmhp_cl_int_constants=>cofu_mandant-austria.

        phone_type = SWITCH #( subty
                               WHEN 9901 THEN 'W1'
                               WHEN 9902 THEN 'WF'
                               WHEN 9905 THEN 'WM'
                               WHEN 9906 THEN 'W2' ).

      WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands.

        phone_type = SWITCH #( subty
                               WHEN 9901 THEN 'W1'
                               WHEN 9902 THEN 'WF'
                               WHEN 9905 THEN 'WM' ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_cogl_data.

    "Get relevant IT0105 entries
    DATA(subty) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9901 )    "Get IT0105 Subty 9901 - AD phone number
                                      ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9906 )    "Get IT0105 Subty 9906 - Phone internal only
                                      ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9905 )    "Get IT0105 Subty 9905 - AD mobil
                                      ( sign = 'I' option = 'EQ' low = zmhp_cl_mig_utils=>it0105_9902 ) ). "Get IT0105 Subty 9902 - AD fax number

**JMB20210312 delete start - not relevant for personPhone
*
*  "Get IT0002
*  SELECT pernr,
*         begda,
*         endda INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
*                                                                           begda LE @endda AND
*                                                                           endda GE @begda.
*JMB20210312 delete end

    "Get IT0001
    SELECT pernr,
           begda,
           endda,
           bukrs,
           werks,
           btrtl INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

    DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
    SORT bukrs BY low.
    DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
    land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).

    "Get IT0105
    SELECT pernr,
           begda,
           endda,
           subty,
           usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr    AND
                                                                             begda LE @sy-datum AND "only actual entry
                                                                             endda GE @sy-datum AND "only actual entry
                                                                             subty IN @subty.

*  update_begda_it0105( ).
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: ad_pn      TYPE string VALUE zmhp_cl_mig_utils=>no,
          phone_type TYPE string,
          src_id     TYPE string,
          sys_id     TYPE string,
          land1      TYPE /iwbep/s_mgw_name_value_pair.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).

      ad_pn = zmhp_cl_mig_utils=>no.

      "Get phone type
      phone_type = get_cofu_phone_type( EXPORTING subty  = <p0105>-subty
                                                  pernr  = <p0105>-pernr
                                                  begda  = <p0105>-begda
                                        CHANGING  number = <p0105>-usrid  ).

      IF phone_type EQ 'W1'.
        ad_pn = zmhp_cl_mig_utils=>yes.
      ENDIF.

      CHECK phone_type IS NOT INITIAL.

      CONCATENATE phone <p0105>-pernr '_' phone_type INTO src_id.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                        begda = <p0105>-begda
                                                        endda = <p0105>-endda
                                                        vp_src_id = vp_src_id ).

      "get legislationcode
      CLEAR land1.
      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0105>-pernr AND
                                                          begda LE <p0105>-endda AND
                                                          endda GE <p0105>-begda.

        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
        EXIT.
      ENDLOOP.

      CHECK <p0001> IS ASSIGNED.

      split_phone_number_cofu( EXPORTING number       = <p0105>-usrid
                                         werks        = <p0001>-werks
                                         btrtl        = <p0001>-btrtl
                                         leg_code     = CONV #( land1-value )
                                         phone_type   = phone_type
                               IMPORTING country_code = DATA(country_code)
                                         area_code    = DATA(area_code)
                                         phone_number = DATA(phone_number)
                                         extension    = DATA(extension) ).

      "in case no country code and phone number is provided, skip entry
      CHECK country_code IS NOT INITIAL AND
            phone_number IS NOT INITIAL.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_phone
                  src_id
                  sys_id
                  src_sys_id
                  begda_tmp
                  endda_tmp
                  ad_pn
                  phone_type
                  country_code
                  area_code
                  phone_number
                  extension
                  land1-value
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: phone_type.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_cogl_data.

    DATA: ad_pn      TYPE string VALUE zmhp_cl_mig_utils=>no,
          phone_type TYPE string,
          src_id     TYPE string,
          sys_id     TYPE string,
          land1      TYPE /iwbep/s_mgw_name_value_pair.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>).

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0105>-endda ).

      ad_pn = zmhp_cl_mig_utils=>no.

      "Get phone type
      CASE <p0105>-subty.
        WHEN 9901.
          phone_type = 'W1'.
          CONCATENATE phone <p0105>-pernr '_' phone_type INTO src_id.
          ad_pn = zmhp_cl_mig_utils=>yes.
        WHEN 9902.
          phone_type = 'WF'.
          CONCATENATE phone <p0105>-pernr '_' phone_type INTO src_id.
        WHEN 9905.
          phone_type = 'WM'.
          CONCATENATE phone <p0105>-pernr '_' phone_type INTO src_id.
        WHEN 9906.
          phone_type = 'W2'.
          CONCATENATE phone <p0105>-pernr '_' phone_type INTO src_id.
      ENDCASE.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0105>-pernr
                                                        begda = <p0105>-begda
                                                        endda = <p0105>-endda
                                                        vp_src_id = vp_src_id ).

      "get legislationcode
      CLEAR land1.
      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0105>-pernr AND
                                                          begda LE <p0105>-endda AND
                                                          endda GE <p0105>-begda.

        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
        EXIT.
      ENDLOOP.

      split_phone_number_cogl( EXPORTING number   = <p0105>-usrid
                                         leg_code = CONV #( land1-value )
                               IMPORTING country_code = DATA(country_code)
                                         area_code    = DATA(area_code)
                                         phone_number = DATA(phone_number)
                                         extension    = DATA(extension) ).

      "in case no country code and phone number is provided, skip entry
      CHECK country_code IS NOT INITIAL AND
            phone_number IS NOT INITIAL.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  person_phone
                  src_id
                  sys_id
                  src_sys_id
                  begda_tmp
                  endda_tmp
                  ad_pn
                  phone_type
                  country_code
                  area_code
                  phone_number
                  extension
                  land1-value
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: country_code, area_code, phone_number, extension.
    ENDLOOP.
  ENDMETHOD.


METHOD proceed_cofu_person_phone.
  get_cofu_data( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


  METHOD proceed_cogl_person_phone.
    get_cogl_data( ).
    data = map_cogl_data( vp_src_id ).
  ENDMETHOD.


METHOD split_phone_number_cofu.
  DATA: number_tmp TYPE string.
  CLEAR: country_code, area_code, phone_number, extension.

  CHECK number IS NOT INITIAL.

  "fallback: pass country code in phone number
  country_code = number+0(3).
  phone_number = number.
  REPLACE ALL OCCURRENCES OF country_code IN phone_number WITH ''.

  "check SAP mandant
  CASE sy-mandt.
    WHEN zmhp_cl_int_constants=>cofu_mandant-germany.

      "for mobile phone pass fallback logic above
      CHECK phone_type NE 'WM'.

      READ TABLE vorw_udata ASSIGNING FIELD-SYMBOL(<vorw>) WITH KEY werks = werks
                                                                    btrtl = btrtl.
      CHECK <vorw> IS ASSIGNED.
      country_code = <vorw>-landesvorw.

      DATA(cc_tmp) = cl_hr_se_generic_tools=>get_country_predialing_code( leg_code ).

      REPLACE ALL OCCURRENCES OF '+' IN cc_tmp WITH ''.

      IF cc_tmp NE country_code AND
         cc_tmp IS NOT INITIAL.
        country_code = cc_tmp.
      ENDIF.

      area_code    = <vorw>-ortsvorw.
      phone_number = number.

    WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands OR
         zmhp_cl_int_constants=>cofu_mandant-austria     OR
         zmhp_cl_int_constants=>cofu_mandant-italy.

      split_phone_number_cogl( EXPORTING number   = number
                                         leg_code = leg_code
                               IMPORTING country_code = country_code
                                         area_code    = area_code
                                         phone_number = phone_number
                                         extension    = extension ).

  ENDCASE.

  REPLACE ALL OCCURRENCES OF '+' IN country_code WITH ''.

ENDMETHOD.


  METHOD split_phone_number_cogl.
    DATA: number_tmp TYPE string.
    TRY.
        country_code = segment( val = number index = 1 sep = '/' ).
        DATA(cc_tmp) = cl_hr_se_generic_tools=>get_country_predialing_code( leg_code ).

        IF cc_tmp NE country_code.
          country_code = cc_tmp.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '+' IN country_code WITH ''.

        area_code    = segment( val = number index = 2 sep = '/' ).
        phone_number = segment( val = number index = 3 sep = '/' ).
        extension    = segment( val = number index = 4 sep = '/' ).

        DATA(leg_special) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'CA' )
                                                ( sign = 'I' option = 'EQ' low = 'US' ) ).

        CHECK leg_code IN leg_special.

        "in case of Canada or USA area code has to have 3 digits and phone number 7 digits
        DATA(area_length) = strlen( area_code ).
        DATA(number_length) = strlen( phone_number ).
        IF strlen( area_code ) NE 3.
          IF area_length GT 3.
            area_length = area_length - 3.
            number_tmp = area_code+3(area_length).
            area_code = area_code+0(3).

            CONCATENATE number_tmp phone_number INTO phone_number.

          ELSEIF area_length LT 3.
            DATA(char_need) = 3 - area_length.
            DATA(phone_len) = strlen( phone_number ).

            IF number_length GE char_need.
              number_tmp = phone_number+0(char_need).

              DATA(number_len) = strlen( number_tmp ).
              phone_len = phone_len - number_len.
              phone_number = phone_number+char_need(phone_len).

              CONCATENATE area_code number_tmp INTO area_code.
            ENDIF.

          ENDIF.
        ENDIF.

        DATA(extension_length) = strlen( extension ).
        number_length = strlen( phone_number ).
        IF number_length NE 7.
          IF number_length GT 7.
            number_length = number_length - 7.
            number_tmp   = phone_number+7(number_length).
            phone_number = phone_number+0(7).

            CONCATENATE number_tmp extension INTO extension.
          ELSEIF number_length LT 7.
            DATA(chr_need)  = 7 - number_length.
            DATA(ext_len) = strlen( extension ).

            IF extension_length GE chr_need.
              number_tmp = extension+0(chr_need).

              DATA(num_len) = strlen( number_tmp ).
              ext_len = ext_len - num_len.
              extension = extension+char_need(ext_len).

              CONCATENATE phone_number number_tmp INTO phone_number.
            ENDIF.
          ENDIF.
        ENDIF.



      CATCH cx_sy_strg_par_val.
    ENDTRY.

  ENDMETHOD.


  METHOD update_begda_it0105.
    DATA: p0105_tmp TYPE p0105,
          pernr_old TYPE rsdsselopt_t.

    SORT p0001 BY pernr begda ASCENDING.

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
      CHECK <p0001>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "delete all records that are older than earliest assign entry
      DELETE p0105 WHERE pernr EQ <p0001>-pernr AND
                         endda LT <p0001>-begda.

      p0105_tmp-begda = <p0001>-begda.

      "set begin date to earliest assignment date
      MODIFY p0105 FROM p0105_tmp TRANSPORTING begda WHERE pernr EQ <p0001>-pernr AND
                                                           begda LT <p0001>-begda.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_old.

      CLEAR p0105_tmp.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
