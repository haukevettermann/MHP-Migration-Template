CLASS zmhp_cl_mig_salary DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF vp_wkr_id ,
        pernr  TYPE pernr,
        begda  TYPE begda,
        endda  TYPE endda,
        wkr_id TYPE string,
      END OF vp_wkr_id .
    TYPES:
      vp_wkr_id_t TYPE STANDARD TABLE OF vp_wkr_id .

    DATA pernr TYPE rsdsselopt_t .
    DATA begda TYPE begda .
    DATA endda TYPE endda .
    DATA cogl TYPE boolean .
    DATA cofu TYPE boolean .
    DATA molga TYPE rsdsselopt_t .
    DATA p0001 TYPE p0001_tab .
    DATA vp_salary_structure TYPE /iwbep/t_mgw_name_value_pair .
    CONSTANTS salary TYPE string VALUE 'Salary' ##NO_TEXT.
    DATA p0008 TYPE p0008_tab .
    DATA vp_wrk_id TYPE zmhp_cl_mig_work_relation=>vp_wkr_id_t .
    DATA pn_begda TYPE begda .
    DATA vp_salary_comp_structure TYPE /iwbep/t_mgw_name_value_pair .
    CONSTANTS salary_comp TYPE string VALUE 'SalarySimpleComponent' ##NO_TEXT.
    DATA hr_periods TYPE hrperiods_tab .

    METHODS proceed_cofu_salary
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
        !worker     TYPE REF TO zmhp_cl_mig_worker
      RETURNING
        VALUE(data) TYPE string .
    METHODS constructor
      IMPORTING
        !pernr    TYPE rsdsselopt_t
        !begda    TYPE begda
        !endda    TYPE endda
        !cofu     TYPE boolean
        !cogl     TYPE boolean
        !molga    TYPE rsdsselopt_t
        !cogu     TYPE boolean
        !pn_begda TYPE begda OPTIONAL .
    METHODS create_metadata
      RETURNING
        VALUE(metadata) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mapping_values_lgart TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapping .
    DATA mapping_fields_lgart TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapp_fi .
    DATA cogu TYPE boolean .

    METHODS collect_hr_periods
      IMPORTING
        !pernr TYPE pernr_d .
    METHODS map_cofu_data
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
      EXPORTING
        !data_comp  TYPE string
      RETURNING
        VALUE(data) TYPE string .
    METHODS map_mig_values
      IMPORTING
        !p0008         TYPE p0008
      EXPORTING
        !componentcode TYPE zmhp_dd_value .
    METHODS get_cofu_data .
    METHODS get_mapping_fields .
    METHODS get_mapping_values .
ENDCLASS.



CLASS ZMHP_CL_MIG_SALARY IMPLEMENTATION.


  METHOD collect_hr_periods.

    DATA(p0001_pernr) = p0001.
    DATA(p0008_pernr) = p0008.

    DELETE p0008_pernr WHERE pernr NE pernr.
    DELETE p0001_pernr WHERE pernr NE pernr.

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0001_pernr
                                       CHANGING  hr_periods = hr_periods ).

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0008_pernr
                                       CHANGING  hr_periods = hr_periods ).

  ENDMETHOD.


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
      vp_salary_structure = VALUE #( ).
    ELSEIF cofu EQ abap_true.
      vp_salary_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                       ( name = 2  value = salary )
                                       ( name = 3  value = 'AssignmentId(SourceSystemId)' )
                                       ( name = 4  value = 'DateFrom' )
                                       ( name = 5  value = 'DateTo' )
                                       ( name = 6  value = 'SalaryAmount' )
                                       ( name = 7  value = 'MultipleComponents' )
                                       ( name = 8  value = 'AssignmentNumber' )
                                       ( name = 9  value = 'SalaryBasisName' )
                                       ( name = 10 value = 'ActionCode' )
                                       ( name = 16 value = 'SourceSystemId' )
                                       ( name = 17 value = 'SourceSystemOwner' ) ).

      vp_salary_comp_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                          ( name = 2  value = salary_comp )
                                          ( name = 3  value = 'ComponentCode' )
                                          ( name = 4  value = 'Amount' )
                                          ( name = 5  value = 'Percentage' )
                                          ( name = 6  value = 'SalaryDateFrom' )
                                          ( name = 8  value = 'AssignmentNumber' ) ).
    ENDIF.
  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_salary_structure LINES DATA(length).

    LOOP AT vp_salary_structure ASSIGNING FIELD-SYMBOL(<salary_struc>).

      "set METADATA title
      CASE <salary_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <salary_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

    DESCRIBE TABLE vp_salary_comp_structure LINES length.

    LOOP AT vp_salary_comp_structure ASSIGNING FIELD-SYMBOL(<salary_comp_struc>).

      "set METADATA title
      CASE <salary_comp_struc>-name.
        WHEN 1.
          CONCATENATE metadata
                      cl_abap_char_utilities=>newline
                      zmhp_cl_mig_utils=>metadata
                      zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <salary_comp_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_cofu_data.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE @p0008 FROM pa0008 WHERE pernr IN @pernr AND
                                                                         begda LE @endda AND
                                                                         endda GE @begda.

    SELECT pernr,
           begda,
           endda,
           persk INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDMETHOD.


  METHOD get_mapping_fields.

    "get mapping fields for actioncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0008'
                                                     sap_field    = 'LGART'
                                                     oracle_field = zmhp_cl_mig_utils=>componentcode
                                                     export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_lgart ).
  ENDMETHOD.


  METHOD get_mapping_values.

    "get mapping values for actioncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = '0008'
                                                     sap_field    = 'LGART'
                                                     oracle_field = zmhp_cl_mig_utils=>componentcode
                                                     export       = abap_true
                                          IMPORTING mapping_values = mapping_values_lgart ).
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id           TYPE string,
          sys_id           TYPE string,
          salary_base_name TYPE string,
          count            TYPE i,
          betrg            TYPE maxbt,
          ppbwla           TYPE hreg_t_pbwla.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      CLEAR: hr_periods.
      collect_hr_periods( CONV #( <pernr>-low ) ).
      count = 1.

      LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<period>).
        LOOP AT p0008 ASSIGNING FIELD-SYMBOL(<p0008>) WHERE pernr EQ <pernr>-low    AND
                                                            begda LE <period>-endda AND
                                                            endda GE <period>-begda.
          EXIT.
        ENDLOOP.

        CHECK sy-subrc IS INITIAL.

        LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <pernr>-low    AND
                                                            begda LE <period>-endda AND
                                                            endda GE <period>-begda.
          EXIT.
        ENDLOOP.

        CALL FUNCTION 'RP_FILL_WAGE_TYPE_TABLE'
          EXPORTING
            begda  = <period>-begda
            endda  = <period>-endda
            pernr  = <p0008>-pernr
          TABLES
            ppbwla = ppbwla.

        CONCATENATE 'E' <p0008>-pernr INTO DATA(assign_number).

        DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <period>-begda ).
        DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <period>-endda ).

        DATA(comp_num) = lines( ppbwla ).
        DATA(multiple) = 'N'.
        IF comp_num GT 1.
          multiple = 'Y'.
        ENDIF.

        "build components structure
        LOOP AT ppbwla ASSIGNING FIELD-SYMBOL(<ppbwla>).
          <p0008>-lga01 = <ppbwla>-lgart.

          map_mig_values( EXPORTING p0008 = <p0008>
                          IMPORTING componentcode  = DATA(componentcode) ).

          betrg = betrg + <ppbwla>-betrg.

          DATA(datum_comp) = zmhp_cl_mig_utils=>convert_date( <ppbwla>-begda ).
          DATA(amount) = CONV string( <ppbwla>-betrg ).

          CONDENSE: amount.

          CONCATENATE zmhp_cl_mig_utils=>merge
                      salary_comp
                      componentcode
                      amount
                      '100'
                      datum_comp
                      assign_number
                      INTO DATA(data_comp_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.
          CONCATENATE data_comp cl_abap_char_utilities=>newline data_comp_tmp INTO data_comp.
        ENDLOOP.

        CASE sy-mandt.
          WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands.

            salary_base_name = SWITCH #( <p0001>-persk
                                         WHEN '10' THEN 'NL_Employee'
                                         WHEN '12' THEN 'NL_Trainee/Stagiaires' ).

            salary_base_name = SWITCH #( <p0001>-pernr
                                         WHEN '00020037' THEN 'NL_Director'
                                         ELSE salary_base_name ).

          WHEN zmhp_cl_int_constants=>cofu_mandant-austria.
            IF '03' IN molga.
              salary_base_name = SWITCH #( <p0001>-persk
                                            WHEN '1B' OR 'A1' THEN 'AT_Wage'
                                            WHEN 'A3'         THEN 'AT_Pension'
                                            ELSE 'AT_Salary' ).
            ENDIF.

        ENDCASE.

        DATA(count_s) = CONV string( count ).
        CONDENSE count_s.
        src_id = salary_base_name && '_' && assign_number && '_' && count_s.
        DATA(sum) = CONV string( betrg ).
        CONDENSE sum.

        CONCATENATE zmhp_cl_mig_utils=>merge
                    salary
                    ''
                    begda_tmp
                    endda_tmp
                    sum
                    multiple
                    assign_number
                    salary_base_name
                    'HIRE'
                    src_id
                    sys_id
        INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.
        CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
        CLEAR: sum, betrg.
        count = count + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_mig_values.
    DATA: value_tmp TYPE zmhp_dd_value.

    value_tmp = p0008-lga01.
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0008'
        field_sap      = 'LGART'
        field_oracle   = zmhp_cl_mig_utils=>componentcode
        mapping_fields = CONV #( mapping_fields_lgart )
        mapping_values = CONV #( mapping_values_lgart )
      CHANGING
        value          = value_tmp ).

    componentcode = value_tmp.
  ENDMETHOD.


  METHOD proceed_cofu_salary.
    DATA: data_comp TYPE string.

    get_cofu_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).

    data = map_cofu_data( EXPORTING vp_src_id = vp_src_id
                          IMPORTING data_comp = data_comp ).
    CONCATENATE data
                cl_abap_char_utilities=>newline
                data_comp
                INTO data.
  ENDMETHOD.
ENDCLASS.
