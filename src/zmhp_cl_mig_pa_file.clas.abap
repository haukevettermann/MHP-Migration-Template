CLASS zmhp_cl_mig_pa_file DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS de_file TYPE char255 VALUE 'Worker_DE.zip' ##NO_TEXT.
    CONSTANTS at_file TYPE char255 VALUE 'Worker_AT.zip' ##NO_TEXT.
    CONSTANTS it_file TYPE char255 VALUE 'Worker_IT.zip' ##NO_TEXT.
    CONSTANTS fr_file TYPE char255 VALUE 'Worker_FR.zip' ##NO_TEXT.
    CONSTANTS nl_file TYPE char255 VALUE 'Worker_NL.zip' ##NO_TEXT.
    DATA contract TYPE REF TO zmhp_cl_mig_contract .
    DATA pn_begda TYPE begda .
    CONSTANTS au_file TYPE char255 VALUE 'Worker_AU.zip' ##NO_TEXT.
    CONSTANTS nz_file TYPE char255 VALUE 'Worker_NZ.zip' ##NO_TEXT.

    METHODS download_files
      IMPORTING
        !files TYPE /iwbep/t_mgw_name_value_pair .
    METHODS constructor
      IMPORTING
        !cogl     TYPE boolean
        !cofu     TYPE boolean
        !pernr    TYPE rsdsselopt_t
        !begda    TYPE begda
        !endda    TYPE endda
        !pic_path TYPE string
        !cogu     TYPE boolean
        !al11     TYPE boolean
        !zip      TYPE REF TO cl_abap_zip
        !pn_begda TYPE begda .
    METHODS proceed_file_construction
      EXPORTING
        !supervisor    TYPE string
        !term          TYPE string
        !user          TYPE string
        !account       TYPE string
        !allocation    TYPE string
        !pay           TYPE string
        !external      TYPE string
        !disability    TYPE string
        !contact       TYPE string
        !salary        TYPE string
      RETURNING
        VALUE(content) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA assignment TYPE REF TO zmhp_cl_mig_assignment .
    DATA assignment_man TYPE REF TO zmhp_cl_mig_assignment_man .
    DATA person_citizenship TYPE REF TO zmhp_cl_mig_person_citizenship .
    DATA external_ident TYPE REF TO zmhp_cl_mig_external_ident .
    DATA person_email TYPE REF TO zmhp_cl_mig_person_email .
    DATA person_leg_data TYPE REF TO zmhp_cl_mig_person_leg_data .
    DATA person_name TYPE REF TO zmhp_cl_mig_person_name .
    DATA person_phone TYPE REF TO zmhp_cl_mig_person_phone .
    DATA worker TYPE REF TO zmhp_cl_mig_worker .
    DATA work_relation TYPE REF TO zmhp_cl_mig_work_relation .
    DATA cofu TYPE boolean .
    DATA cogl TYPE boolean .
    DATA pernr TYPE pernr_tab .
    DATA molga TYPE rsdsselopt_t .
    DATA begda TYPE begda .
    DATA endda TYPE endda .
    DATA workterms TYPE REF TO zmhp_cl_mig_work_terms .
    DATA user TYPE REF TO zmhp_cl_mig_user .
    DATA account TYPE REF TO zmhp_cl_mig_cost_account .
    DATA allocation TYPE REF TO zmhp_cl_mig_cost_allocation .
    DATA extra_info TYPE REF TO zmhp_cl_mig_extra_info .
    DATA person_image TYPE REF TO zmhp_cl_mig_person_image .
    DATA person_pay TYPE REF TO zmhp_cl_mig_person_pay_method .
    CONSTANTS calc_fte TYPE string VALUE 'SET CALCULATE_FTE Y' ##NO_TEXT.
    DATA cogu TYPE boolean .
    DATA work_measure TYPE REF TO zmhp_cl_mig_work_measure .
    DATA al11 TYPE boolean .
    DATA zip TYPE REF TO cl_abap_zip .
    DATA person_address TYPE REF TO zmhp_cl_mig_person_address .
    DATA natio_identifier TYPE REF TO zmhp_cl_mig_natio_identifier .
    DATA passport TYPE REF TO zmhp_cl_mig_passport .
    DATA visa TYPE REF TO zmhp_cl_mig_person_visa .
    DATA disability TYPE REF TO zmhp_cl_mig_person_disability .
    DATA drivers_licence TYPE REF TO zmhp_cl_mig_drivers_licence .
    DATA assign_ext_inf TYPE REF TO zmhp_cl_mig_assign_extra_info .
    DATA contact TYPE REF TO zmhp_cl_mig_contact .
    DATA contact_leg_data TYPE REF TO zmhp_cl_mig_contact_leg_data .
    DATA contact_name TYPE REF TO zmhp_cl_mig_contact_name .
    DATA contact_rel TYPE REF TO zmhp_cl_mig_contact_relation .
    DATA salary TYPE REF TO zmhp_cl_mig_salary .

    METHODS get_molga .
    METHODS build_output
      IMPORTING
        !metadata      TYPE string
        !data          TYPE string
      RETURNING
        VALUE(content) TYPE string .
    METHODS get_metadata
      EXPORTING
        !meta_super      TYPE string
        !meta_term       TYPE string
        !meta_user       TYPE string
        !meta_account    TYPE string
        !meta_allocation TYPE string
        !meta_pay        TYPE string
        !meta_external   TYPE string
        !meta_disability TYPE string
        !meta_contact    TYPE string
        !meta_salary     TYPE string
      RETURNING
        VALUE(metadata)  TYPE string .
    METHODS get_data
      EXPORTING
        !data_super      TYPE string
        !data_term       TYPE string
        !data_user       TYPE string
        !data_account    TYPE string
        !data_allocation TYPE string
        !data_pay        TYPE string
        !data_disability TYPE string
        !data_external   TYPE string
        !data_contact    TYPE string
        !data_salary     TYPE string
      RETURNING
        VALUE(data)      TYPE string .
    METHODS get_cogl_metadata
      EXPORTING
        !meta_super      TYPE string
        !meta_term       TYPE string
        !meta_user       TYPE string
        !meta_account    TYPE string
        !meta_allocation TYPE string
        !meta_external   TYPE string
      RETURNING
        VALUE(metadata)  TYPE string .
    METHODS get_cofu_metadata
      EXPORTING
        !meta_super      TYPE string
        VALUE(meta_term) TYPE string
        !meta_user       TYPE string
        !meta_account    TYPE string
        !meta_allocation TYPE string
        !meta_pay        TYPE string
        !meta_external   TYPE string
        !meta_disability TYPE string
        !meta_contact    TYPE string
        !meta_salary     TYPE string
      RETURNING
        VALUE(metadata)  TYPE string .
    METHODS get_cogu_metadata
      EXPORTING
        !meta_super      TYPE string
        VALUE(meta_term) TYPE string
        !meta_user       TYPE string
        !meta_account    TYPE string
        !meta_allocation TYPE string
        !meta_pay        TYPE string
        !meta_external   TYPE string
      RETURNING
        VALUE(metadata)  TYPE string .
    METHODS get_cofu_data
      EXPORTING
        !data_super      TYPE string
        VALUE(data_term) TYPE string
        !data_user       TYPE string
        !data_account    TYPE string
        !data_allocation TYPE string
        !data_pay        TYPE string
        !data_external   TYPE string
        !data_disability TYPE string
        !data_contact    TYPE string
        !data_salary     TYPE string
      RETURNING
        VALUE(data)      TYPE string .
    METHODS get_cogu_data
      EXPORTING
        !data_super      TYPE string
        VALUE(data_term) TYPE string
        !data_user       TYPE string
        !data_account    TYPE string
        !data_allocation TYPE string
        !data_pay        TYPE string
        !data_external   TYPE string
      RETURNING
        VALUE(data)      TYPE string .
    METHODS get_cogl_data
      EXPORTING
        !data_super      TYPE string
        !data_term       TYPE string
        !data_user       TYPE string
        !data_account    TYPE string
        !data_allocation TYPE string
        !data_external   TYPE string
      RETURNING
        VALUE(data)      TYPE string .
ENDCLASS.



CLASS ZMHP_CL_MIG_PA_FILE IMPLEMENTATION.


  METHOD build_output.
    CONCATENATE metadata data INTO content SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.


  METHOD constructor.
    me->pernr = pernr.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->begda = begda.
    me->pn_begda = pn_begda.  "JMB20211011 I - C400129651-5882
    me->endda = endda.
    me->al11  = al11.
    me->zip   = zip.

    "get relevant molga
    get_molga( ).

    assignment      = NEW zmhp_cl_mig_assignment( pernr = pernr
                                                  begda = begda
                                                  endda = endda
                                                  cogl  = cogl
                                                  cofu  = cofu
                                                  cogu  = cogu
                                                  molga = molga ).

    contract        = NEW zmhp_cl_mig_contract( pernr = pernr
                                                begda = begda
                                                endda = endda
                                                cogl  = cogl
                                                cofu  = cofu
                                                cogu  = cogu
                                                molga = molga ).

    work_measure    = NEW zmhp_cl_mig_work_measure( pernr = pernr
                                                    begda = begda
                                                    endda = endda
                                                    cogl  = cogl
                                                    cofu  = cofu
                                                    cogu  = cogu
                                                    molga = molga ).

    workterms      = NEW zmhp_cl_mig_work_terms( pernr = pernr
                                                 begda = begda
                                                 endda = endda
                                                 cogl  = cogl
                                                 cofu  = cofu
                                                 cogu  = cogu
                                                 molga = molga ).

    external_ident  = NEW zmhp_cl_mig_external_ident( pernr = pernr
                                                      begda = sy-datum  "there can only be passed on entry per ExternalIdentifiert
                                                      endda = sy-datum  "due to it isn´t date-effective in Oracle
                                                      cogl  = cogl
                                                      cofu  = cofu
                                                      cogu  = cogu
                                                      molga = molga ).

    extra_info  = NEW zmhp_cl_mig_extra_info( pernr = pernr
                                              begda = sy-datum
                                              endda = endda "JMB20210812 I - sy-datum not working for future hires
                                              cogl  = cogl
                                              cofu  = cofu
                                              cogu  = cogu
                                              molga = molga ).

    person_email    = NEW zmhp_cl_mig_person_email( pernr = pernr
                                                    begda = begda
                                                    endda = endda
                                                    cogl  = cogl
                                                    cofu  = cofu
                                                    cogu  = cogu
                                                    molga = molga ).

    person_leg_data = NEW zmhp_cl_mig_person_leg_data( pernr = pernr
                                                       begda = begda
                                                       endda = endda
                                                       cogl  = cogl
                                                       cofu  = cofu
                                                       cogu  = cogu
                                                       molga = molga ).

    person_name     = NEW zmhp_cl_mig_person_name( pernr = pernr
                                                   begda = begda
                                                   endda = endda
                                                   cogl  = cogl
                                                   cofu  = cofu
                                                   cogu  = cogu
                                                   molga = molga ).

    person_phone    = NEW zmhp_cl_mig_person_phone( pernr = pernr
                                                    begda = begda
                                                    endda = endda
                                                    cogl  = cogl
                                                    cofu  = cofu
                                                    cogu  = cogu
                                                    molga = molga ).

    worker          = NEW zmhp_cl_mig_worker( pernr = pernr
                                              begda = begda
                                              endda = endda
                                              cogl  = cogl
                                              cofu  = cofu
                                              cogu  = cogu
                                              molga = molga ).

    work_relation   = NEW zmhp_cl_mig_work_relation( pernr = pernr
                                                     pn_begda = pn_begda "JMB20211011 I - C400129651-5882
                                                     begda = begda
                                                     endda = endda
                                                     cogl  = cogl
                                                     cofu  = cofu
                                                     cogu  = cogu
                                                     molga = molga ).

    person_citizenship      = NEW zmhp_cl_mig_person_citizenship( pernr = pernr
                                                                  begda = begda
                                                                  endda = endda
                                                                  cogl  = cogl
                                                                  cofu  = cofu
                                                                  cogu  = cogu
                                                                  molga = molga ).

    person_address  = NEW zmhp_cl_mig_person_address( pernr = pernr "BS20211021 added
                                                      begda = begda
                                                      endda = endda
                                                      cogl  = cogl
                                                      cofu  = cofu
                                                      cogu  = cogu
                                                      molga = molga ).

    assignment_man  = NEW zmhp_cl_mig_assignment_man( pernr = pernr
                                                      begda = begda
                                                      endda = endda
                                                      cogl  = cogl
                                                      cofu  = cofu
                                                      cogu  = cogu
                                                      molga = molga ).

    user      = NEW zmhp_cl_mig_user( pernr = pernr
                                      begda = sy-datum "JMB20211021 I - C400129651-5988
                                      endda = sy-datum "JMB20211021 I - C400129651-5988
                                      cogl  = cogl
                                      cofu  = cofu
                                      cogu  = cogu
                                      molga = molga ).

    account    = NEW zmhp_cl_mig_cost_account( pernr = pernr
                                               begda = sy-datum
                                               endda = endda
                                               cogl  = cogl
                                               cofu  = cofu
                                               cogu  = cogu
                                               molga = molga ).

    allocation = NEW zmhp_cl_mig_cost_allocation( pernr = pernr
                                                  begda = sy-datum
                                                  endda = endda
                                                  cogl  = cogl
                                                  cofu  = cofu
                                                  cogu  = cogu
                                                  molga = molga ).

    person_image = NEW zmhp_cl_mig_person_image( pernr = pernr
                                                 path  = pic_path
                                                 begda = begda
                                                 endda = endda
                                                 cogl  = cogl
                                                 cofu  = cofu
                                                 cogu  = cogu
                                                 molga = molga
                                                 zip   = zip
                                                 al11  = al11   ).

    person_pay = NEW zmhp_cl_mig_person_pay_method( pernr = pernr
                                                    begda = begda
                                                    endda = endda
                                                    cogl  = cogl
                                                    cofu  = cofu
                                                    cogu  = cogu
                                                    molga = molga ).

    natio_identifier = NEW zmhp_cl_mig_natio_identifier( pernr = pernr  "IFT20211026 I
                                                         begda = begda
                                                         endda = endda
                                                         cogl  = cogl
                                                         cofu  = cofu
                                                         cogu  = cogu
                                                         molga = molga ).


    drivers_licence = NEW zmhp_cl_mig_drivers_licence( pernr = pernr  "BS20211029 I
                                                       begda = begda
                                                       endda = endda
                                                       cogl  = cogl
                                                       cofu  = cofu
                                                       cogu  = cogu
                                                       molga = molga ).

    passport = NEW zmhp_cl_mig_passport( pernr = pernr  "BS20211027 I
                                         begda = begda
                                         endda = endda
                                         cogl  = cogl
                                         cofu  = cofu
                                         cogu  = cogu
                                         molga = molga ).

    visa = NEW zmhp_cl_mig_person_visa( pernr = pernr  "IFT20211029 I
                                        begda = begda
                                        endda = endda
                                        cogl  = cogl
                                        cofu  = cofu
                                        cogu  = cogu
                                        molga = molga ).

    disability = NEW zmhp_cl_mig_person_disability( pernr = pernr  "IFT20211029 I
                                                    begda = begda
                                                    endda = endda
                                                    cogl  = cogl
                                                    cofu  = cofu
                                                    cogu  = cogu
                                                    molga = molga ).

    assign_ext_inf = NEW zmhp_cl_mig_assign_extra_info( pernr = pernr
                                                        begda = pn_begda
                                                        endda = endda
                                                        cogl  = cogl
                                                        cofu  = cofu
                                                        cogu  = cogu
                                                        molga = molga ).

    salary   = NEW zmhp_cl_mig_salary( pernr = pernr
                                       pn_begda = pn_begda "JMB20211011 I - C400129651-5882
                                       begda = begda
                                       endda = endda
                                       cogl  = cogl
                                       cofu  = cofu
                                       cogu  = cogu
                                       molga = molga ).

**IFT20211109 Start Insert
*
    contact = NEW zmhp_cl_mig_contact( pernr = pernr
                                       begda = begda
                                       endda = endda
                                       cogl  = cogl
                                       cofu  = cofu
                                       cogu  = cogu
                                       molga = molga ).

    contact_leg_data = NEW zmhp_cl_mig_contact_leg_data( pernr = pernr
                                                         begda = begda
                                                         endda = endda
                                                         cogl  = cogl
                                                         cofu  = cofu
                                                         cogu  = cogu
                                                         molga = molga ).
    contact_name = NEW zmhp_cl_mig_contact_name( pernr = pernr
                                                 begda = begda
                                                 endda = endda
                                                 cogl  = cogl
                                                 cofu  = cofu
                                                 cogu  = cogu
                                                 molga = molga ).
    contact_rel = NEW zmhp_cl_mig_contact_relation( pernr = pernr
                                                    begda = begda
                                                    endda = endda
                                                    cogl  = cogl
                                                    cofu  = cofu
                                                    cogu  = cogu
                                                    molga = molga ).
*IFT20211109 End Insert
  ENDMETHOD.


  METHOD download_files.

    DATA: content   TYPE stringtab,
          filename  TYPE string,
          content_x TYPE xstring,
          zip_file  TYPE string VALUE 'Worker_XX.zip',
          zip_tab   TYPE swxmlcont.

    CASE al11.
      WHEN abap_true.
        LOOP AT files ASSIGNING FIELD-SYMBOL(<files_zip>).
          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text   = <files_zip>-value
            IMPORTING
              buffer = content_x.

          zip->add( name    = <files_zip>-name
                    content = content_x ).
        ENDLOOP.

        DATA(zip_xstring) =  zip->save( ).

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer     = zip_xstring
          TABLES
            binary_tab = zip_tab.

        zip_file = SWITCH #( sy-mandt
                             WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN fr_file
                             WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN de_file
                             WHEN zmhp_cl_int_constants=>cofu_mandant-australia   THEN au_file
                             WHEN zmhp_cl_int_constants=>cofu_mandant-newzealand  THEN nz_file
                             WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN nl_file ).

        "in case of Italy and Autria (same mandant)
        LOOP AT molga ASSIGNING FIELD-SYMBOL(<molga>).
          zip_file        = SWITCH string( <molga>-low
                                           WHEN '03' THEN at_file
                                           WHEN '15' THEN it_file
                                           ELSE zip_file ).
          EXIT.
        ENDLOOP.

        DATA(logical_filename) = CONV filename-fileintern('ZMHP_HCM_ORACLE_EXTRACTS_COGU').
        CALL FUNCTION 'FILE_GET_NAME'
          EXPORTING
            logical_filename = logical_filename
            parameter_1      = zip_file
          IMPORTING
            file_name        = filename
          EXCEPTIONS
            file_not_found   = 1
            OTHERS           = 2.

        CHECK filename IS NOT INITIAL.

        OPEN DATASET filename FOR OUTPUT IN BINARY MODE.
        CHECK sy-subrc IS INITIAL.

        LOOP AT zip_tab ASSIGNING FIELD-SYMBOL(<zip_tab>).
          TRANSFER <zip_tab> TO filename.
        ENDLOOP.
        CLOSE DATASET filename.
      WHEN abap_false.

        LOOP AT files ASSIGNING FIELD-SYMBOL(<files>).
          APPEND <files>-value TO content.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              filename = <files>-name
              filetype = 'DAT'
              codepage = '4110' "UTF-8
            TABLES
              data_tab = content.
          CLEAR: content.
        ENDLOOP.
    ENDCASE.

  ENDMETHOD.


  METHOD get_cofu_data.
    DATA: data_term_tmp   TYPE string,
          data_assign_man TYPE string,
          data_assign_ext TYPE string,
          vp_src_id       TYPE /iwbep/t_mgw_name_value_pair,
          vp_wkr_id       TYPE zmhp_cl_mig_work_relation=>vp_wkr_id_t,
          vp_wterm_id     TYPE zmhp_cl_mig_work_terms=>vp_wterm_id_t.

    DATA(data_work)       = worker->proceed_cofu_worker( IMPORTING vp_src_id = vp_src_id ).
    DATA(data_ext)        = external_ident->proceed_cofu_external_ident( worker       = worker
                                                                         vp_src_id    = vp_src_id
                                                                         vp_lcl_pernr = worker->vp_lcl_pernr ).
    DATA(data_ext_info)   = extra_info->proceed_cofu_extra_info( worker       = worker
                                                                 vp_src_id    = vp_src_id ).
    DATA(data_eml)        = person_email->proceed_cofu_person_email( worker    = worker
                                                                     vp_src_id = vp_src_id ).
    DATA(data_leg)        = person_leg_data->proceed_cofu_person_leg_data( worker    = worker
                                                                           vp_src_id = vp_src_id ).
    DATA(data_nme)        = person_name->proceed_cofu_person_name( worker    = worker
                                                                   vp_src_id = vp_src_id ).
    DATA(data_phn)        = person_phone->proceed_cofu_person_phone( worker    = worker
                                                                     vp_src_id = vp_src_id ).
    DATA(data_wkr)        = work_relation->proceed_cofu_work_relation( EXPORTING worker    = worker
                                                                                 vp_src_id = vp_src_id
                                                                       IMPORTING vp_wkr_id = vp_wkr_id
                                                                                 data_term = data_term_tmp ).
**JMB20211129 start delete - WorkTerms will be created in Assignment class
*
*  DATA(data_workterms)  = workterms->proceed_cofu_work_terms( EXPORTING worker      = worker
*                                                                        vp_src_id   = vp_src_id
*                                                                        vp_wkr_id   = vp_wkr_id
*                                                              IMPORTING vp_wterm_id = vp_wterm_id ).
*JMB20211129 delete end
    DATA(data_assign)     = assignment->proceed_cofu_assignment( EXPORTING   worker      = worker
                                                                             vp_src_id   = vp_src_id
                                                                             vp_wkr_id   = vp_wkr_id
                                                                             vp_wterm_id = vp_wterm_id
                                                                 IMPORTING   data_assign_man = data_assign_man ).  "JMB20211130 I

    DATA(data_addr)       = person_address->proceed_cofu_person_address( vp_src_id = vp_src_id "BS20211021 I
                                                                         worker    = worker ).

    DATA(data_cs)         = person_citizenship->proceed_cofu_per_citizenship( vp_src_id = vp_src_id
                                                                              worker    = worker ).

**JMB20211130 start delete - pass assignmentManager from Assignment entity
*
*  DATA(data_assign_man) = assignment_man->proceed_cofu_assign_manager( worker    = worker
*                                                                       vp_src_id = vp_src_id ).
*JMB20211130 delete end

    data_user             = user->proceed_cofu_user( worker    = worker
                                                     vp_src_id = vp_src_id ).

    data_account          = account->proceed_cofu_cost_account( worker    = worker
                                                                vp_src_id = vp_src_id ).

    data_allocation       = allocation->proceed_cofu_cost_allocation( worker    = worker
                                                                      vp_src_id = vp_src_id ).
    DATA(data_ft) = person_image->proceed_cogl_person_image( vp_src_id = vp_src_id ).

    data_pay          = person_pay->proceed_cofu_person_pay_method( worker    = worker
                                                                    vp_src_id = vp_src_id ).

    DATA(data_natio)      = natio_identifier->proceed_cofu_natio_identifier( worker    = worker "IFT20211016 I
                                                                             vp_src_id = vp_src_id ).

    DATA(data_drivers_licence)       = drivers_licence->proceed_cofu_drivers_licence( vp_src_id = vp_src_id "BS20211029 I
                                                                                      worker    = worker ).

    DATA(data_passport)       = passport->proceed_cofu_passport( vp_src_id = vp_src_id "BS20211027 I
                                                                 worker    = worker ).

    DATA(data_visa) = visa->proceed_cofu_per_visa( vp_src_id = vp_src_id "IFT20211029 I
                                                   worker    = worker ).

    data_disability = disability->proceed_cofu_per_disability( vp_src_id = vp_src_id
                                                               worker    = worker ).

**JMB20211130 start delete - AssignmentExtraInfo will be created in Assignment class
*
*  DATA(data_assign_ext) = assign_ext_inf->proceed_cofu_extra_info( worker    = worker
*                                                                   vp_src_id = vp_src_id ).
*JMB20211130 delete end

**IFT20211109 Start Insert
*
    data_contact = contact->proceed_cofu_per_contact( worker    = worker
                                                      vp_src_id = vp_src_id ).
    DATA(data_contact_leg_data) = contact_leg_data->proceed_cofu_con_leg_data( worker    = worker
                                                                               vp_src_id = vp_src_id ).

    DATA(data_contact_name) = contact_name->proceed_cofu_con_name( worker    = worker
                                                                   vp_src_id = vp_src_id ).

    DATA(data_contact_rel) = contact_rel->proceed_cofu_con_relationship( worker    = worker
                                                                         vp_src_id = vp_src_id ).
*IFT20211109 End Insert

    data_salary             = salary->proceed_cofu_salary( worker    = worker
                                                           vp_src_id = vp_src_id ).

    CONCATENATE data_work
                data_eml
*              data_ext   "JMB20210712 D - Extract into own file
                data_ext_info
                data_leg
                data_nme
                data_phn
                data_wkr
*              data_workterms "JMB20211129 D - In data_assign
                data_assign
                data_addr "BS20211021 I
                data_cs
                data_drivers_licence "BS20211029 I
                data_passport "BS20211027 I
                data_natio "IFT20211026 I
                data_ft
                data_visa "IFT20211029 I
*              data_assign_ext    "JMB20211130 D - In data_assign
    INTO        data SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE data_work
                data_assign_man
    INTO        data_super SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE data_work
                data_ext
    INTO        data_external SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE data_disability
                '' " Placeholder DO NOT USE
    INTO        data_disability SEPARATED BY cl_abap_char_utilities=>newline.

**IFT20211109 Start insert
*
    CONCATENATE data_contact
                data_contact_leg_data
                data_contact_name
                data_contact_rel
    INTO data_contact SEPARATED BY cl_abap_char_utilities=>newline.
*IFT20211109 End insert

    data_term = data_term_tmp.
  ENDMETHOD.


  METHOD get_cofu_metadata.
    DATA(meta_work)       = worker->create_metadata( ).
    DATA(meta_assign)     = assignment->create_metadata( ).
    DATA(meta_workterms)  = workterms->create_metadata( ).
    DATA(meta_contract)   = contract->create_metadata( ).
    DATA(meta_ext)        = external_ident->create_metadata( ).
    DATA(meta_ext_info)   = extra_info->create_metadata( ).
    DATA(meta_eml)        = person_email->create_metadata( ).
    DATA(meta_leg)        = person_leg_data->create_metadata( ).
    DATA(meta_nme)        = person_name->create_metadata( ).
    DATA(meta_phn)        = person_phone->create_metadata( ).
    DATA(meta_wkr)        = work_relation->create_metadata( ).
    DATA(meta_cs)         = person_citizenship->create_metadata( ).
    DATA(meta_addr)       = person_address->create_metadata( ). "BS20211022 added
    DATA(meta_assign_man) = assignment_man->create_metadata( ).
    meta_user             = user->create_metadata( ).
    meta_account          = account->create_metadata( ).
    meta_allocation       = allocation->create_metadata( ).
    DATA(meta_image)      = person_image->create_metadata( ).
    meta_pay              = person_pay->create_metadata( ).
    DATA(meta_natio)      = natio_identifier->create_metadata( ). "IFT20211026 I
    DATA(meta_driv_licen) = drivers_licence->create_metadata( ). "BS20211029 I
    DATA(meta_passport)   = passport->create_metadata( ). "BS20211027 I
    DATA(meta_visa)       = visa->create_metadata( ). "IFT202011029 I
    meta_disability       = disability->create_metadata( ). "IFT202011029 I
    DATA(meta_assign_ext_inf) = assign_ext_inf->create_metadata( ). "JMB20211108 I
    meta_contact          = contact->create_metadata( ). "IFT20211109 I
    DATA(meta_contact_leg_data) = contact_leg_data->create_metadata( ). "IFT20211109 I
    DATA(meta_contact_name) = contact_name->create_metadata( ). "IFT20211109 I
    DATA(meta_contact_rel) = contact_rel->create_metadata( ). "IFT20211109 I
    meta_salary = salary->create_metadata( ).

    CONCATENATE meta_work
                meta_eml
*              meta_ext    "JMB20210712 D - Extract into own file
                meta_ext_info
                meta_leg
                meta_nme
                meta_phn
                meta_wkr
                meta_workterms
                meta_assign
                meta_contract
                meta_cs
                meta_addr "BS20211022 added
                meta_natio "IFT20211026 I
                meta_driv_licen "BS20211029 I
                meta_passport "BS20211027 I
                meta_image
                meta_visa "IFT20211029 I
                meta_assign_ext_inf "JMB20211108 I
    INTO        metadata SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE meta_work
                meta_assign_man
    INTO        meta_super  SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE meta_work
                meta_ext
    INTO        meta_external SEPARATED BY cl_abap_char_utilities=>newline.


**IFT20211123 Start delete
*
***IFT20211105 Start insert
**
*  CONCATENATE meta_work
*              meta_disability
*  INTO        meta_disability SEPARATED BY cl_abap_char_utilities=>newline.
**IFT20211109 End insert
*IFT20211123 End delete
**IFT20211109 Start insert
*
    CONCATENATE meta_contact
                meta_contact_leg_data
                meta_contact_name
                meta_contact_rel
    INTO        meta_contact SEPARATED BY cl_abap_char_utilities=>newline.
*IFT20211109 End insert
    meta_term = meta_wkr.
  ENDMETHOD.


  METHOD get_cogl_data.
    DATA: data_term_tmp     TYPE string,
          data_work_measure TYPE string,
          vp_src_id         TYPE /iwbep/t_mgw_name_value_pair,
          vp_wkr_id         TYPE zmhp_cl_mig_work_relation=>vp_wkr_id_t,
          vp_wterm_id       TYPE zmhp_cl_mig_work_terms=>vp_wterm_id_t.

    DATA(data_work)       = worker->proceed_cogl_worker( IMPORTING vp_src_id = vp_src_id ).
    DATA(data_ext)        = external_ident->proceed_cogl_external_ident( worker       = worker
                                                                         vp_src_id    = vp_src_id
                                                                         vp_lcl_pernr = worker->vp_lcl_pernr ).
    DATA(data_ext_info)   = extra_info->proceed_cogl_extra_info( worker       = worker
                                                                 vp_src_id    = vp_src_id ).
    DATA(data_eml)        = person_email->proceed_cogl_person_email( worker    = worker
                                                                     vp_src_id = vp_src_id ).
    DATA(data_leg)        = person_leg_data->proceed_cogl_person_leg_data( worker    = worker
                                                                           vp_src_id = vp_src_id ).
    DATA(data_nme)        = person_name->proceed_cogl_person_name( worker    = worker
                                                                   vp_src_id = vp_src_id ).
    DATA(data_phn)        = person_phone->proceed_cogl_person_phone( worker    = worker
                                                                     vp_src_id = vp_src_id ).
    DATA(data_wkr)        = work_relation->proceed_cogl_work_relation( EXPORTING worker    = worker
                                                                                 vp_src_id = vp_src_id
                                                                       IMPORTING vp_wkr_id = vp_wkr_id
                                                                                 data_term = data_term_tmp ).
    DATA(data_workterms)  = workterms->proceed_cogl_work_terms( EXPORTING worker      = worker
                                                                          vp_src_id   = vp_src_id
                                                                          vp_wkr_id   = vp_wkr_id
                                                                IMPORTING vp_wterm_id = vp_wterm_id ).
    DATA(data_assign)     = assignment->proceed_cogl_assignment( EXPORTING worker    = worker
                                                                           workterms = workterms
                                                                           vp_src_id = vp_src_id
                                                                           vp_wkr_id = vp_wkr_id
                                                                           vp_wterm_id = vp_wterm_id
                                                                 IMPORTING work_measure = data_work_measure ).  "JMB20210722 I

*  DATA(data_cs)         = person_citizenship->proceed_cogl_per_citizenship( vp_src_id = vp_src_id
*                                                                            worker    = worker ). "JMB20210323 D - Not needed for COGL
    DATA(data_assign_man) = assignment_man->proceed_cogl_assign_manager( worker    = worker
                                                                         workterms = workterms
                                                                         vp_src_id = vp_src_id ).

    data_user             = user->proceed_cogl_user( worker    = worker
                                                     vp_src_id = vp_src_id ).

    data_account          = account->proceed_cogl_cost_account( worker    = worker
                                                                vp_src_id = vp_src_id ).

    data_allocation       = allocation->proceed_cogl_cost_allocation( worker    = worker
                                                                      vp_src_id = vp_src_id ).

    DATA(data_ft) = person_image->proceed_cogl_person_image( vp_src_id = vp_src_id ).

    CONCATENATE data_work
                data_eml
*              data_ext   "JMB20210712 D - Extract into own file
                data_ext_info
                data_leg
                data_nme
                data_phn
                data_wkr
                data_workterms
                data_assign
                data_work_measure "JMB20210722 I - pass workmeasure to calculate headcount
*              data_cs  "JMB20210323 D - Not needed for COGL
                data_ft
    INTO        data SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE data_work
                data_assign_man
    INTO        data_super SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE data_work
                data_ext
    INTO        data_external SEPARATED BY cl_abap_char_utilities=>newline.

    data_term = data_term_tmp.
  ENDMETHOD.


  METHOD get_cogl_metadata.
    DATA(meta_work)       = worker->create_metadata( ).
    DATA(meta_assign)     = assignment->create_metadata( ).
    DATA(meta_work_meas)  = work_measure->create_metadata( ).
    DATA(meta_workterms)  = workterms->create_metadata( ).
    DATA(meta_ext)        = external_ident->create_metadata( ).
    DATA(meta_ext_info)   = extra_info->create_metadata( ).
    DATA(meta_eml)        = person_email->create_metadata( ).
    DATA(meta_leg)        = person_leg_data->create_metadata( ).
    DATA(meta_nme)        = person_name->create_metadata( ).
    DATA(meta_phn)        = person_phone->create_metadata( ).
    DATA(meta_wkr)        = work_relation->create_metadata( ).
*  DATA(meta_cs)         = person_citizenship->create_metadata( ).  "JMB20210323 D - Not needed for COGL
    DATA(meta_assign_man) = assignment_man->create_metadata( ).
    meta_user             = user->create_metadata( ).
    meta_account          = account->create_metadata( ).
    meta_allocation       = allocation->create_metadata( ).
    DATA(meta_image)      = person_image->create_metadata( ).

    CONCATENATE meta_work
                meta_eml
*              meta_ext     "JMB20210712 D - Extract into own file
                meta_ext_info
                meta_leg
                meta_nme
                meta_phn
                meta_wkr
                meta_workterms
                meta_assign
                meta_work_meas  "JMB20210722 I - Pass workmeasure for headcount calculation
*              meta_cs "JMB20210323 D - Not needed for COGL
                meta_image
    INTO        metadata SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE meta_work
                meta_assign_man
    INTO        meta_super SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE meta_work
                meta_ext
    INTO        meta_external SEPARATED BY cl_abap_char_utilities=>newline.

    meta_term = meta_wkr.
  ENDMETHOD.


  METHOD get_cogu_data.
    DATA: data_term_tmp     TYPE string,
          data_work_measure TYPE string,
          data_contract     TYPE string,
          vp_src_id         TYPE /iwbep/t_mgw_name_value_pair,
          vp_wkr_id         TYPE zmhp_cl_mig_work_relation=>vp_wkr_id_t,
          vp_wterm_id       TYPE zmhp_cl_mig_work_terms=>vp_wterm_id_t.

    DATA(data_work)       = worker->proceed_cogu_worker( IMPORTING vp_src_id = vp_src_id ).

    DATA(data_ext)        = external_ident->proceed_cogl_external_ident( worker       = worker
                                                                         vp_src_id    = vp_src_id
                                                                         vp_lcl_pernr = worker->vp_lcl_pernr ).

    DATA(data_ext_info)   = extra_info->proceed_cogl_extra_info( worker    = worker
                                                                 vp_src_id = vp_src_id ).

    DATA(data_eml)        = person_email->proceed_cogl_person_email( worker    = worker
                                                                     vp_src_id = vp_src_id ).

    DATA(data_leg)        = person_leg_data->proceed_cogl_person_leg_data( worker    = worker
                                                                           vp_src_id = vp_src_id ).

    DATA(data_nme)        = person_name->proceed_cofu_person_name( worker    = worker
                                                                   vp_src_id = vp_src_id ).

    DATA(data_phn)        = person_phone->proceed_cofu_person_phone( worker    = worker
                                                                     vp_src_id = vp_src_id ).

    DATA(data_wkr)        = work_relation->proceed_cogu_work_relation( EXPORTING worker    = worker
                                                                                 vp_src_id = vp_src_id
                                                                       IMPORTING vp_wkr_id = vp_wkr_id ).

    DATA(data_workterms)  = workterms->proceed_cogu_work_terms( EXPORTING worker      = worker
                                                                          vp_src_id   = vp_src_id
                                                                          vp_wkr_id   = vp_wkr_id
                                                                IMPORTING vp_wterm_id = vp_wterm_id
                                                                          data_contract = data_contract ). "JMB20210802 I

    DATA(data_assign)     = assignment->proceed_cogu_assignment( EXPORTING worker      = worker
                                                                           workterms   = workterms
                                                                           vp_src_id   = vp_src_id
                                                                           vp_wkr_id   = vp_wkr_id
                                                                           vp_wterm_id = vp_wterm_id
                                                                 IMPORTING work_measure = data_work_measure ).  "JMB20210728 I

    DATA(data_cs)         = person_citizenship->proceed_cofu_per_citizenship( vp_src_id = vp_src_id
                                                                              worker    = worker ).

    DATA(data_assign_man) = assignment_man->proceed_cogu_assign_manager( worker    = worker
                                                                         workterms = workterms
                                                                         vp_src_id = vp_src_id ).

    data_user             = user->proceed_cogl_user( worker    = worker
                                                     vp_src_id = vp_src_id ).

    data_account          = account->proceed_cofu_cost_account( worker    = worker
                                                                vp_src_id = vp_src_id ).

    data_allocation       = allocation->proceed_cofu_cost_allocation( worker    = worker
                                                                      vp_src_id = vp_src_id ).

    DATA(data_ft)         = person_image->proceed_cogl_person_image( vp_src_id = vp_src_id ).

    data_pay              = person_pay->proceed_cofu_person_pay_method( worker    = worker
                                                                        vp_src_id = vp_src_id ).

    CONCATENATE data_work
                data_eml
                data_ext
                data_ext_info
                data_leg
                data_nme
                data_phn
                data_wkr
                data_workterms
                data_assign
                data_contract "JMB20210802 I
                data_work_measure
*              data_cs  "JMB20210802 D
                data_ft
                data_assign_man
    INTO        data SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE data_work
                data_pay
    INTO        data_pay SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.


  METHOD get_cogu_metadata.
    DATA(meta_work)       = worker->create_metadata( ).
    DATA(meta_assign)     = assignment->create_metadata( ).
    DATA(meta_work_meas)  = work_measure->create_metadata( ).
    DATA(meta_workterms)  = workterms->create_metadata( ).
    DATA(meta_contract)   = contract->create_metadata( ).
    meta_external         = external_ident->create_metadata( ).
    DATA(meta_ext_info)   = extra_info->create_metadata( ).
    DATA(meta_eml)        = person_email->create_metadata( ).
    DATA(meta_leg)        = person_leg_data->create_metadata( ).
    DATA(meta_nme)        = person_name->create_metadata( ).
    DATA(meta_phn)        = person_phone->create_metadata( ).
    DATA(meta_wkr)        = work_relation->create_metadata( ).
    DATA(meta_cs)         = person_citizenship->create_metadata( ).
    DATA(meta_assign_man) = assignment_man->create_metadata( ).
    meta_user             = user->create_metadata( ).
    meta_account          = account->create_metadata( ).
    meta_allocation       = allocation->create_metadata( ).
    DATA(meta_image)      = person_image->create_metadata( ).
    meta_pay              = person_pay->create_metadata( ).

    CONCATENATE meta_work
                meta_eml
                meta_external
                meta_ext_info
                meta_leg
                meta_nme
                meta_phn
                meta_wkr
                meta_workterms
                meta_assign
                meta_contract
                meta_work_meas
                meta_cs
                meta_image
                meta_assign_man
    INTO        metadata SEPARATED BY cl_abap_char_utilities=>newline.

    CONCATENATE meta_work
                meta_pay
    INTO        meta_pay  SEPARATED BY cl_abap_char_utilities=>newline.

    meta_term = meta_wkr.
  ENDMETHOD.


  METHOD get_data.
    IF cogl EQ abap_true.
      data = get_cogl_data( IMPORTING data_super      = data_super
                                      data_term       = data_term
                                      data_user       = data_user
                                      data_account    = data_account
                                      data_allocation = data_allocation
                                      data_external   = data_external ).
    ELSEIF cofu EQ abap_true.
      data = get_cofu_data( IMPORTING data_super      = data_super
                                      data_term       = data_term
                                      data_user       = data_user
                                      data_account    = data_account
                                      data_allocation = data_allocation
                                      data_pay        = data_pay
                                      data_external   = data_external
                                      data_salary     = data_salary
                                      data_disability = data_disability "IFT20211105 I
                                      data_contact    = data_contact ). "IFT20211109 I
    ELSEIF cogu EQ abap_true.
      data = get_cogu_data( IMPORTING data_super      = data_super
                                      data_term       = data_term
                                      data_user       = data_user
                                      data_account    = data_account
                                      data_allocation = data_allocation
                                      data_pay        = data_pay
                                      data_external   = data_external ).
    ENDIF.
  ENDMETHOD.


  METHOD get_metadata.

    IF cogl EQ abap_true.
      metadata = get_cogl_metadata( IMPORTING meta_super      = meta_super
                                              meta_term       = meta_term
                                              meta_user       = meta_user
                                              meta_account    = meta_account
                                              meta_external   = meta_external
                                              meta_allocation = meta_allocation ).
    ELSEIF cofu EQ abap_true.
      metadata = get_cofu_metadata( IMPORTING meta_super      = meta_super
                                              meta_term       = meta_term
                                              meta_user       = meta_user
                                              meta_account    = meta_account
                                              meta_allocation = meta_allocation
                                              meta_external   = meta_external
                                              meta_pay        = meta_pay
                                              meta_disability = meta_disability
                                              meta_salary     = meta_salary
                                              meta_contact    = meta_contact ). "IFT20211109 I
    ELSEIF cogu EQ abap_true.
      metadata = get_cogu_metadata( IMPORTING meta_term       = meta_term
                                              meta_user       = meta_user
                                              meta_account    = meta_account
                                              meta_allocation = meta_allocation
                                              meta_external   = meta_external
                                              meta_pay        = meta_pay ).
    ENDIF.

  ENDMETHOD.


  METHOD get_molga.

    LOOP AT pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = zmhp_cl_int_utility=>get_molga( pernr = <pernr>
                                                                                     begda = begda
                                                                                     endda = endda ) ) TO molga.
    ENDLOOP.

    SORT molga BY low.
    DELETE ADJACENT DUPLICATES FROM molga COMPARING low.
    DELETE molga WHERE low IS INITIAL.  "JMB20210803 I
  ENDMETHOD.


  METHOD proceed_file_construction.
    DATA: meta_super      TYPE string,
          meta_term       TYPE string,
          meta_user       TYPE string,
          meta_account    TYPE string,
          meta_allocation TYPE string,
          meta_pay        TYPE string,
          meta_salary     TYPE string,
          meta_external   TYPE string,
          meta_disability TYPE string, "IFT20211105 I
          meta_contact    TYPE string, "IFT20211109 I
          data_super      TYPE string,
          data_term       TYPE string,
          data_user       TYPE string,
          data_account    TYPE string,
          data_allocation TYPE string,
          data_pay        TYPE string,
          data_salary     TYPE string,
          data_external   TYPE string,
          data_disability TYPE string, "IFT20211105 I
          data_contact    TYPE string. "IFT20211109 I

    DATA(metadata) = get_metadata( IMPORTING meta_super      = meta_super
                                             meta_term       = meta_term
                                             meta_user       = meta_user
                                             meta_account    = meta_account
                                             meta_allocation = meta_allocation
                                             meta_pay        = meta_pay
                                             meta_external   = meta_external
                                             meta_salary     = meta_salary
                                             meta_disability = meta_disability   "IFT20211105 I
                                             meta_contact    = meta_contact  ).  "IFT20211109 I

    DATA(data)     = get_data( IMPORTING data_super      = data_super
                                         data_term       = data_term
                                         data_user       = data_user
                                         data_account    = data_account
                                         data_allocation = data_allocation
                                         data_pay        = data_pay
                                         data_external   = data_external
                                         data_salary     = data_salary
                                         data_disability = data_disability "IFT20211105 I
                                         data_contact    = data_contact ). "IFT20211109 I

    CONCATENATE calc_fte metadata INTO metadata SEPARATED BY cl_abap_char_utilities=>newline. "JMB20210705 I Ticket 4999

    content        = build_output( metadata = metadata
                                   data     = data ).
    supervisor     = build_output( metadata = meta_super
                                   data     = data_super ).
    term           = build_output( metadata = meta_term
                                   data     = data_term ).
    user           = build_output( metadata = meta_user
                                   data     = data_user ).
    account        = build_output( metadata = meta_account
                                   data     = data_account ).
    allocation     = build_output( metadata = meta_allocation
                                   data     = data_allocation ).
    pay            = build_output( metadata = meta_pay
                                   data     = data_pay ).
    external       = build_output( metadata = meta_external
                                   data     = data_external ).
    disability     = build_output( metadata = meta_disability   "IFT20211105 I
                                   data     = data_disability ).
    contact        = build_output( metadata = meta_contact      "IFT20211109 I
                                   data     = data_contact ).
    salary         = build_output( metadata = meta_salary
                                   data     = data_salary ).
  ENDMETHOD.
ENDCLASS.
