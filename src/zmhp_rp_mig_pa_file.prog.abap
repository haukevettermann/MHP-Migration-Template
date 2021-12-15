*&---------------------------------------------------------------------*
*& Report ZMHP_RP_PA_MIG_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmhp_rp_mig_pa_file.

TABLES: pernr,
        sscrfields.

NODES: peras.

DATA: pernrs     TYPE rsdsselopt_t,
      supervisor TYPE string,
      term       TYPE string,
      user       TYPE string,
      account    TYPE string,
      allocation TYPE string,
      pay        TYPE string,
      external   TYPE string,
      disability TYPE string, " IFT20211105 I
      contact    TYPE string, " IFT20211109 I
      ucomm      TYPE sscrfields-ucomm.

INITIALIZATION.
  pnptimed = 'A'.

**---------------------------------------------------------------------*
* Selectionscreen
**---------------------------------------------------------------------*
  "selection of status
  SELECTION-SCREEN BEGIN OF BLOCK sel_stat WITH FRAME TITLE TEXT-001.
  PARAMETERS: cogl RADIOBUTTON GROUP r01 DEFAULT 'X' USER-COMMAND stat,
              cofu RADIOBUTTON GROUP r01,
              cogu RADIOBUTTON GROUP r01,
              al11 TYPE boolean AS CHECKBOX MODIF ID cog.
  SELECTION-SCREEN END OF BLOCK sel_stat.

  "additional options
  SELECTION-SCREEN BEGIN OF BLOCK sel_file WITH FRAME TITLE TEXT-002.
  PARAMETERS: file    LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Worker.dat'.
  PARAMETERS: file_sa LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Worker_Supervisor.dat'         MODIF ID cou.
  PARAMETERS: file_tm LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Worker_Termination.dat'        MODIF ID cou.
  PARAMETERS: file_ur LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\User.dat'.
  PARAMETERS: file_ei LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Worker_ExternalIdentifier.dat' MODIF ID cou.
  PARAMETERS: file_al LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\CostAllocationV3.dat'.
  PARAMETERS: file_ac LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\CostAllocationAccountV3.dat'.
  PARAMETERS: file_ft TYPE string                     DEFAULT 'C:\Foto'                          MODIF ID cou.
  PARAMETERS: file_bd LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\PersonalPaymentMethod.dat'     MODIF ID cof.
  PARAMETERS: file_pd LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\PersonDisability.dat'          MODIF ID cof. " IFT20211105
  PARAMETERS: file_ct LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Contact.dat'                   MODIF ID cof. " IFT20211109
  SELECTION-SCREEN END OF BLOCK sel_file.

AT SELECTION-SCREEN.
  ucomm = sscrfields-ucomm.

  "bank details only for CoFu/CoGu relevant

AT SELECTION-SCREEN OUTPUT.
  CASE ucomm.
    WHEN 'STAT'.
      LOOP AT SCREEN.
        screen-active = 0.
        CASE screen-group1.
          WHEN 'COF'.
            IF cofu EQ abap_true OR
               cogu EQ abap_true.
              screen-active = 1.
            ENDIF.
          WHEN 'COG'.
            "AL11 output only for COGU
            IF cogu EQ abap_true.
              screen-active = 1.
            ENDIF.
          WHEN 'COU'.
            IF cofu EQ abap_true OR
               cogl EQ abap_true.
              screen-active = 1.
            ENDIF.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN OTHERS.
      LOOP AT SCREEN.
        screen-active = 1.
        CASE screen-group1.
          WHEN 'COF' OR
               'COG'.
            CHECK cofu IS INITIAL AND
                  cogu IS INITIAL.
            screen-active = 0.
          WHEN 'COU'.
            CHECK cogu EQ abap_false.
            screen-active = 1.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.

  "F4 help for all files

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_sa.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_sa.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_tm.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_tm.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_ur.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_ur.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_ac.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_ac.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_al.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_al.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_ft.

  cl_gui_frontend_services=>directory_browse(
    CHANGING
      selected_folder      = file_ft
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_bd.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_bd.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_ei.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_ei.
**IFT20211105 Insert Start - add PersonDisability.dat file
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_pd.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_pd.
*IFT20211105 Insert End

**IFT20211109 Insert Start - add Contact.dat file
*
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_ct.
*IFT20211109 Insert End

START-OF-SELECTION.

GET peras.
  "collect all pernr
  APPEND VALUE #( sign = 'I' option = 'EQ' low = pernr-pernr ) TO pernrs.

END-of-SELECTION.

  IF pernrs IS NOT INITIAL.
    DATA(zip) = NEW cl_abap_zip( ).
    DATA(mig_pa_file) = NEW zmhp_cl_mig_pa_file( pic_path = CONV #( file_ft )
                                                 cogl     = cogl
                                                 cofu     = cofu
                                                 cogu     = cogu
                                                 pernr    = pernrs
                                                 pn_begda = pn-begda "JMB20211011 I - C400129651-5882
                                                 begda    = cl_hcp_global_constants=>c_lowdate
                                                 endda    = pn-endda
                                                 al11     = al11
                                                 zip      = zip ).

    DATA(content) =  mig_pa_file->proceed_file_construction( IMPORTING supervisor = supervisor
                                                                       term       = term
                                                                       user       = user
                                                                       account    = account
                                                                       allocation = allocation
                                                                       pay        = pay
                                                                       external   = external
                                                                       disability = disability "IFT20211105 I
                                                                       contact    = contact ). "IFT20211109 I

    DATA(files) = VALUE /iwbep/t_mgw_name_value_pair( ( name = file    value = content    )
                                                      ( name = file_ur value = user       )
                                                      ( name = file_ac value = account    )
                                                      ( name = file_al value = allocation ) ).

    IF cofu EQ abap_true OR
       cogl EQ abap_true.
      APPEND LINES OF VALUE /iwbep/t_mgw_name_value_pair( ( name = file_sa value = supervisor )
                                                          ( name = file_tm value = term       )
                                                          ( name = file_ei value = external   )
                                                          ( name = file_pd value = disability ) "IFT20211105 I
                                                          ( name = file_ct value = contact    ) "IFT20211109 I
                                                          ) TO files.

    ENDIF.

    IF pay  IS NOT INITIAL AND
       cogl EQ abap_false.
      APPEND VALUE #( name = file_bd value = pay ) TO files.
    ENDIF.

    mig_pa_file->download_files( files ).

  ENDIF.
