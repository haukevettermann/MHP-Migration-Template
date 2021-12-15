*&---------------------------------------------------------------------*
*& Report ZMHP_RP_MIG_OM_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmhp_rp_mig_om_file.

TABLES: objec, gdstr.

DATA: objec_all    TYPE objec_t,
      om_data_conv TYPE string,
      om_file      TYPE string,
      om_data_ca   TYPE string,
      om_data_cc   TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK blo WITH FRAME TITLE TEXT-001.

PARAMETERS: file    LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\OM.csv'.
PARAMETERS: file_om LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Organization.dat'.
PARAMETERS: file_ca LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\CostAllocationV3.dat'.
PARAMETERS: file_cc LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\CostAllocationAccountV3.dat'.
PARAMETERS: al11    TYPE boolean         AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blo.

INITIALIZATION.

  PERFORM fill_selfields.
  PERFORM deactivate_otype.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_om.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_om.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_ca.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_ca.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_cc.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_cc.

AT SELECTION-SCREEN OUTPUT.
  PERFORM deactivate_otype.

START-OF-SELECTION.

GET objec.

  APPEND objec TO objec_all.

END-OF-SELECTION.

  DATA(om_data) = NEW zmhp_cl_mig_om_data( begda = pchbegda
                                           endda = pchendda
                                           objec = objec_all
                                           al11  = al11 ).

  DATA(om_csv) = om_data->proceed_om_data( IMPORTING om_data_mig = om_data_conv
                                                     om_data_ca  = om_data_ca
                                                     om_data_cc  = om_data_cc ).

  LOOP AT om_csv ASSIGNING FIELD-SYMBOL(<om_csv>).
    CONCATENATE om_file cl_abap_char_utilities=>newline <om_csv> INTO om_file.
  ENDLOOP.

  DATA(files) = VALUE /iwbep/t_mgw_name_value_pair( ( name = file    value = om_file ) ).

  IF al11 EQ abap_false.
    APPEND LINES OF VALUE /iwbep/t_mgw_name_value_pair( ( name = file_om value = om_data_conv )
                                                        ( name = file_ca value = om_data_ca   )
                                                        ( name = file_cc value = om_data_cc   ) ) TO files.
  ENDIF.

  om_data->download_files( files ).

  INCLUDE zmhp_rp_mig_om_file_selfielf01.
  INCLUDE zmhp_rp_mig_om_file_seloutpu01.
