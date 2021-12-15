*&---------------------------------------------------------------------*
*& Report ZMHP_RP_MIG_OM_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmhp_rp_mig_pos_file.

TABLES: objec.

DATA: objec_all TYPE objec_t,
      plvar     TYPE plvar.

SELECTION-SCREEN BEGIN OF BLOCK file WITH FRAME TITLE TEXT-001.

PARAMETERS: file LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Position.dat'.

SELECTION-SCREEN END OF BLOCK file.


INITIALIZATION.

  "Aktive Planvariante ermitteln
  CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
    IMPORTING
      act_plvar       = plvar
    EXCEPTIONS
      no_active_plvar = 1
      OTHERS          = 2.

  pchplvar = plvar.   "Aktive Planvariante
  pchotype = 'S'.     "Planstelle
  pchostat = 1.       "Status


AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file.


START-OF-SELECTION.

GET objec.

  APPEND objec TO objec_all.

END-OF-SELECTION.

  DATA(pos_data) = NEW /sew/cl_mig_pos_data( begda = pchbegda
                                             endda = pchendda
                                             objec = objec_all ).
  DATA(metadata) = pos_data->create_metadata( ).
  DATA(data) = pos_data->proceed_pos_data( ).
  DATA(content) = pos_data->proceed_file_construction( metadata = metadata
                                                       data     = data ).

  DATA(files) = VALUE /iwbep/t_mgw_name_value_pair( ( name = file    value = content ) ).
  pos_data->download_files( files ).
