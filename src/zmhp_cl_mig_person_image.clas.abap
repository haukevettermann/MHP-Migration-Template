class ZMHP_CL_MIG_PERSON_IMAGE definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data PATH type STRING .
  constants PERSON_IMAGE type STRING value 'PersonImage' ##NO_TEXT.
  constants ORACLE_CONS type STRING value 'ORACLECONS' ##NO_TEXT.
  constants IMAGE type STRING value 'IMAGE' ##NO_TEXT.
  constants EMPPHOTO type STRING value 'EmpPhoto' ##NO_TEXT.
  data VP_PERSON_IMAGE_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !PATH type STRING
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN
      !AL11 type BOOLEAN
      !ZIP type ref to CL_ABAP_ZIP .
  methods PROCEED_COFU_PERSON_IMAGE
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_PERSON_IMAGE
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods GET_EMP_PIC
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
protected section.
private section.

  data COGU type BOOLEAN .
  data AL11 type BOOLEAN .
  data ZIP type ref to CL_ABAP_ZIP .

  methods GET_DATA_ENTRY
    importing
      !IMG_NAME type STRING
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS ZMHP_CL_MIG_PERSON_IMAGE IMPLEMENTATION.


METHOD constructor.
  me->pernr = pernr.
  me->path  = path.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.
  me->al11  = al11.
  me->zip   = zip.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    vp_person_image_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                         ( name = 2  value = person_image )
                   	                     ( name = 4  value = 'SourceSystemOwner' )
                                         ( name = 3  value = 'SourceSystemId' )
                                         ( name = 5  value = 'PersonId(SourceSystemId)' )
                                         ( name = 6  value = 'ImageName' )
                                         ( name = 7  value = 'Image' )
                                         ( name = 8  value = 'PrimaryFlag' ) ).
  ELSEIF cofu EQ abap_true.
    vp_person_image_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                       ( name = 2  value = person_image )
                 	                     ( name = 4  value = 'SourceSystemOwner' )
                                       ( name = 3  value = 'SourceSystemId' )
                                       ( name = 5  value = 'PersonId(SourceSystemId)' )
                                       ( name = 6  value = 'ImageName' )
                                       ( name = 7  value = 'Image' )
                                       ( name = 8  value = 'PrimaryFlag' ) ).
  ENDIF.

ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_person_image_structure LINES DATA(length).

  LOOP AT vp_person_image_structure ASSIGNING FIELD-SYMBOL(<person_image_struc>).

    "set METADATA title
    CASE <person_image_struc>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <person_image_struc>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD get_data_entry.

  DATA: src_id TYPE string,
        sys_id TYPE string.

  CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

  CONCATENATE image pernr INTO src_id.

  "get source id
  DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = pernr
                                                    begda = sy-datum
                                                    endda = sy-datum
                                                    vp_src_id = vp_src_id ).

  CONCATENATE zmhp_cl_mig_utils=>merge
              person_image
              sys_id
              src_id
              src_sys_id
              empphoto
              img_name
              zmhp_cl_mig_utils=>yes
  INTO data SEPARATED BY zmhp_cl_mig_utils=>separator.

ENDMETHOD.


METHOD get_emp_pic.

  TYPES : BEGIN OF binary,
            field(1000) TYPE c,
          END OF binary.

  DATA: connect_info TYPE toav0,
        ex_length    TYPE int4,
        buffer       TYPE xstring,
        binary_tab   TYPE tabl1024_t,
        ex_doc       TYPE tabl1024_t,
        pic_exists   TYPE c,
        pic_name     TYPE string,
        file         TYPE string,
        data_bin     TYPE TABLE OF binary.

  CHECK cogu IS INITIAL.    "JMB20210728 I - Pictures aren´t needed for CoGu

  LOOP AT pernr ASSIGNING FIELD-SYMBOL(<pernr>).

    CALL FUNCTION 'HR_IMAGE_EXISTS'
      EXPORTING
        p_pernr               = CONV pernr_d( <pernr>-low )
        p_begda               = sy-datum
        p_endda               = sy-datum
      IMPORTING
        p_exists              = pic_exists
        p_connect_info        = connect_info
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.

    IF pic_exists EQ '1'.

      CALL FUNCTION 'ALINK_RFC_TABLE_GET'
        EXPORTING
          im_docid    = connect_info-arc_doc_id
          im_crepid   = connect_info-archiv_id
        IMPORTING
          ex_length   = ex_length
        TABLES
          ex_document = ex_doc.

      CHECK ex_length IS NOT INITIAL.

      binary_tab = ex_doc.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = ex_length
        IMPORTING
          buffer       = buffer
        TABLES
          binary_tab   = binary_tab
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      CONCATENATE <pernr>-low '.jpg' INTO pic_name.

      "in case files should be transport to AL11
      file = SWITCH #( al11
                       WHEN abap_true  THEN 'BlobFiles/' && pic_name
                       WHEN abap_false THEN path && '\'  && pic_name ).

      CASE al11.
        WHEN abap_true.
          zip->add( name    = file
                    content = buffer ).
        WHEN abap_false.
          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = buffer
            TABLES
              binary_tab = data_bin.

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              filename = file
              filetype = 'BIN'
            TABLES
              data_tab = data_bin.
      ENDCASE.

      DATA(data_tmp) =  get_data_entry( pernr    = CONV pernr_d( <pernr>-low )
                                        img_name = pic_name ).

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

      CLEAR: data_tmp, pic_name, file, data_bin, buffer, binary_tab, ex_length, ex_doc, connect_info, pic_exists.

    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD PROCEED_COFU_PERSON_IMAGE.
  me->vp_src_id = vp_src_id.
  data = get_emp_pic( ).
ENDMETHOD.


METHOD PROCEED_COGL_PERSON_IMAGE.
  me->vp_src_id = vp_src_id.
  data = get_emp_pic( ).
ENDMETHOD.
ENDCLASS.
