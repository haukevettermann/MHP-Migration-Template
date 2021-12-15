class ZMHP_CL_MIG_PERSON_PAY_METHOD definition
  public
  create public .

public section.

  data VP_PERSON_PAY type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_PAYMENT_METHOD type STRING value 'PersonalPaymentMethod' ##NO_TEXT.
  data P0009 type P0009_TAB .
  data BNKA type BF_BNKA .
  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .

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
  methods PROCEED_COFU_PERSON_PAY_METHOD
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    returning
      value(DATA) type STRING .
protected section.
private section.

  data COGU type BOOLEAN .

  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods MAP_COGL_DATA .
ENDCLASS.



CLASS ZMHP_CL_MIG_PERSON_PAY_METHOD IMPLEMENTATION.


METHOD constructor.
  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.

  IF cofu EQ abap_true OR
     cogu EQ abap_true.
    vp_person_pay = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                             ( name = 2  value = person_payment_method )
                             ( name = 3  value = 'LegislativeDataGroupName' )
                             ( name = 4  value = 'AssignmentNumber' )
                             ( name = 5  value = 'PersonalPaymentMethodCode' )
                             ( name = 6  value = 'EffectiveStartDate' )
                             ( name = 7  value = 'PaymentAmountType' )
                             ( name = 8  value = 'Amount' )
                             ( name = 9  value = 'ProcessingOrder' )
                             ( name = 10 value = 'OrganizationPaymentMethodCode' )
                             ( name = 11 value = 'Percentage' )
                             ( name = 12 value = 'BankName' )
                             ( name = 13 value = 'BankBranchNumber' )
                             ( name = 14 value = 'BankCountryCode' )
                             ( name = 15 value = 'BankAccountNumber' ) ).
  ELSEIF cogl EQ abap_true.
    "Not needed for CoGl
  ENDIF.

ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_person_pay LINES DATA(length).

  LOOP AT vp_person_pay ASSIGNING FIELD-SYMBOL(<person_pay>).

    "set METADATA title
    CASE <person_pay>-name.
      WHEN 1.
        CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <person_pay>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD get_cofu_data.

  "Get IT0009
  SELECT pernr,
         begda,
         endda,
         bankl,
         banks,
         bankn INTO CORRESPONDING FIELDS OF TABLE @p0009 FROM pa0009 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "collect bank keys
  DATA(bankl) = VALUE rsdsselopt_t( FOR <bankl> IN p0009 ( sign = 'I' option = 'EQ' low = <bankl>-bankl ) ).
  SORT bankl BY low.
  DELETE ADJACENT DUPLICATES FROM bankl COMPARING low.

  "Get BNKA
  SELECT bankl,
         banka INTO CORRESPONDING FIELDS OF TABLE @bnka FROM bnka WHERE bankl IN @bankl.

ENDMETHOD.


METHOD get_cogl_data.
ENDMETHOD.


METHOD map_cofu_data.

  LOOP AT p0009 ASSIGNING FIELD-SYMBOL(<p0009>).

    "get BankName
    READ TABLE bnka ASSIGNING FIELD-SYMBOL(<bnka>) WITH KEY bankl = <p0009>-bankl.

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0009>-begda ).

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0009>-pernr
                                                      begda = <p0009>-begda
                                                      endda = <p0009>-endda
                                                      vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I

    CHECK <bnka>  IS ASSIGNED.

    CONCATENATE <p0009>-banks 'LDG' INTO DATA(leg_group) SEPARATED BY space.

    CONCATENATE zmhp_cl_mig_utils=>merge
                person_payment_method
                leg_group
                src_sys_id
                ''
                begda_tmp
                'M'
                '10000'
                ''
                ''
                '10000'
                <bnka>-banka
                <p0009>-bankl
                <p0009>-banks
                <p0009>-bankn
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.
    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.

ENDMETHOD.


  method MAP_COGL_DATA.
  endmethod.


METHOD proceed_cofu_person_pay_method.
  get_cofu_data( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
