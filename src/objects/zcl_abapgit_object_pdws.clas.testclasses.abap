CLASS ltc_local_tests DEFINITION FINAL
  FOR TESTING
  DURATION MEDIUM
  RISK LEVEL CRITICAL
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mv_taskid TYPE hrobjid.
    CLASS-DATA mv_changed_by TYPE usrname.

    DATA mo_cut TYPE REF TO zif_abapgit_object.

    CLASS-METHODS class_setup.
    CLASS-METHODS get_any_workflow RETURNING VALUE(rv_result) TYPE hrobjid.

    METHODS setup.

    METHODS validate_created_by FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_local_tests IMPLEMENTATION.

  METHOD class_setup.
    mv_taskid = get_any_workflow( ).
  ENDMETHOD.

  METHOD setup.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PDWS'.
    ls_item-obj_name = 'TS' && mv_taskid.

    TRY.
        CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdws
          EXPORTING
            is_item     = ls_item
            iv_language = sy-langu.

      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Could not instantiate PDWS' ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_any_workflow.

    DATA: BEGIN OF ls_objdata,
            objid TYPE hr_sobjid,
            uname TYPE usrname,
          END OF ls_objdata.

    SELECT SINGLE objid uname
           FROM hrs1000
           INTO ls_objdata
           WHERE otype = 'WS' ##WARN_OK. "#EC CI_NOORDER #EC CI_SGLSELECT

    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc ).
    mv_changed_by = ls_objdata-uname.
    rv_result = ls_objdata-objid.

  ENDMETHOD.

  METHOD validate_created_by.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->changed_by( )
                                        exp = mv_changed_by ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_ci DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.
    METHODS run_ci FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_ci IMPLEMENTATION.

  METHOD run_ci.

    DATA lv_repo_url TYPE string.

    "Use STVARV to optionally override repo in local system
    SELECT SINGLE low
      INTO lv_repo_url
      FROM tvarvc
      WHERE name = 'ABAPGIT_TEST_URL_PDWS'  ##WARN_OK.

    IF sy-subrc = 0.   "Todo: Remove once we have a test repo
      zcl_abapgit_objects_ci_tests=>run(
          iv_object = 'PDWS'
          iv_url  = lv_repo_url ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_smoke_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO zif_abapgit_object.

    METHODS run_trivial_methods FOR TESTING RAISING cx_static_check.
    METHODS serialize FOR TESTING RAISING cx_static_check.
    METHODS deserialize FOR TESTING RAISING cx_static_check.
    METHODS delete FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_smoke_test IMPLEMENTATION.

  METHOD run_trivial_methods.

    DATA ls_item   TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PDWS'.
    ls_item-obj_name = '99999999'.

    CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdws
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    mo_cut->get_comparator( ).
    mo_cut->get_deserialize_steps( ).
    mo_cut->get_metadata( ).
    mo_cut->is_active( ).

  ENDMETHOD.

  METHOD serialize.
    cl_abap_unit_assert=>fail( msg = 'Todo' level = if_aunit_constants=>tolerable ).
  ENDMETHOD.

  METHOD deserialize.
    cl_abap_unit_assert=>fail( msg = 'Todo' level = if_aunit_constants=>tolerable ).
  ENDMETHOD.

  METHOD delete.
    cl_abap_unit_assert=>fail( msg = 'Todo' level = if_aunit_constants=>tolerable ).
  ENDMETHOD.

ENDCLASS.
