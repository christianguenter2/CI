*&---------------------------------------------------------------------*
*& Report zabapgit_ci_pull_requests
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_pull_requests.

PARAMETERS: p_ref TYPE string LOWER CASE.

CLASS lcl_collector DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_ci_view.

    TYPES:
      BEGIN OF ty_result,
        ref           TYPE string,
        url           TYPE string,
        error_message TYPE string.
        INCLUDE TYPE zif_abapgit_ci_definitions=>ty_result.
    TYPES:
    END OF ty_result,
    tty_result TYPE STANDARD TABLE OF ty_result
                    WITH NON-UNIQUE DEFAULT KEY.
    DATA: mt_result TYPE tty_result READ-ONLY.

    METHODS:
      set_repo
        IMPORTING
          iv_ref TYPE string
          iv_url TYPE string,

      report_error
        IMPORTING
          ix_error TYPE REF TO zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_collector IMPLEMENTATION.

  METHOD zif_abapgit_ci_view~display.

    ASSIGN mt_result[ lines( mt_result ) ] TO FIELD-SYMBOL(<ls_result>).
    ASSERT sy-subrc = 0.

    MOVE-CORRESPONDING cs_result TO <ls_result>.

  ENDMETHOD.

  METHOD set_repo.

    INSERT INITIAL LINE INTO TABLE mt_result
           ASSIGNING FIELD-SYMBOL(<ls_result>).

    <ls_result>-ref = iv_ref.
    <ls_result>-url = iv_url.

  ENDMETHOD.

  METHOD report_error.

    ASSIGN mt_result[ lines( mt_result ) ] TO FIELD-SYMBOL(<ls_result>).
    ASSERT sy-subrc = 0.

    <ls_result>-error_message = ix_error->get_text( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_view DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_collector TYPE REF TO lcl_collector,

      display.

  PRIVATE SECTION.
    DATA:
      mo_collector TYPE REF TO lcl_collector,
      mo_alv       TYPE REF TO cl_salv_table,
      mr_result    TYPE REF TO data.

    METHODS:
      create_table
        RETURNING
          VALUE(ro_table_descr) TYPE REF TO cl_abap_tabledescr,

      map_result,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            column
            row .

ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.

  METHOD constructor.

    mo_collector = io_collector.
    DATA(lo_table_descr) = create_table( ).

    CREATE DATA mr_result TYPE HANDLE lo_table_descr.

  ENDMETHOD.

  METHOD display.

    map_result( ).

    ASSIGN mr_result->* TO FIELD-SYMBOL(<lt_result>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv
        CHANGING
            t_table      = <lt_result> ).

        DATA(lo_event) = mo_alv->get_event( ).

        SET HANDLER on_double_click FOR lo_event.

        DATA(lo_columns) = mo_alv->get_columns( ).

        lo_columns->set_optimize( ).

        DATA(lt_components) =
          CAST cl_abap_structdescr(
            CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data(
              mo_collector->mt_result
            )
          )->get_table_line_type( )
        )->components.

        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).

          DATA(lo_column) = lo_columns->get_column( <ls_component>-name ).
          lo_column->set_short_text(  |{ <ls_component>-name }| ).
          lo_column->set_medium_text( |{ <ls_component>-name }| ).
          lo_column->set_long_text(   |{ <ls_component>-name }| ).

        ENDLOOP.

        mo_alv->display( ).

      CATCH cx_salv_error INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD create_table.

    DATA(lo_struct_descr) =
      CAST cl_abap_structdescr(
        CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_name( 'LCL_COLLECTOR=>TTY_RESULT' )
      )->get_table_line_type( ) ).

    DATA(lt_components) = lo_struct_descr->get_components( ).

    ASSIGN lt_components[ as_include = abap_true ] TO FIELD-SYMBOL(<ls_include>).
    ASSERT sy-subrc = 0.

    DATA(lt_include_components) = CAST cl_abap_structdescr( <ls_include>-type )->get_components( ).

    LOOP AT lt_include_components ASSIGNING FIELD-SYMBOL(<ls_component>).

      IF <ls_component>-type IS INSTANCE OF cl_abap_tabledescr
      OR <ls_component>-type IS INSTANCE OF cl_abap_structdescr.

        <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( |ICON_D| ).

      ENDIF.

    ENDLOOP.

    <ls_include>-type = cl_abap_structdescr=>create( lt_include_components ).

    ro_table_descr = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_components ) ).

  ENDMETHOD.


  METHOD map_result.

    FIELD-SYMBOLS: <lt_result> TYPE INDEX TABLE.

    ASSIGN mr_result->* TO <lt_result>.
    ASSERT sy-subrc = 0.

    DATA(lt_components) =
      CAST cl_abap_structdescr(
        CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data(
          mo_collector->mt_result
        )
      )->get_table_line_type( )
    )->components.

    LOOP AT mo_collector->mt_result ASSIGNING FIELD-SYMBOL(<ls_collector_result>).

      INSERT INITIAL LINE INTO TABLE <lt_result> ASSIGNING FIELD-SYMBOL(<ls_result>).
      ASSERT sy-subrc = 0.

      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).

        ASSIGN COMPONENT <ls_component>-name
               OF STRUCTURE <ls_result>
               TO FIELD-SYMBOL(<left>).
        ASSERT sy-subrc = 0.

        ASSIGN COMPONENT <ls_component>-name
               OF STRUCTURE <ls_collector_result>
               TO FIELD-SYMBOL(<right>).
        ASSERT sy-subrc = 0.

        CASE <ls_component>-type_kind.
          WHEN  cl_abap_datadescr=>typekind_struct1
            OR cl_abap_datadescr=>typekind_struct2.

            <left> = icon_structure.

          WHEN cl_abap_datadescr=>typekind_table.

            <left> = icon_view_table.

          WHEN OTHERS.

            <left> = <right>.

        ENDCASE.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD on_double_click.

    ASSIGN mo_collector->mt_result[ row ] TO FIELD-SYMBOL(<ls_result>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT column
           OF STRUCTURE <ls_result>
           TO FIELD-SYMBOL(<value>).
    ASSERT sy-subrc = 0.

    CASE column.
      WHEN 'REPO_RESULT_LIST'
        OR 'GENERIC_RESULT_LIST'.

        cl_demo_output=>display( <value> ).

      WHEN 'STATISTICS'.

        ASSIGN COMPONENT 'FINISH_TIMESTAMP'
               OF STRUCTURE <value>
               TO FIELD-SYMBOL(<value2>).
        ASSERT sy-subrc = 0.
        cl_demo_output=>write( <value2> ).

        ASSIGN COMPONENT 'DURATION_IN_SECONDS'
               OF STRUCTURE <value>
               TO <value2>.
        ASSERT sy-subrc = 0.
        cl_demo_output=>write( <value2> ).

        ASSIGN COMPONENT 'TEST_CASES'
               OF STRUCTURE <value>
               TO <value2>.
        ASSERT sy-subrc = 0.
        cl_demo_output=>write( <value2> ).

        cl_demo_output=>display(  ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.
    METHODS:
      start
        RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_controller IMPLEMENTATION.

  METHOD start.

    DATA(lt_pull_requests) = NEW zcl_abapgit_ci_github_rest( )->fetch_pull_requests(
        iv_user = 'larshp'
        iv_repo = 'abapGit' ).

    IF p_ref IS NOT INITIAL.
      DELETE lt_pull_requests WHERE head-ref <> p_ref.
    ENDIF.

    DATA(lo_collector) = NEW lcl_collector( ).

    LOOP AT lt_pull_requests ASSIGNING FIELD-SYMBOL(<ls_pull_request>).

      lo_collector->set_repo(
          iv_ref = <ls_pull_request>-head-ref
          iv_url = <ls_pull_request>-head-repo-git_url ).

      TRY.

          zcl_abapgit_ci_repos=>update_abapgit_repo(
              iv_branch_name = <ls_pull_request>-head-ref
              iv_url         = <ls_pull_request>-head-repo-git_url ).

          NEW zcl_abapgit_ci_controller(
              ii_view          = lo_collector
              ii_repo_provider = NEW zcl_abapgit_ci_test_repos( )
              is_options       = VALUE #(
                exec_generic_checks    = abap_true
                exec_repository_checks = abap_true
              )
           )->run( ).

        CATCH zcx_abapgit_exception INTO DATA(error).
          lo_collector->report_error( error ).
      ENDTRY.

    ENDLOOP.

    NEW lcl_alv_view( lo_collector )->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW lcl_controller( )->start( ).
    CATCH zcx_abapgit_exception INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
