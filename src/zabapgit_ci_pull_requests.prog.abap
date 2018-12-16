*&---------------------------------------------------------------------*
*& Report zabapgit_ci_pull_requests
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_ci_pull_requests.

CLASS lcl_collector_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_ci_view.
    METHODS:
      set_repo
        IMPORTING
          iv_ref TYPE string
          iv_url TYPE string,

      report_error
        IMPORTING
          ix_error TYPE REF TO zcx_abapgit_exception.

  PRIVATE SECTION.
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

    DATA: mt_result TYPE tty_result.

ENDCLASS.

CLASS lcl_collector_view IMPLEMENTATION.

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

CLASS controller DEFINITION.

  PUBLIC SECTION.
    METHODS:
      start
        RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(lt_pull_requests) = NEW zcl_abapgit_ci_github_rest( )->fetch_pull_requests(
        iv_user = 'larshp'
        iv_repo = 'abapGit' ).

    DATA(li_collector) = NEW lcl_collector_view( ).

    LOOP AT lt_pull_requests ASSIGNING FIELD-SYMBOL(<ls_pull_request>).

      li_collector->set_repo(
          iv_ref = <ls_pull_request>-head-ref
          iv_url = <ls_pull_request>-head-repo-git_url ).

      TRY.

          zcl_abapgit_ci_repos=>update_abapgit_repo(
              iv_branch_name = <ls_pull_request>-head-ref
              iv_url         = <ls_pull_request>-head-repo-git_url ).

          NEW zcl_abapgit_ci_controller(
              ii_view          = li_collector
              ii_repo_provider = NEW zcl_abapgit_ci_test_repos( )
              is_options       = VALUE #(
                exec_generic_checks    = abap_true
                exec_repository_checks = abap_true
              ) ).

        CATCH zcx_abapgit_exception INTO DATA(error).
          li_collector->report_error( error ).
      ENDTRY.

    ENDLOOP.

*        zcl_abapgit_ci_repos=>update_abapgit_repo( iv_branch_name = 'master' ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW controller( )->start( ).
    CATCH zcx_abapgit_exception INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
