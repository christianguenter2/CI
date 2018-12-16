FUNCTION Z_ABAPGIT_CI_UPDATE_ABAPGIT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BRANCH_NAME) TYPE  STRING
*"     VALUE(IV_URL) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"     VALUE(EV_RC) TYPE  SYSUBRC
*"----------------------------------------------------------------------

  CLEAR:
    ev_message,
    ev_rc.

  TRY.
      zcl_abapgit_ci_repos=>update_abapgit_repo(
          iv_branch_name = iv_branch_name
          iv_url         = iv_url ).

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      ev_message = lx_error->get_text( ).
      ev_rc      = 1.
  ENDTRY.

ENDFUNCTION.
