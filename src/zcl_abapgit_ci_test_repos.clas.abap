CLASS zcl_abapgit_ci_test_repos DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ci_repo_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_ci_test_repos IMPLEMENTATION.


  METHOD zif_abapgit_ci_repo_provider~get_repos.

    DATA(li_github_rest) = NEW zcl_abapgit_ci_github_rest( ).

    DO.

      TRY.
          DATA(lt_repos) = li_github_rest->fetch_repo_page( iv_organization = 'abapGit-tests'
                                                            iv_page_count   = sy-index ).

        CATCH zcx_abapgit_exception cx_rest_client_exception INTO DATA(lx_error).
          zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                        ix_previous = lx_error ).
      ENDTRY.

      IF lines( lt_repos ) = 0.
        EXIT.
      ENDIF.

      INSERT LINES OF lt_repos INTO TABLE rt_repos.

    ENDDO.

    " skip because they call the UI.. .
    "
    DELETE rt_repos WHERE name = |CUS0|
                       OR name = |ECATT| " https://github.com/larshp/abapGit/issues/2113
                       OR name = |SPRX| " https://github.com/larshp/abapGit/issues/87
                       OR name = |IEXT| " https://github.com/larshp/abapGit/issues/2044
                       OR name = |IDOC| " https://github.com/larshp/abapGit/issues/2044
                       OR name = |XINK| " https://github.com/larshp/abapGit/issues/2106
                       OR name = |SFSW| "https://github.com/larshp/abapGit/issues/2083
                       .

    " Skip because old testcase. abapGit indicates diff because migration to new format
    DELETE rt_repos WHERE name = |DDLX_old|.

    " Skip because of diffs due to component info not supported in NW752 dev edition
    DELETE rt_repos WHERE name = |DEVC_component|.


    SORT rt_repos BY name.

  ENDMETHOD.
ENDCLASS.
