CLASS zcl_abapgit_ci_tags DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_ci_test.

ENDCLASS.



CLASS zcl_abapgit_ci_tags IMPLEMENTATION.

  METHOD zif_abapgit_ci_test~get_description.

    rv_description = |Clone repo, create, checkout and delete tag, uninstall repo|.

  ENDMETHOD.

  METHOD zif_abapgit_ci_test~execute.



  ENDMETHOD.

ENDCLASS.
