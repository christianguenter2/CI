CLASS zcl_abapgit_ci_github_rest DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      create
        RETURNING
          VALUE(ri_rest_client) TYPE REF TO if_rest_client
        RAISING
          zcx_abapgit_exception.

    METHODS:
      constructor
        RAISING
          zcx_abapgit_exception,

      fetch_repo_page
        IMPORTING
          iv_organization TYPE string
          iv_page_count   TYPE i
        RETURNING
          VALUE(rt_repos) TYPE zif_abapgit_ci_definitions=>tty_repo
        RAISING
          zcx_abapgit_exception,

      fetch_pull_requests
        IMPORTING
          iv_user                 TYPE string
          iv_repo                 TYPE string
        RETURNING
          VALUE(rt_pull_requests) TYPE zif_abapgit_ci_definitions=>tty_pull_requests
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: gi_rest_client TYPE REF TO if_rest_client.

ENDCLASS.



CLASS zcl_abapgit_ci_github_rest IMPLEMENTATION.


  METHOD constructor.

    gi_rest_client = zcl_abapgit_ci_github_rest=>create( ).

  ENDMETHOD.


  METHOD create.

    DATA: lv_rfcdes      TYPE rfcdes-rfcdest,
          li_http_client TYPE REF TO if_http_client.

    lv_rfcdes = |API_GITHUB_{ sy-uname }|.

    SELECT SINGLE FROM rfcdes
           FIELDS rfcdest
           WHERE rfcdest = @lv_rfcdes
           INTO @lv_rfcdes.

    IF sy-subrc = 0.

      cl_http_client=>create_by_destination(
        EXPORTING
          destination              = lv_rfcdes
        IMPORTING
          client                   = li_http_client
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ELSE.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = 'https://api.github.com'
          ssl_id             = 'ANONYM'
        IMPORTING
          client             = li_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

    ri_rest_client = CAST if_rest_client( NEW cl_rest_http_client( li_http_client ) ).

  ENDMETHOD.


  METHOD fetch_pull_requests.

    " https://api.github.com/repos/larshp/abapGit/pulls

    gi_rest_client->create_request_entity( )->set_header_field(
        iv_name  = '~request_uri'
        iv_value = |/repos/{ iv_user }/{ iv_repo }/pulls| ).

    gi_rest_client->get( ).

    DATA(lo_response) = gi_rest_client->get_response_entity( ).

    DATA(lv_status) = gi_rest_client->get_status( ).

    IF lv_status <> cl_rest_status_code=>gc_success_ok.
      zcx_abapgit_exception=>raise( |HTTP status code { lv_status } from api.github.com| ).
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lo_response->get_string_data( )
      CHANGING
        data = rt_pull_requests ).

  ENDMETHOD.


  METHOD fetch_repo_page.

    gi_rest_client->create_request_entity( )->set_header_field(
        iv_name  = '~request_uri'
        iv_value = |/orgs/{ iv_organization }/repos?page={ iv_page_count }| ).

    gi_rest_client->get( ).

    DATA(lo_response) = gi_rest_client->get_response_entity( ).

    DATA(lv_status) = gi_rest_client->get_status( ).

    IF lv_status <> cl_rest_status_code=>gc_success_ok.
      zcx_abapgit_exception=>raise( |HTTP status code { lv_status } from api.github.com| ).
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lo_response->get_string_data( )
      CHANGING
        data = rt_repos ).

  ENDMETHOD.
ENDCLASS.
