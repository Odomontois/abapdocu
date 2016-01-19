*&---------------------------------------------------------------------*
*& Report  ZCLIF_DESCR_EXPORT                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zclif_descr_export.
CLASS cl_ixml DEFINITION LOAD.
CLASS  zrdd_cl_fz223_utils DEFINITION LOAD.
TYPE-POOLS: abap, seoo, seos.
TYPES filename(128) TYPE c.

PARAMETERS fname TYPE filename  MEMORY ID zzdecr_fname LOWER CASE.

TABLES tadir.
*---------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA instance TYPE REF TO lcl_main.
    CLASS-METHODS class_constructor.
    METHODS: run, f4_fname.
  PRIVATE SECTION.
    DATA ixml TYPE REF TO if_ixml.
    DATA doc TYPE REF TO if_ixml_document.

    METHODS put_clif
      IMPORTING tadir TYPE tadir
                root  TYPE REF TO if_ixml_element.

    METHODS put_clif_desc
      IMPORTING key    TYPE seoclskey
                parent TYPE REF TO if_ixml_element.

    METHODS put_methods
      IMPORTING key    TYPE seoclskey
                parent TYPE REF TO if_ixml_element.

    METHODS add_field
       IMPORTING elem  TYPE REF TO if_ixml_element
                 check TYPE abap_bool DEFAULT abap_true
                 name  TYPE c
                 value TYPE c.

    METHODS put_params
      IMPORTING cmp    TYPE seocmpkey
                parent TYPE REF TO if_ixml_element.
ENDCLASS.

END-OF-SELECTION.
  CALL METHOD lcl_main=>instance->run.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR fname.
  CALL METHOD lcl_main=>instance->f4_fname.

*---------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT instance.
  ENDMETHOD.

  METHOD run.
    DATA sf TYPE REF TO if_ixml_stream_factory.
    DATA tab        TYPE TABLE OF so_raw255.
    DATA filename   TYPE string.
    DATA ostream    TYPE REF TO if_ixml_ostream.
    DATA renderer   TYPE REF TO if_ixml_renderer.
    DATA root       TYPE REF TO if_ixml_element.
    DATA root_node  TYPE REF TO if_ixml_node.
    DATA len        TYPE i.

    DATA xstring      TYPE xstring.
    DATA xstr_ostream TYPE REF TO if_ixml_ostream.
    DATA xstr_render  TYPE REF TO if_ixml_renderer.

    ixml = cl_ixml=>create( ).

    sf = ixml->create_stream_factory( ).

    doc = ixml->create_document( ).

    ostream = sf->create_ostream_itable( tab ).

    renderer = ixml->create_renderer(
      document  = doc
      ostream   = ostream ).

    root = doc->create_element( name = 'types' ).

    root_node = doc->get_root( ).

    CALL METHOD root_node->append_child( new_child = root ).

    SELECT * FROM tadir INTO tadir
      WHERE devclass = 'ZRDD_FZ223'
        AND object IN ('CLAS', 'INTF').
      WRITE / tadir-obj_name.

      CALL METHOD put_clif
        EXPORTING tadir = tadir root = root.
    ENDSELECT.

    CALL METHOD renderer->set_normalizing( is_normalizing = abap_true ).

    CALL METHOD renderer->render.

    xstr_ostream = sf->create_ostream_xstring( xstring ).
    xstr_render  = ixml->create_renderer(
      document = doc
      ostream  = xstr_ostream ).

    CALL METHOD xstr_render->set_normalizing(
      is_normalizing = abap_true ).
    CALL METHOD xstr_render->render.

    len = strlen( xstring ).

    filename  = fname.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = filename
        filetype                = 'BIN'
        bin_filesize            = len
      CHANGING
        data_tab                = tab
      EXCEPTIONS
        OTHERS                  = 22.
  ENDMETHOD.

  METHOD put_clif.
    DATA elem      TYPE REF TO if_ixml_element.
    DATA name      TYPE string.
    DATA clif_name TYPE string.
    DATA key       TYPE seoclskey.

    CASE tadir-object.
      WHEN 'CLAS'.
        name = 'class'.
      WHEN 'INTF'.
        name = 'interface'.
    ENDCASE.

    elem = doc->create_element( name = name ).

    clif_name = tadir-obj_name.

    CALL METHOD elem->set_attribute(
     name = 'name' value = clif_name ).

    key-clsname = tadir-obj_name.

    CALL METHOD put_clif_desc( parent = elem key  = key ).

    CALL METHOD put_methods( parent = elem key = key ).

    CALL METHOD root->append_child( new_child = elem ).
  ENDMETHOD.

  METHOD put_clif_desc.
    DATA elem      TYPE REF TO if_ixml_element.
    DATA class     TYPE vseoclass.
    DATA intf      TYPE vseointerf.
    DATA type      TYPE seoclstype.
    DATA desc      TYPE string.

    elem = doc->create_element( name = 'description' ).

    CALL FUNCTION 'SEO_CLIF_GET'
         EXPORTING
              cifkey    = key
         IMPORTING
              clstype   = type
              class     = class
              interface = intf
         EXCEPTIONS
              OTHERS    = 4.

    CASE type.
      WHEN 0.
        desc = class-descript.
      WHEN 1.
        desc = intf-descript.
    ENDCASE.

    CALL METHOD elem->set_value( value = desc ).

    CALL METHOD parent->append_child( new_child = elem ).
  ENDMETHOD.

  METHOD put_methods.
    DATA methods TYPE seoo_methods_r.
    DATA method  TYPE seoo_method_r.
    DATA elem    TYPE REF TO if_ixml_element.
    DATA value   TYPE string.
    DATA cmp     TYPE seocmpkey.
    DATA vis     TYPE string.

    CALL FUNCTION 'SEO_METHOD_READ_ALL'
         EXPORTING
              cifkey  = key
         IMPORTING
              methods = methods
         EXCEPTIONS
              OTHERS  = 1.

    LOOP AT methods INTO method.
      elem = doc->create_element( name = 'method' ).
      CALL METHOD parent->append_child( new_child = elem ).

      cmp-clsname = method-clsname.
      cmp-cmpname = method-cmpname.

      value = method-cmpname.
      CALL METHOD elem->set_attribute( name = 'name' value = value ).

      IF method-mtdtype EQ 1.
        CALL METHOD elem->set_attribute(
          name = 'event_handler' value = 'true' ).
      ENDIF.

      IF method-mtddecltyp EQ 1.
        CALL METHOD elem->set_attribute(
          name = 'static' value = 'true' ).
      ENDIF.

      CASE method-exposure.
        WHEN 0.
          vis = 'private'.
        WHEN 1.
          vis = 'private'.
        WHEN 2.
          vis = 'public'.
      ENDCASE.
      CALL METHOD elem->set_attribute(
          name = 'exposure' value = vis ).

      CALL METHOD add_field EXPORTING elem = elem:
        name = 'description'  value = method-descript,
        name = 'refClass'     value = method-refclsname,
        name = 'refInterface' value = method-refintname,
        name = 'refName'      value = method-refcmpname.

      CALL METHOD put_params
         EXPORTING parent = elem
                   cmp    = cmp.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_field.
    DATA name_str TYPE string.
    DATA val_str  TYPE string.
    DATA fld      TYPE REF TO if_ixml_element.

    IF check EQ abap_true AND value IS INITIAL.
      EXIT.
    ENDIF.

    name_str = name.
    val_str = value.
    fld = doc->create_element( name = name_str ).

    CALL METHOD fld->set_value( value = val_str ).

    CALL METHOD elem->append_child( new_child = fld ).
  ENDMETHOD.

  METHOD put_params.
    DATA params   TYPE  seos_parameters_r.
    DATA param    TYPE  seos_parameter_r.
    DATA elem     TYPE REF TO if_ixml_element.
    DATA value    TYPE string.
    DATA kind(20) TYPE c.

    CALL FUNCTION 'SEO_PARAMETER_READ_ALL'
         EXPORTING
              cmpkey     = cmp
         IMPORTING
              parameters = params
         EXCEPTIONS
              OTHERS     = 2.

    LOOP AT params INTO param.
      elem = doc->create_element( name = 'param' ).
      CALL METHOD parent->append_child( new_child = elem ).

      value = param-sconame.
      CALL METHOD elem->set_attribute( name = 'name' value = value ).

      IF param-parpasstyp EQ 1.
        CALL METHOD elem->set_attribute(
           name = 'byValue' value = 'true' ).
      ENDIF.

      CASE param-pardecltyp.
        WHEN 0.
          kind = 'importing'.
        WHEN 1.
          kind = 'exporting'.
        WHEN 2.
          kind = 'changing'.
        WHEN 3.
          kind = 'returning'.
      ENDCASE.

      CALL METHOD add_field EXPORTING elem = elem :
         name = 'description' value = param-descript,
         name = 'type'        value = param-type,
         name = 'kind'        value = kind.
    ENDLOOP.
  ENDMETHOD.

  METHOD f4_fname.
    DATA fname_str TYPE string.
    DATA path  TYPE string.
    DATA fullpath TYPE string.
    DATA action    TYPE i.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_extension = 'XML'
        default_file_name = 'methods.xml'
      CHANGING
        filename          = fname_str
        path              = path
        fullpath          = fullpath
        user_action       = action
    EXCEPTIONS
      OTHERS            = 3
            .
    IF sy-subrc EQ 0 AND
       action  EQ cl_gui_frontend_services=>action_ok.
      fname = fullpath.
    ENDIF.
  ENDMETHOD.
ENDCLASS.