+++
title = "Adding a field to the API"
+++
This page describes how to add a field to XenAPI. A field is a parameter of a class that can be used in functions and read from the API. 

Bumping the database schema version
-----------------------------------
Whenever a field is added to or removed from the API, its schema version needs
to be increased. XAPI needs this fundamental procedure in order to be able to
detect that an automatic database upgrade is necessary or to find out that the
new schema is incompatible with the existing database. If the schema version is
not bumped, XAPI will start failing in unpredictable ways. Note that bumping
the version is not necessary when adding functions, only when adding fields.

The current version number is kept at the top of the file
`ocaml/idl/datamodel_common.ml` in the variables `schema_major_vsn` and
`schema_minor_vsn`, of which only the latter should be incremented (the major
version only exists for historical reasons). When moving to a new XenServer
release, also update the variable `last_release_schema_minor_vsn` to the schema
version of the last release. To keep track of the schema versions of recent
XenServer releases, the file contains variables for these, such as
`miami_release_schema_minor_vsn`. After starting a new version of Xapi on an
existing server, the database is automatically upgraded if the schema version
of the existing database matches the value of `last_release_schema_*_vsn` in the
new Xapi.

As an example, the patch below shows how the schema version was bumped when the
new API fields used for ActiveDirectory integration were added:

    --- a/ocaml/idl/datamodel.ml  Tue Nov 11 16:17:48 2008 +0000
    +++ b/ocaml/idl/datamodel.ml  Tue Nov 11 15:53:29 2008 +0000
    @@ -15,17 +15,20 @@ open Datamodel_types
      open Datamodel_types

      (* IMPORTANT: Please bump schema vsn if you change/add/remove a _field_.
         You do not have to dump vsn if you change/add/remove a message *)
    
      let schema_major_vsn = 5
     -let schema_minor_vsn = 55
     +let schema_minor_vsn = 56
    
      (* Historical schema versions just in case this is useful later *)
      let rio_schema_major_vsn = 5
      let rio_schema_minor_vsn = 19
    
     +let miami_release_schema_major_vsn = 5
     +let miami_release_schema_minor_vsn = 35
     +
      (* the schema vsn of the last release: used to determine whether we can
         upgrade or not.. *)
      let last_release_schema_major_vsn = 5
     -let last_release_schema_minor_vsn = 35
     +let last_release_schema_minor_vsn = 55

### Setting the schema hash

In the `ocaml/idl/schematest.ml` there is the `last_known_schema_hash` This needs to be updated to be the next hash after the schema version was bumped. Get the new hash by running `make test` and you will receive the correct hash in the error message.

Adding the new field to some existing class
-------------------------------------------

### ocaml/idl/datamodel.ml

Add a new "field" line to the class in the file `ocaml/idl/datamodel.ml` or `ocaml/idl/datamodel_[class].ml`. The new field might require
a suitable default value. This default value is used in case the user does not
provide a value for the field.

A field has a number of parameters:

- The lifecycle parameter, which shows how the field has evolved over time.
- The qualifier parameter, which controls access to the field. The following
  values are possible:

| Value     | Meaning                                       |
| --------- | --------------------------------------------- |
| StaticRO  | Field is set statically at install-time.      |
| DynamicRO | Field is computed dynamically at run time.    |
| RW        | Field is read/write.                          |

- The ty parameter for the type of the field.
- The default_value parameter.
- The name of the field.
- A documentation string.

Example of a field in the pool class:

    field ~lifecycle:[Published, rel_orlando, "Controls whether HA is enabled"]
          ~qualifier:DynamicRO ~ty:Bool
          ~default_value:(Some (VBool false)) "ha_enabled" "true if HA is enabled on the pool, false otherwise";

See datamodel_types.ml for information about other parameters.

## Changing Constructors

Adding a field would change the constructors for the class – functions
Db.*.create – and therefore, any references to these in the code need to be
updated. In the example, the argument ~ha_enabled:false should be added to any
call to Db.Pool.create. 

Examples of where these calls can be found is in `ocaml/tests/common/test_common.ml` and `ocaml/xapi/xapi_[class].ml`.

### CLI Records

If you want this field to show up in the CLI (which you probably do), you will
also need to modify the Records module, in the file
`ocaml/xapi-cli-server/records.ml`. Find the record function for the class which
you have modified, add a new entry to the fields list using make_field. This type can be found in the same file.

The only required parameters are name and get (and unit, of course ).
If your field is a map or set, then you will need to pass in get_{map,set}, and
optionally set_{map,set}, if it is a RW field. The hidden parameter is useful
if you don't want this field to show up in a *_params_list call. As an example,
here is a field that we've just added to the SM class:

    make_field ~name:"versioned-capabilities"
               ~get:(fun () -> Record_util.s2sm_to_string "; " (x ()).API.sM_versioned_capabilities)
               ~get_map:(fun () -> (x ()).API.sM_versioned_capabilities)
               ~hidden:true ();

Testing
-------
The new fields can be tested by copying the newly compiled xapi binary to a
test box. After the new xapi service is started, the file
*/var/log/xensource.log* in the test box should contain a few lines reporting the
successful upgrade of the metadata schema in the test box:

    [...|xapi] Db has schema major_vsn=5, minor_vsn=57 (current is 5 58) (last is 5 57)
    [...|xapi] Database schema version is that of last release: attempting upgrade
    [...|sql] attempting to restore database from /var/xapi/state.db
    [...|sql] finished parsing xml
    [...|sql] writing db as xml to file '/var/xapi/state.db'.
    [...|xapi] Database upgrade complete, restarting to use new db

Making this field accessible as a CLI attribute
-----------------------------------------------
XenAPI functions to get and set the value of the new field are generated
automatically. It requires some extra work, however, to enable such operations
in the CLI.

The CLI has commands such as host-param-list and host-param-get. To make a new
field accessible by these commands, the file `xapi-cli-server/records.ml` needs to
be edited. For the pool.ha-enabled field, the pool_record function in this file
contains the following (note the convention to replace underscores by hyphens
in the CLI):

    let pool_record rpc session_id pool =
      ...
    [
      ...
      make_field ~name:"ha-enabled" ~get:(fun () -> string_of_bool (x ()).API.pool_ha_enabled) ();
      ...
    ]}

NB: the ~get parameter must return a string so include a relevant function to convert the type of the field into a string i.e. `string_of_bool`

See `xapi-cli-server/records.ml` for examples of handling field types other than Bool.
