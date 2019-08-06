let filename = Filename.concat "." "setup.py"

let setuppy = Printf.sprintf
    "from setuptools import setup\n\
     \n\
     setup(version='%s')\n" Datamodel_common.api_version_string

let () =
  Xapi_stdext_unix.Unixext.write_string_to_file filename setuppy
