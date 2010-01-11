let simple_call =
"<methodCall>
  <methodName>session.login_with_password</methodName>
  <params>
    <param>
      <value/>
    </param>
    <param>
      <value/>
    </param>
    <param>
      <value>1.4</value>
    </param>
  </params>
</methodCall>
"

let error = 
"<methodResponse>
<fault>
<value><struct>
<member>
<name>faultCode</name>
<value><int>143</int></value>
</member>
<member>
<name>faultString</name>
<value><string>Failed to parse the request</string></value>
</member>
</struct></value>
</fault>
</methodResponse>
"

let sm =
"<?xml version='1.0'?>
<methodResponse>
<params>
<param>
<value><struct>
<member>
<name>required_api_version</name>
<value><string>1.0</string></value>
</member>
<member>
<name>vendor</name>
<value><string>Citrix Systems Inc</string></value>
</member>
<member>
<name>name</name>
<value><string>Local EXT3 VHD</string></value>
</member>
<member>
<name>copyright</name>
<value><string>(C) 2008 Citrix Systems Inc</string></value>
</member>
<member>
<name>capabilities</name>
<value><array><data>
<value><string>SR_PROBE</string></value>
<value><string>SR_UPDATE</string></value>
<value><string>VDI_CREATE</string></value>
<value><string>VDI_DELETE</string></value>
<value><string>VDI_ATTACH</string></value>
<value><string>VDI_DETACH</string></value>
<value><string>VDI_UPDATE</string></value>
<value><string>VDI_CLONE</string></value>
<value><string>VDI_SNAPSHOT</string></value>
<value><string>VDI_RESIZE</string></value>
<value><string>VDI_RESIZE_ONLINE</string></value>
</data></array></value>
</member>
<member>
<name>driver_version</name>
<value><string>1.0</string></value>
</member>
<member>
<name>configuration</name>
<value><array><data>
<value><struct>
<member>
<name>description</name>
<value><string>local device path (required) (e.g. /dev/sda3)</string></value>
</member>
<member>
<name>key</name>
<value><string>device</string></value>
</member>
</struct></value>
</data></array></value>
</member>
<member>
<name>description</name>
<value><string>SR plugin which represents disks as VHD files stored on a local EXT3 filesystem, created inside an LVM volume</string></value>
</member>
</struct></value>
</param>
</params>
</methodResponse>
"

let empty = "<value></value>"

let _ =
	Printf.printf "Parsing SM XML ... %!";
	Xmlrpc.response_of_string sm;

	Printf.printf "OK\nParsing empty tags ... %!";
	Xmlrpc.of_string empty;

	Printf.printf "OK\nParsing error ... %!";
	Xmlrpc.response_of_string error;

	Printf.printf "OK\nParsing simple call ... %!";
	Xmlrpc.call_of_string simple_call;

	Printf.printf "OK\n%!"

