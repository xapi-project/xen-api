run the self-checks for xapi-storage-script, it logs to stderr, so process
stderr instead of stdout

The output of the logs needs to delete randomization, there are two sources:
pids and uuids

  $ export PYTHONPATH=../xapi-storage/python/; ./main.exe --root=$PWD/test --self-test-only=true 2>&1 >/dev/null | sed -E 's/\[[0-9]+\]/[PID]/g' | sed -E 's/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/UUID/g'
  [INFO] {"method":"Plugin.query","params":[{"dbg":"debug"}],"id":2}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/Plugin.Query[PID] succeeded: {"plugin": "dummy", "name": "dummy SR plugin", "description": "Dummy v5 SR for unit tests.", "vendor": "Citrix Systems Inc", "copyright": "(C) 2018 Citrix Inc", "version": "1.0", "required_api_version": "5.0", "features": ["SR_ATTACH", "SR_DETACH", "SR_CREATE", "SR_PROBE", "VDI_CREATE", "VDI_DESTROY"], "configuration": {}, "required_cluster_stack": []}
  
  [INFO] {"method":"Plugin.diagnostics","params":[{"dbg":"debug"}],"id":4}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/Plugin.diagnostics[PID] succeeded: "Dummy diagnostics"
  
  [INFO] {"method":"SR.create","params":[{"description":"dummy description","name":"dummy name","configuration":{"uri":"file:///dev/null"},"uuid":"dummySR","dbg":"debug"}],"id":6}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/SR.create[PID] succeeded: {"uri": "file:///tmp/dummy"}
  
  [INFO] {"method":"SR.attach","params":[{"configuration":{"uri":"file:///tmp/dummy"},"dbg":"debug"}],"id":9}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/SR.attach[PID] succeeded: "file:///tmp/dummy"
  
  [INFO] {"method":"SR.stat","params":[{"sr":"file:///tmp/dummy","dbg":"debug"}],"id":10}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/SR.stat[PID] succeeded: {"sr": "file:///tmp/dummy", "name": "dummy SR plugin", "description": "Dummy v5 SR for unit tests.", "total_space": 0, "free_space": 0, "datasources": [], "clustered": false, "health": ["Healthy", ""]}
  
  [INFO] {"method":"Volume.create","params":[{"sharable":false,"size":0,"description":"vdi description","name":"vdi name","sr":"file:///tmp/dummy","dbg":"debug"}],"id":12}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/Volume.create[PID] succeeded: {"name": "vdi name", "description": "vdi description", "key": "UUID", "uuid": "UUID", "read_write": true, "sharable": false, "virtual_size": 0, "physical_utilisation": 0, "uri": ["raw+file:///tmp/disk.raw"], "keys": {}}
  
  [INFO] {"method":"Volume.set","params":[{"v":"redolog","k":"vdi-type","key":"UUID","sr":"file:///tmp/dummy","dbg":"debug"}],"id":13}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/Volume.set[PID] succeeded: null
  
  [INFO] {"method":"Volume.stat","params":[{"key":"UUID","sr":"file:///tmp/dummy","dbg":"debug"}],"id":15}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/Volume.stat[PID] succeeded: {"name": "dummy SR plugin", "description": "Dummy v5 SR for unit tests.", "key": "UUID", "uuid": "UUID", "read_write": true, "virtual_size": 0, "physical_utilisation": 0, "sharable": false, "uri": ["raw+file:///tmp/disk.raw"], "keys": {}}
  
  [INFO] {"method":"Volume.stat","params":[{"key":"UUID","sr":"file:///tmp/dummy","dbg":"debug"}],"id":17}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/Volume.stat[PID] succeeded: {"name": "dummy SR plugin", "description": "Dummy v5 SR for unit tests.", "key": "UUID", "uuid": "UUID", "read_write": true, "virtual_size": 0, "physical_utilisation": 0, "sharable": false, "uri": ["raw+file:///tmp/disk.raw"], "keys": {}}
  
  [INFO] {"method":"Volume.destroy","params":[{"key":"UUID","sr":"file:///tmp/dummy","dbg":"debug"}],"id":18}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/Volume.destroy[PID] succeeded: null
  
  [INFO] {"method":"SR.stat","params":[{"sr":"file:///tmp/dummy","dbg":"debug"}],"id":20}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/SR.stat[PID] succeeded: {"sr": "file:///tmp/dummy", "name": "dummy SR plugin", "description": "Dummy v5 SR for unit tests.", "total_space": 0, "free_space": 0, "datasources": [], "clustered": false, "health": ["Healthy", ""]}
  
  [INFO] {"method":"SR.ls","params":[{"sr":"file:///tmp/dummy","dbg":"debug"}],"id":22}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/SR.ls[PID] succeeded: [{"name": "dummy SR plugin", "description": "Dummy v5 SR for unit tests.", "key": "file1", "uuid": "file1", "read_write": true, "virtual_size": 0, "physical_utilisation": 0, "sharable": false, "uri": ["raw+file:///tmp/disk.raw"], "keys": {}}]
  
  [INFO] {"method":"SR.probe","params":[{"configuration":{"uri":"file:///tmp/dummy"},"dbg":"debug"}],"id":24}
  [INFO] $TESTCASE_ROOT/test/volume/org.xen.xapi.storage.dummyv5/SR.probe[PID] succeeded: [{"configuration": {"uri": "file:///tmp/dummy"}, "complete": true, "extra_info": {}}, {"configuration": {"uri": "file:///tmp/dummy", "sr_uuid": "myuuid"}, "sr": {"sr": "file:///tmp/dummy", "name": "dummy SR plugin", "description": "Dummy v5 SR for unit tests.", "total_space": 0, "free_space": 0, "datasources": [], "clustered": false, "health": ["Healthy", ""]}, "complete": true, "extra_info": {}}]
  
  [INFO] test thread shutdown cleanly
