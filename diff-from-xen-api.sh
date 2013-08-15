XAPI=xen-api/ocaml/xenops
XENOPSD=xenopsd

diff -up ${XAPI}/bootloader.ml ${XENOPSD}/lib/bootloader.ml
diff -up ${XAPI}/bootloader.mli ${XENOPSD}/lib/bootloader.mli
diff -up ${XAPI}/cancel_utils.ml ${XENOPSD}/xc/cancel_utils.ml 
diff -up ${XAPI}/cancel_utils_test.ml ${XENOPSD}/xc/cancel_utils_test.ml
# dbgring
diff -up ${XAPI}/device_common.ml ${XENOPSD}/xc/device_common.ml
diff -up ${XAPI}/device_common.mli ${XENOPSD}/xc/device_common.mli
diff -up ${XAPI}/device.mli ${XENOPSD}/xc/device.mli
diff -up ${XAPI}/device.ml ${XENOPSD}/xc/device.ml
#device_number.ml
#device_number.mli
diff -up ${XAPI}/device_number_test.ml ${XENOPSD}/xc/device_number_test.ml 
diff -up ${XAPI}/domain.ml ${XENOPSD}/xc/domain.ml 
diff -up ${XAPI}/domain.mli ${XENOPSD}/xc/domain.mli 
diff -up ${XAPI}/domain_sethandle.ml ${XENOPSD}/xc/domain_sethandle.ml 
diff -up ${XAPI}/fence.ml ${XENOPSD}/xc/fence.ml 
diff -up ${XAPI}/hotplug.ml ${XENOPSD}/xc/hotplug.ml 
diff -up ${XAPI}/io.ml ${XENOPSD}/xc/io.ml
diff -up ${XAPI}/ionice.ml ${XENOPSD}/lib/ionice.ml
diff -up ${XAPI}/list_domains.ml ${XENOPSD}/xc/list_domains.ml 
# memory_breakdown.ml
# memory_client.ml
# memory_interface.ml
diff -up ${XAPI}/memory_breakdown.ml ${XENOPSD}/xc/memory_breakdown.ml 
diff -up ${XAPI}/memory_summary.ml ${XENOPSD}/xc/memory_summary.ml 
diff -up ${XAPI}/memory.ml ${XENOPSD}/xc/memory.ml
 diff -up ${XAPI}/netman.ml ${XENOPSD}/xc/netman.ml
#squeezed.ml
#squeeze_state.ml
#squeeze.ml
#squeeze_test_main.ml
#squeeze_test.ml
#squeeze_xen.ml
diff -up ${XAPI}/statdev.ml ${XENOPSD}/xc/statdev.ml 
diff -up ${XAPI}/statdev_stubs.c ${XENOPSD}/xc/statdev_stubs.c
diff -up ${XAPI}/stubdom.ml ${XENOPSD}/xc/stubdom.ml
diff -up ${XAPI}/stubdom.mli ${XENOPSD}/xc/stubdom.mli
diff -up ${XAPI}/task_server.ml ${XENOPSD}/lib/task_server.ml 
#tests.ml
diff -up ${XAPI}/updates.ml ${XENOPSD}/lib/updates.ml 
#xal_main.ml
#xenbus_utils
diff -up ${XAPI}/xenguestHelper.ml ${XENOPSD}/xc/xenguestHelper.ml 
#xenops_client.ml
diff -up ${XAPI}/xenopsd.ml ${XENOPSD}/lib/xenopsd.ml 
diff -up ${XAPI}/xenops_helpers.ml ${XENOPSD}/xc/xenops_helpers.ml 
diff -up ${XAPI}/xenops_hooks.ml ${XENOPSD}/lib/xenops_hooks.ml 
diff -up ${XAPI}/xenops_migrate.ml ${XENOPSD}/lib/xenops_migrate.ml 
#xenops.ml
diff -up ${XAPI}/xenops_server.ml ${XENOPSD}/lib/xenops_server.ml 
diff -up ${XAPI}/xenops_server_plugin.ml ${XENOPSD}/lib/xenops_server_plugin.ml 
diff -up ${XAPI}/xenops_server_simulator.ml ${XENOPSD}/simulator/xenops_server_simulator.ml 
diff -up ${XAPI}/xenops_server_xen.ml ${XENOPSD}/xc/xenops_server_xen.ml
diff -up ${XAPI}/xenops_task.ml ${XENOPSD}/lib/xenops_task.ml 
diff -up ${XAPI}/xenops_utils.ml ${XENOPSD}/lib/xenops_utils.ml 
diff -up ${XAPI}/xenstore_watch.ml ${XENOPSD}/xc/xenstore_watch.ml 
#xenvmlib.ml
