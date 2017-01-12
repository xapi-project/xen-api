#!/usr/bin/env python

import xmlrpclib
import sys

s=xmlrpclib.Server("http://localhost/")
sess=s.session.login_with_password("root","xenroot")['Value']

pool = s.pool.get_all(sess)['Value'][0]
restrictions = s.pool.get_restrictions(sess,pool)['Value']

base_request = {'user_version':'1', 'is_a_template':False, 'affinity':'', 'memory_static_max':'4', 'memory_static_min':'1', 'memory_dynamic_max':'3', 'memory_dynamic_min':'2', 'VCPUs_params':{}, 'VCPUs_max':'1', 'VCPUs_at_startup':'1', 'name_label':'hello', 'name_description':'hi', 'memory_target':'2', 'actions_after_shutdown':'destroy', 'actions_after_reboot':'restart', 'actions_after_crash':'destroy', 'PV_bootloader':'', 'PV_kernel':'', 'PV_ramdisk':'', 'PV_args':'', 'PV_bootloader_args':'', 'PV_legacy_args':'', 'HVM_boot_policy':'', 'HVM_boot_params':{}, 'HVM_shadow_multiplier':1.0, 'platform':{}, 'PCI_bus':'', 'other_config':{}, 'recommendations':'', 'xenstore_data':{}, 'ha_always_run':False, 'ha_restart_priority':'1', 'tags':[], 'blocked_operations':{}, 'protection_policy':'', 'is_snapshot_from_vmpp':False, 'appliance':'', 'start_delay':'0', 'shutdown_delay':'0', 'order':'0', 'suspend_SR':'', 'version':'0', 'generation_id':'', 'hardware_platform_version':'0'}

#

def create():
    res = s.VM.create(sess, base_request)
    return res

def create_with_vd(b):
    request = base_request.copy()
    request['has_vendor_device']=b
    return s.VM.create(sess,request)

# VD in request | OK by license | pool.policy_no_vendor_device | resulting VM.has_vendor_device
#    -          | False         | False                        | False
#    False      | False         | False                        | False
#    True       | False         | False                        | Failure
#    -          | False         | True                         | False
#    False      | False         | True                         | False
#    True       | False         | True                         | Failure


def test_with_restriction(): # OK by license column above
    # Expect this to be successful on an unlicensed host, and for the field to be 'false'
    print "running restricted tests (license says you're not allowed the vendor device)"

    s.pool.set_policy_no_vendor_device(sess,pool,False)

#    -          | False         | False                        | False
    res = create()
    vm = res['Value']
    expected = False
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    False      | False         | False                        | False
    res = create_with_vd(False)
    vm = res['Value']
    expected = False
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    True       | False         | False                        | Failure
    res = create_with_vd(True)
    print "Expecting failure: got %s" % res['Status']
    assert(res['Status']=='Failure')

    s.pool.set_policy_no_vendor_device(sess,pool,True)

#    -          | False         | True                         | False
    res = create()
    vm = res['Value']
    expected = False
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    False      | False         | True                         | False
    res = create_with_vd(False)
    vm = res['Value']
    expected = False
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    True       | False         | True                         | Failure
    res = create_with_vd(True)
    print "Expecting failure: got %s" % res['Status']
    assert(res['Status']=='Failure')


    
def test_no_restriction():
    print "running unrestricted tests"

#    -          | True          | False                        | True
#    False      | True          | False                        | False
#    True       | True          | False                        | True
#    -          | True          | True                         | False
#    False      | True          | True                         | False
#    True       | True          | True                         | True

    s.pool.set_policy_no_vendor_device(sess,pool,False)

#    -          | True          | False                        | True
    res = create()
    vm = res['Value']
    expected = True
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    False      | True          | False                        | False
    res = create_with_vd(False)
    vm = res['Value']
    expected = False
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    True       | True          | False                        | True
    res = create_with_vd(True)
    vm = res['Value']
    expected = True
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

    s.pool.set_policy_no_vendor_device(sess,pool,True)

#    -          | True          | True                         | False
    res = create()
    vm = res['Value']
    expected = False
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    False      | True          | True                         | False
    res = create_with_vd(False)
    vm = res['Value']
    expected = False
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

#    True       | True          | True                         | True
    res = create_with_vd(True)
    vm = res['Value']
    expected = True
    found = s.VM.get_has_vendor_device(sess,vm)['Value']
    print "Expecting has-vendor-device to be %s: got %s" % (expected,found)
    assert(expected == found)

    

if restrictions['restrict_pci_device_for_auto_update'] == "true":
    test_with_restriction()
else:
    test_no_restriction()

    



