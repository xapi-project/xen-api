#!/usr/bin/env python
# Copyright (c) 2011 Citrix Systems, Inc. All use and distribution of this
# copyrighted material is governed by and subject to terms and conditions
# as licensed by Citrix Systems, Inc. All other rights reserved.
# Xen, XenSource and XenEnterprise are either registered trademarks or
# trademarks of Citrix Systems, Inc. in the United States and/or other 
# countries.

import os
import shutil
import sys
import tempfile
import StringIO

import accessor
import bootloader
import cpiofile
import repository
import xelogging
import XenAPI
import XenAPIPlugin

boot_files = [ 'install.img', 'boot/vmlinuz', 'boot/xen.gz']

def get_boot_files(accessor, dest_dir):
    done = True
    accessor.start()
    for f in boot_files:
        try:
            xelogging.log("Fetching "+f)
            inf = accessor.openAddress(f)
            if dest_dir:
                outf = open(os.path.join(dest_dir, os.path.basename(f)), 'w')
                outf.writelines(inf)
                outf.close()
            inf.close()
        except:
            done = False
            break
    accessor.finish()
    return done

def gen_answerfile(installer_dir, url):
    root_device = None

    try:
        # determine root disk
        f = open('/etc/xensource-inventory')
        for l in f:
            line = l.strip()
            if line.startswith('PRIMARY_DISK='):
                root_device = line[13:].strip("'")
                break
        f.close()
    except:
        return False
    if not root_device:
        return False

    if not os.path.exists(root_device):
        return False

    xelogging.log("Root device: "+root_device)
    
    in_arc = cpiofile.CpioFile.open(installer_dir+'/install.img', 'r|*')
    out_arc = cpiofile.CpioFile.open(installer_dir+'/upgrade.img', 'w|gz')
    out_arc.hardlinks = False

    # copy initrd
    xelogging.log("Copying initrd...")
    for f in in_arc:
        data = None
        if f.size > 0:
            data = in_arc.extractfile(f)
        out_arc.addfile(f, data)
    in_arc.close()

    # create answerfile
    xelogging.log("Creating answerfile")
    text = '<?xml version="1.0"?>\n'
    text += ' <installation mode="upgrade">\n'
    text += '  <existing-installation>%s</existing-installation>\n' % root_device
    text += '  <source type="url">%s</source>\n' % url
    text += ' </installation>\n'
    
    contents = StringIO.StringIO(text)

    f = cpiofile.CpioInfo('answerfile')
    f.size = len(contents.getvalue())
    out_arc.addfile(f, contents)

    out_arc.close()

    return True

def get_mgmt_config():
    ret = None

    session = XenAPI.xapi_local()
    session.xenapi.login_with_password('', '')
    this_host = session.xenapi.session.get_this_host(session._session)
    host_record = session.xenapi.host.get_record(this_host)

    for pif in host_record['PIFs']:
        pif_record = session.xenapi.PIF.get_record(pif)
        if pif_record['management']:
            ret = pif_record
            break
        
    session.xenapi.session.logout()
    return ret

def set_boot_config(installer_dir):
    try:
        config = bootloader.Bootloader.loadExisting()

        default = config.menu[config.default]
        if 'upgrade' in config.menu_order:
            config.remove('upgrade')
        else:
            config.commit(os.path.join(installer_dir, os.path.basename(config.src_file)))

        pif = get_mgmt_config()

        xen_args = ['dom0_mem=752M']
        xen_args.extend(filter(lambda x: x.startswith('com') or x.startswith('console='), default.hypervisor_args.split()))
        kernel_args = filter(lambda x: x.startswith('console=') or x.startswith('xencons='), default.kernel_args.split())
        kernel_args.extend(['install', 'answerfile=file:///answerfile'])

        if pif['ip_configuration_mode'] == 'Static':
            config_str = "static;ip=%s;netmask=%s" % (pif['IP'], pif['netmask'])
            if 'gateway' in pif:
                config_str += ";gateway=" + pif['gateway']
            if 'DNS' in pif:
                config_str += ";dns=" + ','.join(pif['DNS'])
            kernel_args.extend(['network_device='+pif['MAC'],
                                'network_config=static;'+config_str])
        else:
            kernel_args.append('network_device=' + pif['MAC'])

        e = bootloader.MenuEntry(installer_dir+'/xen.gz', ' '.join(xen_args),
                                 installer_dir+'/vmlinuz', ' '.join(kernel_args),
                                 installer_dir+'/upgrade.img', 'Rolling pool upgrade')
        config.append('upgrade', e)
        config.default = 'upgrade'
        xelogging.log("Writing updated bootloader config")
        config.commit()
    except:
        return False

    return True

def test_repo(url):
    try:
        a = accessor.createAccessor(url)
    except:
        return False
    if not get_boot_files(a, None):
        return False
    r = repository.Repository.findRepositories(a)
    return len(r) > 0

def prepare_host_upgrade(url):
    installer_dir = '/boot/installer'
    done = True

    # download the installer files
    try:
        shutil.rmtree(installer_dir)
    except:
        pass
    os.mkdir(installer_dir, 0700)

    a = accessor.createAccessor(url)
    done = get_boot_files(a, installer_dir)

    if done:
        done = gen_answerfile(installer_dir, url)
        
    if done:
        # create bootloader entry
        set_boot_config(installer_dir)
        
    if not done:
        try:
            shutil.rmtree(installer_dir)
        except:
            pass
    return done

# plugin url test
def testUrl(session, args):
    try:
        url = args['url']
    except KeyError:
        raise Exception('MISSING_URL')

    if not test_repo(url):
        raise Exception('INVALID_URL')
    else:
        return "true"
    
# plugin entry point
def main(session, args):
    xelogging.logToStderr()

    try:
        url = args['url']
    except KeyError:
        xelogging.log("Missing argument 'url'")
        raise Exception('MISSING_URL')

    xelogging.log("Verifying repo...")
    succeeded = False
    if not test_repo(url):
        xelogging.log("%s is not a valid repo" % url)
        raise Exception('INVALID_URL')
    else:
        xelogging.log("Repo ok, preparing for upgrade.")
        succeeded = prepare_host_upgrade(url)

    if succeeded:
        xelogging.log("Preparation succeeded, ready for upgrade.")
        return "true"
    else:
        xelogging.log("There was an error in preparing the host for upgrade.")
        raise Exception('ERROR_PREPARING_HOST')

if __name__ == '__main__':
    XenAPIPlugin.dispatch({"main": main,
                           "testUrl": testUrl})
