#!/usr/bin/python3

# program to run through all the interesting metrics (i.e. those that are related in some way to a resident VM)
# and output a graph suitable for graphviz.
# note that graphviz comments are /*C-style*/
# the fundamental type here is a 'record dictionary', indexing object records by their opaque refs.

import atexit
import contextlib
from pprint import pprint, pformat

import XenAPI


# given a set of object references of type 'type_string', return a
# dictionary linking the references to the associated records
def get_records(sx, object_set, type_string):
    dic = {}
    for x in object_set:
        dic[x] = sx.__getattr__(type_string).get_record(x)
    return dic


# given a record dictionary, print out the 'name_labels' of each entry if it exists, and the reference otherwise
def print_names(dictionary, title):
    print("/*")
    print(
        title,
        ":",
        ", ".join([dictionary[x].get("name_label", x) for x in dictionary.keys()]),
    )
    print("*/")


# for example, the record dictionary of VMs contains a key VIFs, which is a list of the associated VIFs
# this function will take say, the dictionary of all the resident VMs, and return the set of all the VIFs
# associated with them
def set_from_key_in_dictionary_of_records(record_dict, key_name, key_is_list=True):
    s = set()
    for v in record_dict.values():
        key = v[key_name]
        if key_is_list:
            s.update(key)
        else:
            s.add(key)
    return s


# and this composition function will, say, given the VM dictionary, and the key name VIFs, return the
# dictionary of all associated VIFs
def chase_key(sx, record_dictionary, type_name, key_name, key_is_list=True):
    new_set = set_from_key_in_dictionary_of_records(
        record_dictionary, key_name, key_is_list
    )
    return get_records(sx, new_set, type_name)


# The metrics records hold a lot of data. The following functions take a reference/record pair of a particular type
# get the associated metrics, and return a few interesting quantities we want to see on the graphs
def host_data(sx, ref, record):
    data = {}
    data["name"] = record["name_label"]
    metrics = sx.host_metrics.get_record(sx.host.get_metrics(ref))
    cpu_utilisations = [
        sx.host_cpu.get_utilisation(x) for x in sx.host.get_host_CPUs(ref)
    ]
    data["label"] = {}
    data["label"]["cpus"] = " ".join(["%.2f" % x for x in cpu_utilisations])
    data["label"]["memory"] = "%.2f" % (
        float(metrics["memory_free"]) / float(metrics["memory_total"])
    )
    return data


def vm_data(sx, ref, record):
    data = {}
    data["name"] = record["name_label"]
    metrics = sx.VM_metrics.get_record(sx.VM.get_metrics(ref))
    data["label"] = {}
    data["label"]["cpus"] = " ".join(
        ["%.2f" % x for x in metrics["VCPUs_utilisation"].values()]
    )
    data["label"]["memory"] = "%sM" % (
        float(metrics["memory_actual"]) / float(1024 * 1024)
    )
    return data


# vifs vbds and pifs all work the same way, but we need a type variable for the xapi bindings dispatcher
def io_data(sx, ref, record, type_name, data_owner, suffixes):
    data = {}
    data["name"] = type_name.lower()
    obj_class = sx.__getattr__(type_name)
    owner_class = sx.__getattr__(data_owner)
    belongs_to = obj_class.__getattr__(f"get_{data_owner}")(ref)
    device_number = obj_class.__getattr__("get_device")(ref)
    metric_name = f"{type_name.lower()}_{device_number}"
    metrics = [
        x
        for x in owner_class.get_data_sources(belongs_to)
        if x["name_label"].startswith(metric_name)
    ]
    metrics = {x["name_label"]: x["value"] for x in metrics}

    data["label"] = {}
    data["label"]["read"] = "%.2fk" % (metrics[f"{metric_name}_{suffixes[0]}"])
    data["label"]["write"] = "%.2fk" % (metrics[f"{metric_name}_{suffixes[1]}"])
    return data


# these functions use the object lists constructed and metric analysis functions defined above
# to print out the node and edge definitions required by graphviz
def print_nodes(record_dictionary):
    for x in record_dictionary.keys():
        print('"%s" [label="%s"];' % (x, record_dictionary[x].get("name_label", "?")))


# calls the metric analysis functions to output nodes labelled with their metrics as well as their names
def print_metric_nodes(sx, dic, metricfn):
    for ref, record in dic.items():
        d = metricfn(sx, ref, record)
        label = (
            d["name"]
            + "\\n"
            + "\\n".join(["%s=%s" % (k, v) for k, v in d["label"].items()])
        )
        print('"%s" [label="%s"];' % (ref, label))


# prints out the connecting edges, in similar manner to the key-chasing above when we first got the objects
def print_edges(record_dictionary, key, key_is_list=True):
    for k, v in record_dictionary.items():
        if key_is_list:
            for x in v[key]:
                print('"%s" -> "%s";' % (k, x))
        else:
            print('"%s" -> "%s";' % (k, v[key]))


def main():
    session = XenAPI.xapi_local()

    def logout():
        with contextlib.suppress(Exception):
            session.xenapi.session.logout()

    atexit.register(logout)

    session.xenapi.login_with_password("", "", "1.0", "newmetricsgraph-script")
    sx = session.xenapi

    # find all the hosts
    host_dic = get_records(sx, set(sx.host.get_all()), "host")

    # chase the chain of types through hosts->VMs->VIFs->networks->PIFs and hosts->VMs->VBDs
    resident_vms_dic = chase_key(sx, host_dic, "VM", "resident_VMs")
    active_vifs_dic = chase_key(sx, resident_vms_dic, "VIF", "VIFs")
    active_vbds_dic = chase_key(sx, resident_vms_dic, "VBD", "VBDs")
    active_networks_dic = chase_key(sx, active_vifs_dic, "network", "network", False)
    active_pifs_dic = chase_key(sx, active_networks_dic, "PIF", "PIFs")

    # print out the objects we found as a graphviz comment
    print_names(host_dic, "hosts")
    print_names(resident_vms_dic, "resident VMs")
    print_names(active_vifs_dic, "active VIFs")
    print_names(active_vbds_dic, "active VBDs")
    print_names(active_networks_dic, "active networks")
    print_names(active_pifs_dic, "active PIFs")

    # We've now got all the objects we need, so we now need to get their metrics data and output it as a graphviz file

    print('digraph active_objects {')
    print('node [shape="rect"];')

    print_metric_nodes(sx, host_dic, host_data)
    print_metric_nodes(sx, resident_vms_dic, vm_data)
    print_metric_nodes(
        sx,
        active_vifs_dic,
        lambda sx, ref, record: io_data(sx, ref, record, "VIF", "VM", ["rx", "tx"]),
    )
    print_metric_nodes(
        sx,
        active_vbds_dic,
        lambda sx, ref, record: io_data(
            sx, ref, record, "VBD", "VM", ["read", "write"]
        ),
    )
    print_metric_nodes(
        sx,
        active_pifs_dic,
        lambda sx, ref, record: io_data(sx, ref, record, "PIF", "host", ["rx", "tx"]),
    )

    print_nodes(active_networks_dic)

    print_edges(host_dic, "resident_VMs")
    print_edges(resident_vms_dic, "VIFs")
    print_edges(resident_vms_dic, "VBDs")
    print_edges(active_vifs_dic, "network", False)
    print_edges(active_networks_dic, "PIFs")

    print("}")


if __name__ == "__main__":
    main()
