/*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 * 
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Collections;
using System.Collections.Generic;

using CookComputing.XmlRpc;


namespace XenAPI
{
    public partial interface Proxy : IXmlRpcProxy
    {
        #region pre-6.2 compatibility

        [XmlRpcMethod("session.login_with_password")]
        Response<string>
        session_login_with_password(string _uname, string _pwd, string _version);

        [XmlRpcMethod("host.apply_edition")]
        Response<string>
        host_apply_edition(string session, string _host, string _edition);

        #endregion

        #region pre-6.1 compatibility
        
        [XmlRpcMethod("Bond.create")]
        Response<string>
        bond_create(string session, string _network, string[] _members, string _mac, string _mode);

        [XmlRpcMethod("Async.Bond.create")]
        Response<string>
        async_bond_create(string session, string _network, string[] _members, string _mac, string _mode);
        
        [XmlRpcMethod("pool.create_new_blob")]
        Response<string>
        pool_create_new_blob(string session, string _pool, string _name, string _mime_type);
        
        [XmlRpcMethod("Async.pool.create_new_blob")]
        Response<string>
        async_pool_create_new_blob(string session, string _pool, string _name, string _mime_type);
        
        [XmlRpcMethod("VM.create_new_blob")]
        Response<string>
        vm_create_new_blob(string session, string _vm, string _name, string _mime_type);

        [XmlRpcMethod("Async.VM.create_new_blob")]
        Response<string>
        async_vm_create_new_blob(string session, string _vm, string _name, string _mime_type);
        
        [XmlRpcMethod("host.create_new_blob")]
        Response<string>
        host_create_new_blob(string session, string _host, string _name, string _mime_type);

        [XmlRpcMethod("Async.host.create_new_blob")]
        Response<string>
        async_host_create_new_blob(string session, string _host, string _name, string _mime_type);
        
        [XmlRpcMethod("network.create_new_blob")]
        Response<string>
        network_create_new_blob(string session, string _network, string _name, string _mime_type);

        [XmlRpcMethod("Async.network.create_new_blob")]
        Response<string>
        async_network_create_new_blob(string session, string _network, string _name, string _mime_type);
        
        [XmlRpcMethod("PIF.db_introduce")]
        Response<string>
        pif_db_introduce(string session, string _device, string _network, string _host, string _mac, string _mtu, string _vlan, bool _physical, string _ip_configuration_mode, string _ip, string _netmask, string _gateway, string _dns, string _bond_slave_of, string _vlan_master_of, bool _management, Object _other_config, bool _disallow_unplug);

        [XmlRpcMethod("Async.PIF.db_introduce")]
        Response<string>
        async_pif_db_introduce(string session, string _device, string _network, string _host, string _mac, string _mtu, string _vlan, bool _physical, string _ip_configuration_mode, string _ip, string _netmask, string _gateway, string _dns, string _bond_slave_of, string _vlan_master_of, bool _management, Object _other_config, bool _disallow_unplug);
        
        [XmlRpcMethod("SR.create_new_blob")]
        Response<string>
        sr_create_new_blob(string session, string _sr, string _name, string _mime_type);

        [XmlRpcMethod("Async.SR.create_new_blob")]
        Response<string>
        async_sr_create_new_blob(string session, string _sr, string _name, string _mime_type);
        
        [XmlRpcMethod("VDI.introduce")]
        Response<string>
        vdi_introduce(string session, string _uuid, string _name_label, string _name_description, string _sr, string _type, bool _sharable, bool _read_only, Object _other_config, string _location, Object _xenstore_data, Object _sm_config);

        [XmlRpcMethod("Async.VDI.introduce")]
        Response<string>
        async_vdi_introduce(string session, string _uuid, string _name_label, string _name_description, string _sr, string _type, bool _sharable, bool _read_only, Object _other_config, string _location, Object _xenstore_data, Object _sm_config);

        [XmlRpcMethod("VDI.db_introduce")]
        Response<string>
        vdi_db_introduce(string session, string _uuid, string _name_label, string _name_description, string _sr, string _type, bool _sharable, bool _read_only, Object _other_config, string _location, Object _xenstore_data, Object _sm_config);

        [XmlRpcMethod("Async.VDI.db_introduce")]
        Response<string>
        async_vdi_db_introduce(string session, string _uuid, string _name_label, string _name_description, string _sr, string _type, bool _sharable, bool _read_only, Object _other_config, string _location, Object _xenstore_data, Object _sm_config);
        
        [XmlRpcMethod("blob.create")]
        Response<string>
        blob_create(string session, string _mime_type);

        #endregion
        
        #region pre-6.0 compatibility

        [XmlRpcMethod("Bond.create")]
        Response<string>
        bond_create(string session, string _network, string[] _members, string _mac);

        [XmlRpcMethod("Async.Bond.create")]
        Response<string>
        async_bond_create(string session, string _network, string[] _members, string _mac);

        #endregion
    }

    public partial class Blob
    {
        /// <summary>
        /// Backward compatibility for Blob.create in XenServer 6.0.
        /// </summary>
        public static XenRef<Blob> create(Session session, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Blob>.Create(session.proxy.blob_create(session.uuid, (_mime_type != null) ? _mime_type : "").parse());
        }
    }

    public partial class Bond
    {
        // Backward compatibility for Bond.create in XenServer 5.x.

        public static XenRef<Bond> create(Session session, string _network, List<XenRef<PIF>> _members, string _mac)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_9))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.0 or newer.");

            return XenRef<Bond>.Create(session.proxy.bond_create(session.uuid, (_network != null) ? _network : "", (_members != null) ? Helper.RefListToStringArray(_members) : new string[] { }, (_mac != null) ? _mac : "").parse());
        }

        public static XenRef<Task> async_create(Session session, string _network, List<XenRef<PIF>> _members, string _mac)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_9))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.0 or newer.");

            return XenRef<Task>.Create(session.proxy.async_bond_create(session.uuid, (_network != null) ? _network : "", (_members != null) ? Helper.RefListToStringArray(_members) : new string[] { }, (_mac != null) ? _mac : "").parse());
        }
        
        // Backward compatibility for Bond.create in XenServer 6.0.

        public static XenRef<Bond> create(Session session, string _network, List<XenRef<PIF>> _members, string _mac, bond_mode _mode)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Bond>.Create(session.proxy.bond_create(session.uuid, (_network != null) ? _network : "", (_members != null) ? Helper.RefListToStringArray(_members) : new string[] { }, (_mac != null) ? _mac : "", bond_mode_helper.ToString(_mode)).parse());
        }

        public static XenRef<Task> async_create(Session session, string _network, List<XenRef<PIF>> _members, string _mac, bond_mode _mode)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_bond_create(session.uuid, (_network != null) ? _network : "", (_members != null) ? Helper.RefListToStringArray(_members) : new string[] { }, (_mac != null) ? _mac : "", bond_mode_helper.ToString(_mode)).parse());
        }
    }

    public partial class Host
    {
        /// <summary>
        /// Backward compatibility for Host.create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Blob> create_new_blob(Session session, string _host, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Blob>.Create(session.proxy.host_create_new_blob(session.uuid, (_host != null) ? _host : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }

        /// <summary>
        /// Backward compatibility for Host.async_create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_create_new_blob(Session session, string _host, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_host_create_new_blob(session.uuid, (_host != null) ? _host : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }
		
        /// <summary>
        /// Backward compatibility for Host.apply_edition in XenServer 6.1.
        /// </summary>
        public static void apply_edition(Session session, string _host, string _edition)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_2_0))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.2 or newer.");
				
            session.proxy.host_apply_edition(session.uuid, (_host != null) ? _host : "", (_edition != null) ? _edition : "").parse();
        }
    }

    public partial class Network
    {
        /// <summary>
        /// Backward compatibility for Network.create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Blob> create_new_blob(Session session, string _network, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Blob>.Create(session.proxy.network_create_new_blob(session.uuid, (_network != null) ? _network : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }

        /// <summary>
        /// Backward compatibility for Network.async_create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_create_new_blob(Session session, string _network, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_network_create_new_blob(session.uuid, (_network != null) ? _network : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }
    }

    public partial class PIF
    {
        /// <summary>
        /// Backward compatibility for PIF.db_introduce in XenServer 6.0.
        /// </summary>
        public static XenRef<PIF> db_introduce(Session session, string _device, string _network, string _host, string _mac, long _mtu, long _vlan, bool _physical, ip_configuration_mode _ip_configuration_mode, string _ip, string _netmask, string _gateway, string _dns, string _bond_slave_of, string _vlan_master_of, bool _management, Dictionary<string, string> _other_config, bool _disallow_unplug)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<PIF>.Create(session.proxy.pif_db_introduce(session.uuid, (_device != null) ? _device : "", (_network != null) ? _network : "", (_host != null) ? _host : "", (_mac != null) ? _mac : "", _mtu.ToString(), _vlan.ToString(), _physical, ip_configuration_mode_helper.ToString(_ip_configuration_mode), (_ip != null) ? _ip : "", (_netmask != null) ? _netmask : "", (_gateway != null) ? _gateway : "", (_dns != null) ? _dns : "", (_bond_slave_of != null) ? _bond_slave_of : "", (_vlan_master_of != null) ? _vlan_master_of : "", _management, Maps.convert_to_proxy_string_string(_other_config), _disallow_unplug).parse());
        }

        /// <summary>
        /// Backward compatibility for PIF.async_db_introduce in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_db_introduce(Session session, string _device, string _network, string _host, string _mac, long _mtu, long _vlan, bool _physical, ip_configuration_mode _ip_configuration_mode, string _ip, string _netmask, string _gateway, string _dns, string _bond_slave_of, string _vlan_master_of, bool _management, Dictionary<string, string> _other_config, bool _disallow_unplug)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_pif_db_introduce(session.uuid, (_device != null) ? _device : "", (_network != null) ? _network : "", (_host != null) ? _host : "", (_mac != null) ? _mac : "", _mtu.ToString(), _vlan.ToString(), _physical, ip_configuration_mode_helper.ToString(_ip_configuration_mode), (_ip != null) ? _ip : "", (_netmask != null) ? _netmask : "", (_gateway != null) ? _gateway : "", (_dns != null) ? _dns : "", (_bond_slave_of != null) ? _bond_slave_of : "", (_vlan_master_of != null) ? _vlan_master_of : "", _management, Maps.convert_to_proxy_string_string(_other_config), _disallow_unplug).parse());
        }
    }

    public partial class Pool : XenObject<Pool>
    {
        /// <summary>
        /// Backward compatibility for Pool.create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Blob> create_new_blob(Session session, string _pool, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Blob>.Create(session.proxy.pool_create_new_blob(session.uuid, (_pool != null) ? _pool : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }

        /// <summary>
        /// Backward compatibility for Pool.async_create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_create_new_blob(Session session, string _pool, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_pool_create_new_blob(session.uuid, (_pool != null) ? _pool : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }
    }

    public partial class SR
    {
        /// <summary>
        /// Backward compatibility for SR.create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Blob> create_new_blob(Session session, string _sr, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Blob>.Create(session.proxy.sr_create_new_blob(session.uuid, (_sr != null) ? _sr : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }

        /// <summary>
        /// Backward compatibility for SR.async_create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_create_new_blob(Session session, string _sr, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_sr_create_new_blob(session.uuid, (_sr != null) ? _sr : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }
    }

    public partial class VDI
    {
        /// <summary>
        /// Backward compatibility for VDI.introduce in XenServer 6.0.
        /// </summary>
        public static XenRef<VDI> introduce(Session session, string _uuid, string _name_label, string _name_description, string _sr, vdi_type _type, bool _sharable, bool _read_only, Dictionary<string, string> _other_config, string _location, Dictionary<string, string> _xenstore_data, Dictionary<string, string> _sm_config)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<VDI>.Create(session.proxy.vdi_introduce(session.uuid, (_uuid != null) ? _uuid : "", (_name_label != null) ? _name_label : "", (_name_description != null) ? _name_description : "", (_sr != null) ? _sr : "", vdi_type_helper.ToString(_type), _sharable, _read_only, Maps.convert_to_proxy_string_string(_other_config), (_location != null) ? _location : "", Maps.convert_to_proxy_string_string(_xenstore_data), Maps.convert_to_proxy_string_string(_sm_config)).parse());
        }

        /// <summary>
        /// Backward compatibility for VDI.async_introduce in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_introduce(Session session, string _uuid, string _name_label, string _name_description, string _sr, vdi_type _type, bool _sharable, bool _read_only, Dictionary<string, string> _other_config, string _location, Dictionary<string, string> _xenstore_data, Dictionary<string, string> _sm_config)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_vdi_introduce(session.uuid, (_uuid != null) ? _uuid : "", (_name_label != null) ? _name_label : "", (_name_description != null) ? _name_description : "", (_sr != null) ? _sr : "", vdi_type_helper.ToString(_type), _sharable, _read_only, Maps.convert_to_proxy_string_string(_other_config), (_location != null) ? _location : "", Maps.convert_to_proxy_string_string(_xenstore_data), Maps.convert_to_proxy_string_string(_sm_config)).parse());
        }

        /// <summary>
        /// Backward compatibility for VDI.db_introduce in XenServer 6.0.
        /// </summary>
        public static XenRef<VDI> db_introduce(Session session, string _uuid, string _name_label, string _name_description, string _sr, vdi_type _type, bool _sharable, bool _read_only, Dictionary<string, string> _other_config, string _location, Dictionary<string, string> _xenstore_data, Dictionary<string, string> _sm_config)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<VDI>.Create(session.proxy.vdi_db_introduce(session.uuid, (_uuid != null) ? _uuid : "", (_name_label != null) ? _name_label : "", (_name_description != null) ? _name_description : "", (_sr != null) ? _sr : "", vdi_type_helper.ToString(_type), _sharable, _read_only, Maps.convert_to_proxy_string_string(_other_config), (_location != null) ? _location : "", Maps.convert_to_proxy_string_string(_xenstore_data), Maps.convert_to_proxy_string_string(_sm_config)).parse());
        }

        /// <summary>
        /// Backward compatibility for VDI.async_db_introduce in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_db_introduce(Session session, string _uuid, string _name_label, string _name_description, string _sr, vdi_type _type, bool _sharable, bool _read_only, Dictionary<string, string> _other_config, string _location, Dictionary<string, string> _xenstore_data, Dictionary<string, string> _sm_config)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_vdi_db_introduce(session.uuid, (_uuid != null) ? _uuid : "", (_name_label != null) ? _name_label : "", (_name_description != null) ? _name_description : "", (_sr != null) ? _sr : "", vdi_type_helper.ToString(_type), _sharable, _read_only, Maps.convert_to_proxy_string_string(_other_config), (_location != null) ? _location : "", Maps.convert_to_proxy_string_string(_xenstore_data), Maps.convert_to_proxy_string_string(_sm_config)).parse());
        }
    }

    public partial class VM
    {
        /// <summary>
        /// Backward compatibility for VM.create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Blob> create_new_blob(Session session, string _vm, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Blob>.Create(session.proxy.vm_create_new_blob(session.uuid, (_vm != null) ? _vm : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }

        /// <summary>
        /// Backward compatibility for VM.async_create_new_blob in XenServer 6.0.
        /// </summary>
        public static XenRef<Task> async_create_new_blob(Session session, string _vm, string _name, string _mime_type)
        {
            if (Helper.APIVersionMeets(session, API_Version.API_1_10))
                System.Diagnostics.Debug.Assert(false, "Cannot use this call on XenServer 6.1 or newer.");

            return XenRef<Task>.Create(session.proxy.async_vm_create_new_blob(session.uuid, (_vm != null) ? _vm : "", (_name != null) ? _name : "", (_mime_type != null) ? _mime_type : "").parse());
        }
    }
}
