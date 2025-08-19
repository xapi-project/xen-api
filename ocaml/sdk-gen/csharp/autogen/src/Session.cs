/*
 * Copyright (c) Cloud Software Group, Inc.
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
using System.Collections.Generic;
using System.Linq;
using System.Net;
#if (NET8_0_OR_GREATER)
using System.Net.Http;
using System.Security.Cryptography.X509Certificates;
#endif
using System.Net.Security;
using Newtonsoft.Json;


namespace XenAPI
{
    public partial class Session : XenObject<Session>
    {
        public const int STANDARD_TIMEOUT = 24 * 60 * 60 * 1000;

        /// <summary>
        /// This string is used as the HTTP UserAgent for each request.
        /// </summary>
        public static string UserAgent = $"XenAPI/{Helper.APIVersionString(API_Version.LATEST)}";

        /// <summary>
        /// If null, no proxy is used, otherwise this proxy is used for each request.
        /// </summary>
        public static IWebProxy Proxy = null;

        public API_Version APIVersion = API_Version.UNKNOWN;

        public object Tag;

        #region Constructors

        public Session(JsonRpcClient client)
        {
            client.Timeout = STANDARD_TIMEOUT;
            client.KeepAlive = true;
            client.UserAgent = UserAgent;
            client.WebProxy = Proxy;
            client.AllowAutoRedirect = true;
            JsonRpcClient = client;
        }

        public Session(string url) :
            this(new JsonRpcClient(url))
        {
        }

        public Session(string host, int port)
            : this(GetUrl(host, port))
        {
        }

        public Session(string url, string opaqueRef)
            : this(url)
        {
            opaque_ref = opaqueRef;
            SetupSessionDetails();
        }

        /// <summary>
        /// Create a new Session instance, using the given instance. The connection details
        /// and Xen-API session handle will be copied from the given instance, but a new
        /// connection will be created. Use this if you want a duplicate connection to a host,
        /// for example when you need to cancel an operation that is blocking the primary connection.
        /// </summary>
        public Session(Session session)
        {
            opaque_ref = session.opaque_ref;
            APIVersion = session.APIVersion;

            //in the following do not copy over the ConnectionGroupName

            if (session.JsonRpcClient != null &&
                (APIVersion == API_Version.API_2_6 || APIVersion >= API_Version.API_2_8))
            {
                JsonRpcClient = new JsonRpcClient(session.Url)
                {
                    JsonRpcVersion = session.JsonRpcClient.JsonRpcVersion,
                    UserAgent = session.JsonRpcClient.UserAgent,
                    KeepAlive = session.JsonRpcClient.KeepAlive,
                    WebProxy = session.JsonRpcClient.WebProxy,
                    Timeout = session.JsonRpcClient.Timeout,
                    ProtocolVersion = session.JsonRpcClient.ProtocolVersion,
                    Expect100Continue = session.JsonRpcClient.Expect100Continue,
                    AllowAutoRedirect = session.JsonRpcClient.AllowAutoRedirect,
                    PreAuthenticate = session.JsonRpcClient.PreAuthenticate,
                    Cookies = session.JsonRpcClient.Cookies,
                    ServerCertificateValidationCallback = session.JsonRpcClient.ServerCertificateValidationCallback
                };
            }
            CopyADFromSession(session);
        }

        #endregion

        private static string GetUrl(string hostname, int port)
        {
            return $"{(port == 8080 || port == 80 ? "http" : "https")}://{hostname}:{port}";
        }

        private void SetupSessionDetails()
        {
            SetAPIVersion();
            SetADDetails();
            SetRbacPermissions();
        }

        private void SetAPIVersion()
        {
            Dictionary<XenRef<Pool>, Pool> pools = Pool.get_all_records(this);

            if (pools.Values.Count > 0)
            {
                var pool = pools.Values.First();
                Host host = Host.get_record(this, pool.master);
                APIVersion = Helper.GetAPIVersion(host.API_version_major, host.API_version_minor);
            }

            if (JsonRpcClient != null)
            {
                if (APIVersion == API_Version.API_2_6)
                    JsonRpcClient.JsonRpcVersion = JsonRpcVersion.v1;
                else if (APIVersion >= API_Version.API_2_8)
                    JsonRpcClient.JsonRpcVersion = JsonRpcVersion.v2;
            }
        }

        private void CopyADFromSession(Session session)
        {
            IsLocalSuperuser = session.IsLocalSuperuser;
            SessionSubject = session.SessionSubject;
            UserSid = session.UserSid;
            Roles = session.Roles;
            Permissions = session.Permissions;
        }

        /// <summary>
        /// Applies only to API 1.6 (george) and above.
        /// </summary>
        private void SetADDetails()
        {
            if (APIVersion < API_Version.API_1_6)
            {
                IsLocalSuperuser = true;
                return;
            }

            IsLocalSuperuser = get_is_local_superuser();
            if (IsLocalSuperuser)
                return;

            SessionSubject = get_subject(this, opaque_ref);
            UserSid = get_auth_user_sid();

            // Cache the details of this user to avoid making server calls later
            // For example, some users get access to the pool through a group subject and will not be in the main cache
            UserDetails.UpdateDetails(UserSid, this);
        }

        /// <summary>
        /// Applies only to API 1.7 (midnight-ride) and above.
        /// Older versions have no RBAC, only AD.
        /// </summary>
        private void SetRbacPermissions()
        {
            if (APIVersion < API_Version.API_1_7)
                return;

            // allRoles will contain every role on the server, permissions contains the subset of those that are available to this session.
            Permissions = Session.get_rbac_permissions(this, opaque_ref);
            Dictionary<XenRef<Role>, Role> allRoles = Role.get_all_records(this);
            // every Role object is either a single api call (a permission) or has subroles and contains permissions through its descendants.
            // We take out the parent Roles (VM-Admin etc.) into the Session.Roles field
            foreach (string s in Permissions)
            {
                foreach (XenRef<Role> xr in allRoles.Keys)
                {
                    Role r = allRoles[xr];
                    if (r.subroles.Count > 0 && r.name_label == s)
                    {
                        r.opaque_ref = xr.opaque_ref;
                        Roles.Add(r);
                        break;
                    }
                }
            }
        }

        public override void UpdateFrom(Session update)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        public override string SaveChanges(Session session, string serverOpaqueRef, Session serverObject)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        #region Properties

        /// <summary>
        /// Retrieves the current users details from the UserDetails map. These values are only updated when a new session is created.
        /// </summary>
        public virtual UserDetails CurrentUserDetails => UserSid == null ? null : UserDetails.Sid_To_UserDetails[UserSid];

        public JsonRpcClient JsonRpcClient { get; private set; }

        public string Url => JsonRpcClient.Url;

        public string ConnectionGroupName
        {
            get => JsonRpcClient?.ConnectionGroupName;
            set => JsonRpcClient.ConnectionGroupName = value;
        }

        public int Timeout
        {
            get => JsonRpcClient?.Timeout ?? STANDARD_TIMEOUT;
            set => JsonRpcClient.Timeout = value;
        }

#if (NET8_0_OR_GREATER)
        public Func<HttpRequestMessage, X509Certificate2, X509Chain, SslPolicyErrors, bool> ServerCertificateValidationCallback
        {
            get => JsonRpcClient?.ServerCertificateValidationCallback;
            set => JsonRpcClient.ServerCertificateValidationCallback = value;
        }
#else
        public RemoteCertificateValidationCallback ServerCertificateValidationCallback
        {
            get => JsonRpcClient?.ServerCertificateValidationCallback;
            set => JsonRpcClient.ServerCertificateValidationCallback = value;
        }
#endif

        public ICredentials Credentials => JsonRpcClient?.WebProxy?.Credentials;

        /// <summary>
        /// Optional headers in name-value pairs to be passed in the HttpWebRequests. The
        /// default value is null. This property can be set by the implementing code before
        /// each request. It is automatically reset to null once the request has been sent.
        /// </summary>
        public Dictionary<string, string> RequestHeaders
        {
            set => JsonRpcClient.RequestHeaders = value;
            get => JsonRpcClient.RequestHeaders;
        }

        /// <summary>
        /// Exposes the headers returned in the HttpWebResponses in name-value pairs.
        /// This property is set once a response is received. The values are comma
        /// separated strings of header values stored in a header.
        /// It returns an empty dictionary if no headers are found.
        /// </summary>
        public Dictionary<string, string> ResponseHeaders => JsonRpcClient.ResponseHeaders;

        /// <summary>
        /// Always true before API version 1.6.
        /// Filled in after successful session_login_with_password for 1.6 or newer connections
        /// </summary>
        public virtual bool IsLocalSuperuser { get; private set; }

        /// <summary>
        /// The OpaqueRef for the Subject under whose authority the current user is logged in;
        /// may correspond to either a group or a user.
        /// Null if IsLocalSuperuser is true.
        /// </summary>
        [JsonConverter(typeof(XenRefConverter<Subject>))]
        public XenRef<Subject> SessionSubject { get; private set; }

        /// <summary>
        /// The Active Directory SID of the currently logged-in user.
        /// Null if IsLocalSuperuser is true.
        /// </summary>
        public string UserSid { get; private set; }

        /// <summary>
        /// All permissions associated with the session at the time of log in. This is the list xapi uses until the session is logged out;
        /// even if the permitted roles change on the server side, they don't apply until the next session.
        /// </summary>
        public string[] Permissions { get; private set; }

        /// <summary>
        /// All roles associated with the session at the time of log in. Do not rely on roles for determining what a user can do,
        /// instead use Permissions. This list should only be used for UI purposes.
        /// </summary>
        [JsonConverter(typeof(XenRefListConverter<Role>))]
        public List<Role> Roles { get; private set; }

        #endregion

        public string[] GetSystemMethods()
        {
            return JsonRpcClient.system_list_methods();
        }

        public static Session get_record(Session session, string sessionOpaqueRef)
        {
            Session newSession = new Session(session.Url) { opaque_ref = sessionOpaqueRef };
            newSession.SetAPIVersion();
            return newSession;
        }

        public void login_with_password(string username, string password)
        {
            opaque_ref = JsonRpcClient.session_login_with_password(username, password);
            SetupSessionDetails();
        }

        public void login_with_password(string username, string password, string version)
        {
            try
            {
                opaque_ref = JsonRpcClient.session_login_with_password(username, password, version);

                SetupSessionDetails();
            }
            catch (Failure exn)
            {
                if (exn.ErrorDescription[0] == Failure.MESSAGE_PARAMETER_COUNT_MISMATCH)
                {
                    // Call the 1.1 version instead.
                    login_with_password(username, password);
                }
                else
                {
                    throw;
                }
            }
        }

        public void login_with_password(string username, string password, string version, string originator)
        {
            try
            {
                opaque_ref = JsonRpcClient.session_login_with_password(username, password, version, originator);

                SetupSessionDetails();
            }
            catch (Failure exn)
            {
                if (exn.ErrorDescription[0] == Failure.MESSAGE_PARAMETER_COUNT_MISMATCH)
                {
                    // Call the pre-2.0 version instead.
                    login_with_password(username, password, version);
                }
                else
                {
                    throw;
                }
            }
        }

        public void login_with_password(string username, string password, API_Version version)
        {
            login_with_password(username, password, Helper.APIVersionString(version));
        }

        public void slave_local_login_with_password(string username, string password)
        {
            opaque_ref = JsonRpcClient.session_slave_local_login_with_password(username, password);
            //assume the latest API
            APIVersion = API_Version.LATEST;
        }

        public void logout()
        {
            logout(this);
        }

        /// <summary>
        /// Log out of the given session2, using this session for the connection.
        /// </summary>
        /// <param name="session2">The session to log out</param>
        public void logout(Session session2)
        {
            logout(session2.opaque_ref);
            session2.opaque_ref = null;
        }

        /// <summary>
        /// Log out of the session with the given reference, using this session for the connection.
        /// </summary>
        /// <param name="self">The session to log out</param>
        public void logout(string self)
        {
            if (self == null)
                return;

            JsonRpcClient.session_logout(self);
        }

        public void local_logout()
        {
            local_logout(this);
        }

        public void local_logout(Session session2)
        {
            local_logout(session2.opaque_ref);
            session2.opaque_ref = null;
        }

        public void local_logout(string opaqueRef)
        {
            if (opaqueRef == null)
                return;

            JsonRpcClient.session_local_logout(opaqueRef);
        }

        public void change_password(string oldPassword, string newPassword)
        {
            change_password(this, oldPassword, newPassword);
        }

        /// <summary>
        /// Change the password on the given session2, using this session for the connection.
        /// </summary>
        /// <param name="session2">The session to change</param>
        /// <param name="oldPassword"></param>
        /// <param name="newPassword"></param>
        public void change_password(Session session2, string oldPassword, string newPassword)
        {
            JsonRpcClient.session_change_password(session2.opaque_ref, oldPassword, newPassword);
        }

        public string get_this_host()
        {
            return get_this_host(this, opaque_ref);
        }

        public static string get_this_host(Session session, string self)
        {
            return session.JsonRpcClient.session_get_this_host(session.opaque_ref, self ?? "");
        }

        public string get_this_user()
        {
            return get_this_user(this, opaque_ref);
        }

        public static string get_this_user(Session session, string self)
        {
            return session.JsonRpcClient.session_get_this_user(session.opaque_ref, self ?? "");
        }

        public bool get_is_local_superuser()
        {
            return get_is_local_superuser(this, opaque_ref);
        }

        public static bool get_is_local_superuser(Session session, string self)
        {
            return session.JsonRpcClient.session_get_is_local_superuser(session.opaque_ref, self ?? "");
        }

        public static string[] get_rbac_permissions(Session session, string self)
        {
            return session.JsonRpcClient.session_get_rbac_permissions(session.opaque_ref, self ?? "");
        }

        public DateTime get_last_active()
        {
            return get_last_active(this, opaque_ref);
        }

        public static DateTime get_last_active(Session session, string self)
        {
            return session.JsonRpcClient.session_get_last_active(session.opaque_ref, self ?? "");
        }

        public bool get_pool()
        {
            return get_pool(this, opaque_ref);
        }

        public static bool get_pool(Session session, string self)
        {
            return session.JsonRpcClient.session_get_pool(session.opaque_ref, self ?? "");
        }

        public XenRef<Subject> get_subject()
        {
            return get_subject(this, opaque_ref);
        }

        public static XenRef<Subject> get_subject(Session session, string self)
        {
            return session.JsonRpcClient.session_get_subject(session.opaque_ref, self ?? "");
        }

        public string get_auth_user_sid()
        {
            return get_auth_user_sid(this, opaque_ref);
        }

        public static string get_auth_user_sid(Session session, string self)
        {
            return session.JsonRpcClient.session_get_auth_user_sid(session.opaque_ref, self ?? "");
        }

        #region AD SID enumeration and bootout

        public string[] get_all_subject_identifiers()
        {
            return get_all_subject_identifiers(this);
        }

        public static string[] get_all_subject_identifiers(Session session)
        {
            return session.JsonRpcClient.session_get_all_subject_identifiers(session.opaque_ref);
        }

        public XenRef<Task> async_get_all_subject_identifiers()
        {
            return async_get_all_subject_identifiers(this);
        }

        public static XenRef<Task> async_get_all_subject_identifiers(Session session)
        {
            return session.JsonRpcClient.async_session_get_all_subject_identifiers(session.opaque_ref);
        }

        public string logout_subject_identifier(string subjectIdentifier)
        {
            return logout_subject_identifier(this, subjectIdentifier);
        }

        public static string logout_subject_identifier(Session session, string subjectIdentifier)
        {
            session.JsonRpcClient.session_logout_subject_identifier(session.opaque_ref, subjectIdentifier);
            return string.Empty;
        }

        public XenRef<Task> async_logout_subject_identifier(string subjectIdentifier)
        {
            return async_logout_subject_identifier(this, subjectIdentifier);
        }

        public static XenRef<Task> async_logout_subject_identifier(Session session, string subjectIdentifier)
        {
            return session.JsonRpcClient.async_session_logout_subject_identifier(session.opaque_ref, subjectIdentifier);
        }

        #endregion

        #region other_config stuff

        public Dictionary<string, string> get_other_config()
        {
            return get_other_config(this, opaque_ref);
        }

        public static Dictionary<string, string> get_other_config(Session session, string self)
        {
            return session.JsonRpcClient.session_get_other_config(session.opaque_ref, self ?? "");
        }

        public void set_other_config(Dictionary<string, string> otherConfig)
        {
            set_other_config(this, opaque_ref, otherConfig);
        }

        public static void set_other_config(Session session, string self, Dictionary<string, string> otherConfig)
        {
            session.JsonRpcClient.session_set_other_config(session.opaque_ref, self ?? "", otherConfig);
        }

        public void add_to_other_config(string key, string value)
        {
            add_to_other_config(this, opaque_ref, key, value);
        }

        public static void add_to_other_config(Session session, string self, string key, string value)
        {
            session.JsonRpcClient.session_add_to_other_config(session.opaque_ref, self ?? "", key ?? "", value ?? "");
        }

        public void remove_from_other_config(string key)
        {
            remove_from_other_config(this, opaque_ref, key);
        }

        public static void remove_from_other_config(Session session, string self, string key)
        {
            session.JsonRpcClient.session_remove_from_other_config(session.opaque_ref, self ?? "", key ?? "");
        }

        #endregion
    }
}
