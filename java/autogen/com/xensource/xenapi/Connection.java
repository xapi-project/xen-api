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

package com.xensource.xenapi;

import java.net.URL;
import java.util.Map;
import java.util.TimeZone;

import org.apache.xmlrpc.XmlRpcException;
import org.apache.xmlrpc.client.XmlRpcClient;
import org.apache.xmlrpc.client.XmlRpcClientConfigImpl;
import org.apache.xmlrpc.client.XmlRpcHttpClientConfig;

import com.xensource.xenapi.Types.BadServerResponse;
import com.xensource.xenapi.Types.XenAPIException;

/**
 * Represents a connection to a XenServer. Creating a new instance of this class initialises a new XmlRpcClient that is
 * then used by all method calls: each method call in xenapi takes a Connection as a parameter, composes an XMLRPC
 * method call, and dispatches it on the Connection's client via the dispatch method.
 */
public class Connection
{
    /**
     * The version of the bindings that this class belongs to.
     */
    public static final String BINDINGS_VERSION = "@SDK_VERSION@";

    private APIVersion apiVersion;

    /**
     * Default reply timeout for xml-rpc calls in seconds
     */
    protected static final int DEFAULT_REPLY_TIMEOUT = 600;

    /**
     * Default connection timeout for xml-rpc calls in seconds
     */
    protected static final int DEFAULT_CONNECTION_TIMEOUT = 5;

    /**
     * Reply timeout for xml-rpc calls. The default value is 10 minutes.
     *
     * @deprecated This field is not used any more. To set the reply timeout
     * for xml-rpc calls, please use the appropriate Connection constructor.
     */
    @Deprecated
    protected int _replyWait = 600;

    /**
     * Connection timeout for xml-rpc calls. The default value is 5 seconds.
     *
     * @deprecated This field is not used any more. To set the connection timeout
     * for xml-rpc calls, please use the appropriate Connection constructor.
     */
    @Deprecated
    protected int _connWait = 5;

    /**
     * Updated when Session.login_with_password() is called.
     */
    public APIVersion getAPIVersion()
    {
        return apiVersion;
    }

    /**
     * The opaque reference to the session used by this connection
     */
    private String sessionReference;

    /**
     * As seen by the xmlrpc library. From our point of view it's a server.
     */
    private final XmlRpcClient client;

    /**
     * Creates a connection to a particular server using a given url. This object can then be passed
     * in to any other API calls.
     *
     * Note this constructor does NOT call Session.loginWithPassword; the programmer is responsible for calling it,
     * passing the Connection as a parameter. No attempt to connect to the server is made until login is called.
     *
     * When this constructor is used, a call to dispose() will do nothing. The programmer is responsible for manually
     * logging out the Session.
     *
     * This constructor uses the default values of the reply and connection timeouts for the xmlrpc calls
     * (600 seconds and 5 seconds respectively).
     *
     * @param url The URL of the server to connect to
     */
    public Connection(URL url)
    {
        this.client = getClientFromURL(url, DEFAULT_REPLY_TIMEOUT, DEFAULT_CONNECTION_TIMEOUT);
    }

    /**
     * Creates a connection to a particular server using a given url. This object can then be passed
     * in to any other API calls.
     *
     * Note this constructor does NOT call Session.loginWithPassword; the programmer is responsible for calling it,
     * passing the Connection as a parameter. No attempt to connect to the server is made until login is called.
     *
     * When this constructor is used, a call to dispose() will do nothing. The programmer is responsible for manually
     * logging out the Session.
     *
     * @param url The URL of the server to connect to
     * @param replyTimeout The reply timeout for xml-rpc calls in seconds
     * @param connTimeout The connection timeout for xml-rpc calls in seconds
     */
    public Connection(URL url, int replyTimeout, int connTimeout)
    {
        this.client = getClientFromURL(url, replyTimeout, connTimeout);
    }


    /**
     * Creates a connection to a particular server using a given url. This object can then be passed
     * in to any other API calls.
     *
     * This constructor uses the default values of the reply and connection timeouts for the xmlrpc calls
     * (600 seconds and 5 seconds respectively).
     *
     * @param url The URL of the server to connect to
     * @param sessionReference A reference to a logged-in Session. Any method calls on this
     * Connection will use it. This constructor does not call Session.loginWithPassword, and dispose() on the resulting
     * Connection object does not call Session.logout. The programmer is responsible for ensuring the Session is logged
     * in and out correctly.
     */
    public Connection(URL url, String sessionReference)
    {
        this.client = getClientFromURL(url, DEFAULT_REPLY_TIMEOUT, DEFAULT_CONNECTION_TIMEOUT);
        this.sessionReference = sessionReference;
    }

    /**
     * Creates a connection to a particular server using a given url. This object can then be passed
     * in to any other API calls.
     *
     * @param url The URL of the server to connect to
     * @param sessionReference A reference to a logged-in Session. Any method calls on this Connection will use it.
     *                         This constructor does not call Session.loginWithPassword, and dispose() on the resulting
     *                         Connection object does not call Session.logout. The programmer is responsible for
     *                         ensuring the Session is logged in and out correctly.
     * @param replyTimeout The reply timeout for xml-rpc calls in seconds
     * @param connTimeout The connection timeout for xml-rpc calls in seconds
     */
    public Connection(URL url, String sessionReference, int replyTimeout, int connTimeout)
    {
        this.client = getClientFromURL(url, replyTimeout, connTimeout);
        this.sessionReference = sessionReference;
    }

    private XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();

    public XmlRpcClientConfigImpl getConfig()
    {
        return config;
    }

    private XmlRpcClient getClientFromURL(URL url, int replyWait, int connWait)
    {
        config.setTimeZone(TimeZone.getTimeZone("UTC"));
        config.setServerURL(url);
        config.setReplyTimeout(replyWait * 1000);
        config.setConnectionTimeout(connWait * 1000);
        XmlRpcClient client = new XmlRpcClient();
        client.setConfig(config);
        return client;
    }

    /*
     * Because the binding calls are constructing their own parameter lists, they need to be able to get to
     * the session reference directly. This is all rather ugly and needs redone
     * Changed to public to allow easier integration with HTTP-level streaming interface,
     * see CA-15447
     */
    public String getSessionReference()
    {
        return this.sessionReference;
    }

    /**
     * The (auto-generated parts of) the bindings dispatch XMLRPC calls on this Connection's client through this method.
     */
    protected Map dispatch(String method_call, Object[] method_params) throws XmlRpcException, XenAPIException
    {
        Map response = (Map) client.execute(method_call, method_params);

        if (method_call.equals("session.login_with_password") &&
            response.get("Status").equals("Success"))
        {
            Session session = Types.toSession(response.get("Value"));
            sessionReference = session.ref;
            setAPIVersion(session);
        }
        else if (method_call.equals("session.slave_local_login_with_password") &&
                 response.get("Status").equals("Success"))
        {
            sessionReference = Types.toSession(response.get("Value")).ref;
            apiVersion = APIVersion.latest();
        }
        else if (method_call.equals("session.logout"))
        {
            // Work around a bug in XenServer 5.0 and below.
            // session.login_with_password should have rejected us with
            // HOST_IS_SLAVE, but instead we don't find out until later.
            // We don't want to leak the session, so we need to log out
            // this session from the master instead.
            if (response.get("Status").equals("Failure"))
            {
                Object[] error = (Object[]) response.get("ErrorDescription");
                if (error.length == 2 && error[0].equals("HOST_IS_SLAVE"))
                {
                    try
                    {
                        XmlRpcHttpClientConfig clientConfig = (XmlRpcHttpClientConfig)client.getClientConfig();
                        URL client_url = clientConfig.getServerURL();
                        URL masterUrl = new URL(client_url.getProtocol(), (String)error[1], client_url.getPort(), client_url.getFile());

                        Connection tmp_conn = new Connection(masterUrl, sessionReference, clientConfig.getReplyTimeout(), clientConfig.getConnectionTimeout());

                        Session.logout(tmp_conn);
                    }
                    catch (Exception ex)
                    {
                        // Ignore
                    }
                }
            }

            this.sessionReference = null;
        }

        return Types.checkResponse(response);
    }


    private void setAPIVersion(Session session) throws XenAPIException, XmlRpcException
    {
        try
        {
            long major = session.getThisHost(this).getAPIVersionMajor(this);
            long minor = session.getThisHost(this).getAPIVersionMinor(this);
            apiVersion = APIVersion.fromMajorMinor(major, minor);
        }
        catch (BadServerResponse exn)
        {
            apiVersion = APIVersion.UNKNOWN;
        }
    }
}
