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

import java.net.URL;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;

import com.xensource.xenapi.*;

/**
 * Tests using an https (SSL) connection to a XenServer.
 * 
 * Before running, perform these steps:
 * 
 * 1. Copy to your client machine /etc/xensource/xapi-ssl.pem from the XenServer you want to connect to.
 * 2. Run 'openssl x509 -inform PEM -outform DER -in xapi-ssl.pem -out xapi-ssl.jks'
 *    This converts the certificate into a form that Java's keytool can understand.
 * 3. Run keytool (found in Java's bin directory) as follows:
 *    'keytool -importcert -file xapi-ssl.jks -alias <hostname>'
 *    You can optionally pass the -keystore argument if you want to specify the location of your keystore.
 * 4. To tell the JVM the location and password of your keystore, run it with the additional parameters
 *    (Sun's keytool seems to insist on using private key and keystore passwords):
 *    -Djavax.net.ssl.trustStore="<path to keystore>" -Djavax.net.ssl.trustStorePassword=<password>
 *    For extra debug info, try:
 *    -Djavax.net.debug=ssl
 */
public class Https extends TestBase
{
    public String getTestName() {
        return "Https";
    }

    protected void TestCore() throws Exception {

    }

    @Override
    public void RunTest(FileLogger logger, TargetServer server) throws Exception
    {
        this.logger = logger;
        Connection conn = null;

        try
        {
            // Create our own HostnameVerifier
            HostnameVerifier hv = new HostnameVerifier()
            {
                public boolean verify(String hostname, SSLSession session)
                {
                    return session.getPeerHost().equals(hostname);
                }
            };

            // Sets the default HostnameVerifier used on all Https connections created after this point
            HttpsURLConnection.setDefaultHostnameVerifier(hv);

            URL url = new URL("https://" + server.Hostname);
            conn = new Connection(url);

            // Log in
            Session.loginWithPassword(conn, server.Username, server.Password, "1.3");

            // Print a record
            for (Host host : Host.getAllRecords(conn).keySet())
            {
                log(host.toString());
                break;
            }
        }
        finally
        {
            if (conn != null)
                Session.logout(conn);

        }
    }
}
