/*
 * Copyright (c) 2005-2009 Citrix Systems, Inc.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as published
 * by the Free Software Foundation, with the additional linking exception as
 * follows:
 * 
 *   Linking this library statically or dynamically with other modules is
 *   making a combined work based on this library. Thus, the terms and
 *   conditions of the GNU General Public License cover the whole combination.
 * 
 *   As a special exception, the copyright holders of this library give you
 *   permission to link this library with independent modules to produce an
 *   executable, regardless of the license terms of these independent modules,
 *   and to copy and distribute the resulting executable under terms of your
 *   choice, provided that you also meet, for each linked independent module,
 *   the terms and conditions of the license of that module. An independent
 *   module is a module which is not derived from or based on this library. If
 *   you modify this library, you may extend this exception to your version of
 *   the library, but you are not obligated to do so. If you do not wish to do
 *   so, delete this exception statement from your version.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
package com.citrix.xenserver.console;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.*;
import java.security.SecureRandom;
import java.util.*;
import java.security.*;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;
import java.util.logging.Level;

import java.util.regex.*;

import javax.net.SocketFactory;
//import javax.net.ssl.SSLParameters;
import javax.net.ssl.*;
import javax.net.ssl.SSLSocket;

//import javax.net.ssl.SSLSocketFactory;
//import com.sun.net.ssl.SSLContext;

/**
 * Send an HTTP CONNECT or PUT request to a XenAPI host with a Session ID,
 * return the connected socket and the Task ID. Used for tunnelling VNC
 * connections and import/export operations.
 */
public final class RawHTTP {
    private static final Logger logger = Logger.getLogger(RawHTTP.class.getName());

    private static final Pattern END_PATTERN = Pattern.compile("^\r\n$");
    private static final Pattern HEADER_PATTERN = Pattern
            .compile("^([A-Z_a-z0-9-]+):\\s*(.*)\r\n$");
    private static final Pattern HTTP_PATTERN = Pattern
            .compile("^HTTP/\\d+\\.\\d+ (\\d*) (.*)\r\n$");

    private final String command;
    private final String host;
    private final int port;
    private final String path;
    private final String session;
    private final boolean useSSL;

    private final Map<String, String> responseHeaders = new HashMap<String, String>();

    private InputStream ic;
    private OutputStream oc;
    private Socket s;

    public InputStream getInputStream() {
        return ic;
    }

    public OutputStream getOutputStream() {
        return oc;
    }

    public Socket getSocket() {
        return s;
    }

    private ConsoleListener _console;

    public RawHTTP(String command, String host, int port, String path,
            String session, boolean useSSL, ConnectionListener listner,
            ConsoleListener console) {
        this.command = command;
        this.host = host;
        this.port = port;
        this.path = path;
        this.session = session;
        this.useSSL = useSSL;
        _console = console;
    }

    private static final TrustManager[] trustAllCerts = new TrustManager[] { 
        new X509TrustManager() {
            public X509Certificate[] getAcceptedIssuers() {
                return null;
            }

            public void checkClientTrusted(X509Certificate[] certs, String authType) {
            }

            public void checkServerTrusted(X509Certificate[] certs, String authType) {
            }
        } 
    };

    private Socket _getSocket() throws IOException {
        if (useSSL) {
            javax.net.ssl.SSLContext context = SSLHelper.getInstance()
                    .getSSLContext();
            SSLSocket ssl = null;
            try {
                context.init(null, trustAllCerts, new SecureRandom());
                SocketFactory factory = context.getSocketFactory();
                ssl = (SSLSocket) factory.createSocket(host, port);
                /* ssl.setSSLParameters(context.getDefaultSSLParameters()); */
            } catch (IOException e) {
                _console.writeline("IOException: " + e.getMessage());
                throw e;
            } catch (KeyManagementException e) {
                _console.writeline("KeyManagementException: " + e.getMessage());
            }
            return ssl;
        } else {
            return new Socket(host, port);
        }
    }

    public Socket connect() throws IOException {
        String[] headers = makeHeaders();
        s = _getSocket();
        try {
            oc = s.getOutputStream();
            for (String header : headers) {
                oc.write(header.getBytes());
                oc.write("\r\n".getBytes());
            }
            oc.flush();
            ic = s.getInputStream();
            while (true) {
                String line = readline(ic);

                Matcher m = END_PATTERN.matcher(line);
                if (m.matches()) {
                    return s;
                }

                m = HEADER_PATTERN.matcher(line);
                if (m.matches()) {
                    responseHeaders.put(m.group(1), m.group(2));
                    continue;
                }

                m = HTTP_PATTERN.matcher(line);
                if (m.matches()) {
                    String status_code = m.group(1);
                    String reason_phrase = m.group(2);
                    if (!"200".equals(status_code)) {
                        throw new IOException("HTTP status " + status_code
                                + " " + reason_phrase);
                    }
                } else {
                    throw new IOException("Unknown HTTP line " + line);
                }
            }
        } catch (IOException exn) {
            s.close();
            throw exn;
        } catch (RuntimeException exn) {
            s.close();
            throw exn;
        }
    }

    public Map<String, String> getResponseHeaders() {
        return responseHeaders;
    }

    private String[] makeHeaders() {
        String[] headers = { String.format("%s %s HTTP/1.0", command, path),
                String.format("Host: %s", host),
                String.format("Cookie: session_id=%s", session), "" };
        return headers;
    }

    private static String readline(InputStream ic) throws IOException {
        String result = "";
        while (true) {
            try {
                int c = ic.read();

                if (c == -1) {
                    return result;
                }
                result = result + (char) c;
                if (c == 0x0a /* LF */) {
                    return result;
                }
            } catch (IOException e) {
                ic.close();
                throw e;
            }
        }
    }
}
