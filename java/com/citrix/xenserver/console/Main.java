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

import java.net.Socket;
import java.net.URL;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.swing.SwingUtilities;

public class Main {

    public static void main(String[] args) throws Throwable {
        new Main(args, new Initialize(), new Initialize());
    }

    private static final Logger logger = Logger.getLogger(Main.class.getName());

    public final VNCCanvas canvas_ = new VNCCanvas();
    public VNCStream stream_;
    private boolean usessl;
    private String path;
    private String auth;
    private int port;
    private ConnectionListener _listener;
    private ConsoleListener _console;

    public boolean firstTime = true;

    public Main(String[] args, ConnectionListener listener,
            ConsoleListener console) {

        if ("true".equals(args[2])) {
            usessl = true;
        } else {
            usessl = false;
            port = Integer.parseInt(args[3]);
        }
        path = args[0];
        auth = args[1];
        stream_ = new VNCStream(canvas_, listener, console);
        canvas_.setStream(stream_);
        canvas_.setConsoleListener(console);
        _listener = listener;
        _console = console;
    }

    public void connect() {
        try {
            if (usessl) {
                stream_.disconnect();
                URL uri = new URL(path);
                String uuid = auth;
                RawHTTP http = new RawHTTP("CONNECT", uri.getHost(), 443, uri
                        .getPath().concat("?").concat(uri.getQuery()), uuid,
                        "https".equals(uri.getProtocol()), _listener, _console);
                http.connect();
                stream_.connect(http, new char[0]);
            } else {
                stream_.disconnect();
                String password = auth;
                int n = password.length();
                char[] c = new char[n];
                password.getChars(0, n, c, 0);
                stream_.connectSocket(new Socket(path, port), c);
            }
        } catch (final Throwable t) {
            if (_listener != null) {
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        _listener.ConnectionFailed(t.getMessage());
                    }
                });
            }
        }
    }
}
