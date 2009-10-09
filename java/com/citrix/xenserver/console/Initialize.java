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

import javax.swing.BorderFactory;
import javax.swing.JApplet;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import java.awt.BorderLayout;
import java.awt.Color;
import java.net.URL;

import java.util.logging.Logger;
import java.util.logging.Level;

public class Initialize extends JApplet implements ConnectionListener,
        ConsoleListener {
    private static final Logger logger = Logger.getLogger(Initialize.class.getName());

    static final long serialVersionUID = 0;
    static public URL path;

    private Main main;
    private VNCControls controls;
    private JPanel background = new JPanel(new BorderLayout(), true);
    private JPanel errorPanel = new JPanel(true);
    private Thread t;
    public JTextArea console = new JTextArea();
    
    private int retries = 5;
    private boolean connecting = false;

    private boolean logOnConsole = false;
    private boolean hideCADButton = false;

    private String[] getArgs() {
        String[] args;
        if ("true".equals(getParameter("USEURL"))) {
            args = new String[3];
            args[0] = getParameter("URL");
            args[1] = getParameter("SESSION");
            args[2] = getParameter("USEURL");
        } else {
            args = new String[4];
            args[0] = getParameter("IPADDRESS");
            args[1] = getParameter("PASSWORD");
            args[2] = getParameter("USEURL");
            args[3] = getParameter("PORT");
        }
        return args;
    }

    public void init() {
        try {           
            logOnConsole = Boolean.valueOf(getParameter("CONSOLELOGGING"));
            hideCADButton = Boolean.valueOf(getParameter("HIDECAD"));
            path = getDocumentBase();
            writeline("");
            writeline("Loading UI...");
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            writeline("Initializing...");
            String[] args = getArgs();
            Color c;
            if (getParameter("BACKCOLOR") != null) {
                String s = getParameter("BACKCOLOR");
                String[] vals = s.split(":");
                if (vals.length == 3 && vals[0].length() == 2
                        && vals[1].length() == 2 && vals[2].length() == 2) {
                    c = new Color(Integer.parseInt(vals[0], 16), Integer
                            .parseInt(vals[1], 16), Integer.parseInt(vals[2],
                            16));
                } else {
                    c = Color.WHITE;
                }
            } else {
                c = Color.WHITE;
            }
            this.setBackground(c);
            writeline("Starting main...");
            main = new Main(args, this, this);
            writeline("Creating controls...");
            controls = new VNCControls(main, background, c, !hideCADButton);
            writeline("Adding controls...");
            background.add(controls);
            this.add(background);

            errorPanel.setBackground(c);
            errorPanel.setLayout(new BorderLayout());
            console.setBackground(Color.white);
            console.setEditable(false);
            JScrollPane areaScrollPane = new JScrollPane(console);
            areaScrollPane
                    .setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            areaScrollPane.setBorder(BorderFactory.createLineBorder(
                    Color.BLACK, 1));
            errorPanel.add(areaScrollPane, BorderLayout.CENTER);
        } catch (Exception e) {
            writeline(e.getMessage());
        }

    }

    public void start() {
        writeline("Starting...");
        main.connect();
    }

    public void stop() {
        writeline("Stopping...");
        if (main != null && main.stream_ != null && main.stream_.isConnected()) {
            main.stream_.disconnect();
        }
    }

    public void destroy() {
        writeline("Destroying...");
    }

    public void ConnectionClosed() {
        writeline("Connection closed");
        if (retries > 0) {

            controls.consolePanel.remove(main.canvas_);
            controls.consolePanel.setLayout(new BorderLayout());
            controls.consolePanel.add(errorPanel);
            controls.invalidate();
            controls.validate();
            controls.consolePanel.invalidate();
            controls.consolePanel.validate();

            t = new Thread(new Runnable() {
                public void run() {

                    writeline("Reconnecting in 5 seconds...");
                    try {
                        Thread.sleep(5000);
                    } catch (Exception e) {
                        writeline(e.getMessage());
                    }
                    writeline("Retry ".concat(Integer.toString(6 - retries))
                            .concat(" out of 5"));
                    main.connect();
                    retries--;
                };
            });
            t.start();
        }
    }

    public void ConnectionLost(String reason) {
        if (main != null) {
            if (reason != null) {
                writeline("Connection lost: ".concat(reason));
            } else {
                writeline("Connection lost");
            }
            if (!connecting) {
                connecting = true;
                ConnectionClosed();
            }
        }
    }

    public void ConnectionMade() {
        controls.consolePanel.remove(errorPanel);
        controls.setupConsole();
        controls.consolePanel.getParent().repaint();
        controls.consolePanel.invalidate();
        controls.consolePanel.validate();
        main.canvas_.requestFocusInWindow();
        connecting = false;
        retries = 5;
    }

    public void ConnectionFailed(String reason) {
        if (main != null) {
            writeline("Connection failed: ".concat(reason));
            // if(!connecting)
            // {
            connecting = true;
            ConnectionClosed();
            // }
        }
    }

    public void writeline(final String line) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                if (logOnConsole && line != null) {
                    console.append(line);
                    console.append("\n");
                }
                System.out.println(line);
            }
        });
    }
}
