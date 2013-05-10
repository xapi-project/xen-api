/*
 * Copyright (c) 2008-2011 Citrix Systems, Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
package com.citrix.xenserver.console.examples;

import java.awt.BorderLayout;
import java.awt.Color;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import com.citrix.xenserver.console.ConnectionListener;
import com.citrix.xenserver.console.ConsoleListener;
import com.citrix.xenserver.console.Main;
import com.citrix.xenserver.console.VNCControls;

public class JavaInitialize extends JDialog implements ConnectionListener,
        ConsoleListener {
    private static final Logger logger = Logger.getLogger(JavaInitialize.class.getName());

    static final long serialVersionUID = 0;

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
    
    public JavaInitialize() {
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
	}
    
    public void init(String[] args) {
        try {           
            logOnConsole = false;
            hideCADButton = false;
            writeline("");
            writeline("Loading UI...");
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            writeline("Initializing...");
            this.setBackground(Color.white);
            writeline("Starting main...");
            main = new Main(args, this, this);
            writeline("Creating controls...");
            controls = new VNCControls(main, background, Color.white, !hideCADButton);
            writeline("Adding controls...");
            background.add(controls);
            this.add(background);

            errorPanel.setBackground(Color.white);
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
        	e.printStackTrace();
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
