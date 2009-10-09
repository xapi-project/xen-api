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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.logging.Logger;
import java.util.logging.Level;

//import javax.swing.ImageIcon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.RepaintManager;

public class VNCControls extends JPanel {
    private static final Logger logger = Logger.getLogger(VNCControls.class.getName());

    static final long serialVersionUID = 0;

    public JPanel consolePanel = new JPanel(true);
    public JPanel buttonPanel = new JPanel(true);
    final private JPanel undockedPanel = new JPanel(true);

    public VNCFullscreen undockedConsole;

    final private JButton fullscreenButton = new JButton();
    final private JButton dockButton = new JButton();
    final private JButton findconsoleButton = new JButton();
    final private JButton redockButton = new JButton();
    final private JButton ctrlaltdelButton = new JButton();

    public Main main;
    public JPanel backPanel;
    public JPanel controls;

    private Color _backColor;

    public VNCControls(Main main_, JPanel applet_, Color c, boolean showCADButton) {
        main = main_;
        backPanel = applet_;
        controls = this;
        _backColor = c;
        setupConsole();
        setupButtons(showCADButton);
        initialize();
        setColors();
    }

    private void setColors() {
        backPanel.setBackground(_backColor);
        controls.setBackground(_backColor);
        consolePanel.setBackground(_backColor);
        buttonPanel.setBackground(_backColor);
        undockedPanel.setBackground(_backColor);
        fullscreenButton.setBackground(_backColor);
        dockButton.setBackground(_backColor);
        findconsoleButton.setBackground(_backColor);
        redockButton.setBackground(_backColor);
        ctrlaltdelButton.setBackground(_backColor);
    }

    private void initialize() {
        RepaintManager.currentManager(main.canvas_).setDoubleBufferingEnabled(true);
        main.canvas_.setDoubleBuffered(true);
        
        // backPanel.add(undockedPanel, BorderLayout.NORTH);

        BorderLayout layout = new BorderLayout();
        this.setLayout(layout);
 
        this.add(buttonPanel, BorderLayout.NORTH);
        this.add(consolePanel, BorderLayout.CENTER);

        setColors();
        consolePanel.addComponentListener(new ComponentListener() {

            public void componentHidden(ComponentEvent e) {
            }

            public void componentMoved(ComponentEvent e) {
            }

            public void componentResized(ComponentEvent e) {
                // main.canvas_.setLocation((getWidth() -
                // main.canvas_.getWidth())/2, (getHeight() -
                // main.canvas_.getHeight())/2);
                if (main != null) {
                    main.canvas_.setMaxHeight(e.getComponent().getHeight());
                    main.canvas_.setMaxWidth(e.getComponent().getWidth());
                }
                if (consolePanel != null) {
                    consolePanel.invalidate();
                    consolePanel.validate();
                }
            }

            public void componentShown(ComponentEvent e) {
            }
        });
    }

    private void setupButtons(boolean showCADButton) {
        fullscreenButton.setText("Fullscreen (Ctrl+Alt)");
        dockButton.setText("Undock");
        findconsoleButton.setText("Find Console");
        redockButton.setText("Redock Console");
        ctrlaltdelButton.setText("Send Ctrl-Alt-Del");

        fullscreenButton.setVisible(true);
        dockButton.setVisible(true);
        redockButton.setVisible(true);
        findconsoleButton.setVisible(true);
        ctrlaltdelButton.setVisible(showCADButton);

        FlowLayout layout = new FlowLayout();
        layout.setHgap(0);
        layout.setAlignment(FlowLayout.LEFT);
        buttonPanel.setLayout(layout);       
        buttonPanel.add(ctrlaltdelButton);
        //buttonPanel.add(dockButton);
        //buttonPanel.add(fullscreenButton);
        
        layout = new FlowLayout();
        layout.setAlignment(FlowLayout.LEFT);
        undockedPanel.setLayout(layout);
        undockedPanel.add(findconsoleButton);
        undockedPanel.add(redockButton);
        undockedPanel.setVisible(false);

        // on fullscreen press remove panels from frame/applet, show the
        // fullscreen window and add the controls to this

        dockButton.addActionListener(dockListener());
        fullscreenButton.addActionListener(fullscreenListener());

        findconsoleButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                undockedConsole.focus();

            }
        });

        redockButton.addActionListener(redockListener());

        ctrlaltdelButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                main.canvas_.sendCtrlAltDel();
                main.canvas_.requestFocusInWindow();
            }
        });
    }

    private ActionListener redockListener() {
        return new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                undockedConsole.dispose();

            }
        };
    }

    public void setupConsole() {
        BorderLayout layout = new BorderLayout();
        consolePanel.setLayout(layout);
        consolePanel.add(main.canvas_, BorderLayout.NORTH);
    }

    private ActionListener fullscreenListener() {
        return new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                controls.remove(consolePanel);
                VNCFullscreen fc = new VNCFullscreen(consolePanel,
                        main.canvas_, true, _backColor);
                fc.addWindowListener(new WindowListener() {
                    public void windowActivated(WindowEvent e) {
                    };

                    public void windowClosed(WindowEvent e) {
                        controls.add(consolePanel);
                        main.canvas_.setMaxHeight(consolePanel.getHeight());
                        main.canvas_.setMaxWidth(consolePanel.getWidth());
                        controls.invalidate();
                        controls.validate();
                        main.canvas_.invalidate();
                        main.canvas_.validate();
                        main.canvas_.requestFocusInWindow();
                    };

                    public void windowClosing(WindowEvent e) {
                    };

                    public void windowDeactivated(WindowEvent e) {
                        ((VNCFullscreen) e.getComponent()).dispose();
                    };

                    public void windowDeiconified(WindowEvent e) {
                    };

                    public void windowIconified(WindowEvent e) {
                    };

                    public void windowOpened(WindowEvent e) {
                    };
                });
                backPanel.invalidate();
                backPanel.repaint();
            }
        };
    }

    private ActionListener dockListener() {
        return new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                // theApplet.setVisible(false);
                backPanel.remove(controls);
                undockedPanel.setVisible(true);
                undockedConsole = null;
                undockedConsole = new VNCFullscreen(controls, main.canvas_,
                        false, _backColor);
                undockedConsole.setTitle("Console");
                dockButton.setText("Redock");
                try {
                    dockButton.setIcon(new ImageIcon(getClass().getResource(
                            "attach_24.png")));
                } catch (Exception ex) {
                    System.out.println(ex.getMessage());
                }
                undockedConsole.addWindowListener(new WindowListener() {
                    public void windowActivated(WindowEvent e) {
                    };

                    public void windowClosed(WindowEvent e) {
                        undockedPanel.setVisible(false);
                        backPanel.add(controls);
                        dockButton.setText("Undock");
                        try {
                            dockButton.setIcon(new ImageIcon(getClass()
                                    .getResource("detach_24.png")));
                        } catch (Exception ex2) {
                            System.out.println(ex2.getMessage());
                        }
                        backPanel.invalidate();
                        backPanel.validate();
                        if (main != null) {
                            main.canvas_.setMaxHeight(consolePanel.getHeight());
                            main.canvas_.setMaxWidth(consolePanel.getWidth());
                            main.canvas_.invalidate();
                            main.canvas_.repaint();
                            main.canvas_.requestFocusInWindow();
                        }
                        dockButton.removeActionListener(dockButton
                                .getActionListeners()[0]);
                        dockButton.addActionListener(dockListener());
                    };

                    public void windowClosing(WindowEvent e) {
                    };

                    public void windowDeactivated(WindowEvent e) {
                    };

                    public void windowDeiconified(WindowEvent e) {
                    };

                    public void windowIconified(WindowEvent e) {
                    };

                    public void windowOpened(WindowEvent e) {
                    };

                });
                backPanel.invalidate();
                backPanel.repaint();
                dockButton
                        .removeActionListener(dockButton.getActionListeners()[0]);
                dockButton.addActionListener(redockListener());
            }
        };
    }
}
