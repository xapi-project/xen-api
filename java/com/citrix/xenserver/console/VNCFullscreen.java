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

import javax.swing.JPanel;
import javax.swing.JFrame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.logging.Logger;
import java.util.logging.Level;

public class VNCFullscreen extends JFrame implements KeyListener {
    private static final Logger logger = Logger.getLogger(VNCFullscreen.class.getName());

    static final long serialVersionUID = 0;

    JPanel _panel;
    VNCCanvas _canvas;
    JPanel _buttons;

    public VNCFullscreen(JPanel console, VNCCanvas canvas, boolean fullscreen,
            java.awt.Color c) {
        this.setBackground(c);
        _panel = console;
        _canvas = canvas;
        if (fullscreen) {
            this.setUndecorated(true);
            _canvas.isFullscreen = true;
            _canvas.screen = this;
            java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment()
                    .getDefaultScreenDevice().setFullScreenWindow(this);
            this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            this.addKeyListener(this);
        } else {
            this.setSize(800, 600);
        }
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        setVisible(true);
        this.add(_panel);
        _canvas.requestFocusInWindow();
    }

    public void dispose() {
        setVisible(false);
        if (_panel != null) {
            remove(_panel);
        }
        if (_buttons != null) {
            remove(_buttons);
        }
        super.dispose();
    }

    public void focus() {
        if (getState() == JFrame.ICONIFIED) {
            setState(JFrame.NORMAL);
        }
        this.requestFocus();
        _canvas.requestFocusInWindow();
    }

    public void NoMessinDispose() {
        super.dispose();
    }

    public void keyPressed(KeyEvent e) {
        if (e.isControlDown() && e.getKeyCode() == KeyEvent.VK_ALT) {
            dispose();
        } else {
            _canvas.keyPressed(e);
        }
    }

    public void keyReleased(KeyEvent e) {
        _canvas.keyReleased(e);

    }

    public void keyTyped(KeyEvent e) {
        _canvas.keyTyped(e);

    }
}
