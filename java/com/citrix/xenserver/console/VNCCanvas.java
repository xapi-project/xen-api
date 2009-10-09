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

import java.lang.reflect.InvocationTargetException;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.JPanel;

public class VNCCanvas extends JComponent implements VNCClient, ClipboardOwner,
        MouseListener, MouseMotionListener, MouseWheelListener, KeyListener,
        FocusListener {
    private static final Logger logger = Logger.getLogger(VNCCanvas.class.getName());

    /*
     * There are two threads coming through here -- the GUI event thread and the
     * VNC stream. VNC has a tendency to send lots of small rectangles,
     * especially at startup, so the intention is to gather these up on the
     * stream thread before punting across to the GUI thread for redraw.
     * Switching threads on each rectangle causes the GUI thread to stall
     * processing lots of small updates, blocking other activity in the GUI.
     * 
     * Accumulation is done on the stream thread, writing to imageGraphics_ and
     * damageStreamside. When we receive clientFrameBufferUpdate, which is
     * called whenever a batch of VNC rectangles has been processed,
     * damageStreamside is copied into damageEventside, and the redraw is
     * performed.
     * 
     */

    static final long serialVersionUID = 0;

    private static final KeyMap keyMap = KeyMap.getInstance();

    public boolean isFullscreen = false;
    public VNCFullscreen screen;

    /**
     * The width of the border around the panel. This is 1 for the highlight
     * line, and two for padding.
     */
    private static final int BW = 3;

    // private static final Border focusBorder =
    // BorderFactory.createCompoundBorder(
    // UIManager.getBorder("Table.focusCellHighlightBorder"),
    // BorderFactory.createLineBorder(
    // LookAndFeelManager.PANEL_BACKGROUND_COLOR, 2));
    // private static final Border noFocusBorder =
    // BorderFactory.createLineBorder(
    // LookAndFeelManager.PANEL_BACKGROUND_COLOR, BW);

    private final Toolkit toolkit;

    /**
     * stream_ may only be changed before the VNCStream is connected.
     */
    private VNCStream stream_;

    /**
     * Volatile, and may be accessed by either the stream or the event thread.
     * The reference should be copied into the current thread before being
     * accessed. This will only change when the desktop resizes, in which case
     * imageGraphics_ will have already changed.
     */
    private BufferedImage image_ = null;

    /**
     * May only be accessed by the stream thread.
     */
    private Graphics2D imageGraphics_ = null;

    /**
     * May only be updated by the stream thread. Damage details will be
     * accumulated here, and then copied by the event thread into
     * damageEventside (with the stream thread blocked).
     */
    private final Rectangle damageStreamside = new Rectangle();

    private Cursor _cursor;

    /**
     * May only be accessed by the event thread. Damage details will be copied
     * here from damageStreamside, when clientFrameBufferUpdate is called.
     */
    private final Rectangle damageEventside = new Rectangle();

    /**
     * May only be accessed from the event thread.
     */
    // private BufferedImage thumbnail = null;
    /**
     * This reference may be read from any thread, but the instance itself only
     * be modified from the event thread.
     */
    private final ImageIcon thumbnailIcon = new ImageIcon();

    /**
     * May only be accessed from the event thread.
     */
    // private Graphics thumbnailGraphics = null;
    /**
     * May only be accessed from the event thread.
     */
    // private boolean updateThumbnail = false;
    /**
     * May only be accessed from the event thread.
     */
    // private double thumbnailRatio;
    /**
     * May only be accessed from the event thread.
     */
    private String serverText_ = "";

    /**
     * May only be accessed from the event thread.
     */
    // private ConsolePanel panel = null;

    static {
        // Force early class loading. This pulls in bits of sun.java2d.
        // BufferedImage i1 =
        // new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB);
        // BufferedImage i2 =
        // new BufferedImage(18, 18, BufferedImage.TYPE_INT_RGB);
        // Graphics g1 = i1.getGraphics();
        // Graphics g2 = i2.getGraphics();
        // g1.setColor(Color.BLACK);
        // g1.fillRect(0, 0, 10, 10);
        // g2.drawImage(i1, 1, 1, 18, 18, 0, 0, 20, 20, null);
        // g2.copyArea(10, 10, 20, 20, 0, 0);
    }

    /**
     * Constructor.
     */
    public VNCCanvas() {

        toolkit = Toolkit.getDefaultToolkit();

        // setCursor(LookAndFeelManager.emptyCursor);

        // setBorder(noFocusBorder);

        setDoubleBuffered(true);
        setOpaque(false);

        BufferedImage blankThumbnail = new BufferedImage(200, 150,
                BufferedImage.TYPE_INT_RGB);
        Graphics g = blankThumbnail.getGraphics();
        g.fillRect(0, 0, 200, 150);
        thumbnailIcon.setImage(blankThumbnail);

        setFocusTraversalKeysEnabled(false);
        addFocusListener(this);
        addMouseListener(this);
        addMouseMotionListener(this);
        addMouseWheelListener(this);
        addKeyListener(this);
    }

    public Dimension getPreferredSize() {
        if (image_ != null) {
            return new Dimension(Translate(_streamWidth),
                    Translate(_streamHeight));
        }
        return super.getPreferredSize();
    }

    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    public Dimension getMaximumSize() {
        return getPreferredSize();
    }

    public void setStream(VNCStream stream) {
        stream_ = stream;
    }

    private double scaleFactor = 1;

    private void setScaleFactor(double sf) {
        scaleFactor = sf;
    }

    private int Translate(int i) {
        return (int) ((i * scaleFactor));
    }

    private int TranslateRev(int i) {
        return (int) (i / (scaleFactor - (double) BW / (double) _streamHeight));
    }

    public void setUpdateThumbnail(boolean updateThumbnail) {
        assert SwingUtilities.isEventDispatchThread();
        // this.updateThumbnail = updateThumbnail;
        if (updateThumbnail) {
            repairThumbnail();
        }
    }

    private ConsoleListener _console;

    public void setConsoleListener(ConsoleListener console) {
        _console = console;
    }

    public void paintComponent(Graphics graphics) {

        if (image_ != null && ui == null) {
            Graphics2D g2 = (Graphics2D) graphics;
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                    RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING,
                    RenderingHints.VALUE_RENDER_SPEED);
            g2.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION,
                    RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                    RenderingHints.VALUE_ANTIALIAS_OFF);
            g2.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING,
                    RenderingHints.VALUE_COLOR_RENDER_SPEED);
            g2.setRenderingHint(RenderingHints.KEY_DITHERING,
                    RenderingHints.VALUE_DITHER_DISABLE);
            if (!isFullscreen) {
                g2.drawImage(image_, BW, BW, Translate(_streamWidth) - 2 * BW,
                        Translate(_streamHeight) - 2 * BW, null);
            } else {
                g2.drawImage(image_, 0, 0, Translate(_streamWidth),
                        Translate(_streamHeight), null);
            }
        }
    }

    public void paintBorder(Graphics graphics) {
        this.setSize(Translate(_streamWidth), Translate(_streamHeight));
        super.paintBorder(graphics);
    }

    public void paintComponents(Graphics graphics) {

    }

    private int _windowHeight;
    private int _windowWidth;
    private int _streamHeight;
    private int _streamWidth;

    public int getConsoleLeft() {
        return (_windowWidth - Translate(_streamWidth)) / 2;
    }

    public int getConsoleTop() {
        return (_windowHeight - Translate(_streamHeight)) / 2;
    }

    public void setMaxHeight(int h) {
        _windowHeight = h;
        if (_streamHeight > 1 && _streamWidth > 1) {
            double sf1 = (double) (_windowHeight) / (double) (_streamHeight);
            double sf2 = (double) (_windowWidth) / (double) (_streamWidth);
            if (sf1 < sf2) {
                setScaleFactor(sf1);
            } else {
                setScaleFactor(sf2);
            }
        } else {
            setScaleFactor(1.0);
        }
    }

    public void setMaxWidth(int w) {
        _windowWidth = w;
        if (_streamWidth > 1 && _streamHeight > 1) {
            double sf1 = (double) (_windowHeight) / (double) (_streamHeight);
            double sf2 = (double) (_windowWidth) / (double) (_streamWidth);
            if (sf1 < sf2) {
                setScaleFactor(sf1);
            } else {
                setScaleFactor(sf2);
            }
        } else {
            setScaleFactor(1.0);
        }

    }

    public void setVisible(boolean visible) {
        super.setVisible(visible);
        if (visible) {
            stream_.unpause();
        } else {
            stream_.pause();
        }
    }

    private void pointerEvent(MouseEvent event) {
        if (this.hasFocus()) {
            int buttonMask = 0;
            if ((event.getModifiersEx() & MouseEvent.BUTTON1_DOWN_MASK) != 0) {
                buttonMask |= 1;
            }
            if ((event.getModifiersEx() & MouseEvent.BUTTON2_DOWN_MASK) != 0) {
                buttonMask |= 2;
            }
            if ((event.getModifiersEx() & MouseEvent.BUTTON3_DOWN_MASK) != 0) {
                buttonMask |= 4;
            }
            stream_.pointerEvent(buttonMask, TranslateRev(event.getX()),
                    TranslateRev(event.getY()));
            // event.consume();
        }
    }

    public void mouseReleased(MouseEvent event) {
        pointerEvent(event);
        if (this.hasFocus()) {
            if (_cursor != null) {
                setCursor(_cursor);
            } else {
                int[] pixels = new int[16 * 16];
                Image image = Toolkit.getDefaultToolkit().createImage(
                        new java.awt.image.MemoryImageSource(16, 16, pixels, 0,
                                16));
                setCursor(Toolkit.getDefaultToolkit().createCustomCursor(image,
                        new Point(0, 0), "invisibleCursor"));
            }
        }
    }

    public void mousePressed(MouseEvent event) {

        if (_cursor != null) {
            setCursor(_cursor);
        } else {
            int[] pixels = new int[16 * 16];
            Image image = Toolkit.getDefaultToolkit()
                    .createImage(
                            new java.awt.image.MemoryImageSource(16, 16,
                                    pixels, 0, 16));
            setCursor(Toolkit.getDefaultToolkit().createCustomCursor(image,
                    new Point(0, 0), "invisibleCursor"));
        }

        if (!this.hasFocus()) {
            this.requestFocusInWindow();
            setCursor(Cursor.getDefaultCursor());
        }
        pointerEvent(event);
    }

    public void mouseExited(MouseEvent event) {
        setCursor(Cursor.getDefaultCursor());

    }

    public void mouseEntered(MouseEvent event) {
        if (this.hasFocus()) {
            if (_cursor != null) {
                setCursor(_cursor);
            } else {
                int[] pixels = new int[16 * 16];
                Image image = Toolkit.getDefaultToolkit().createImage(
                        new java.awt.image.MemoryImageSource(16, 16, pixels, 0,
                                16));
                setCursor(Toolkit.getDefaultToolkit().createCustomCursor(image,
                        new Point(0, 0), "invisibleCursor"));
            }
        } else {
            setCursor(Cursor.getDefaultCursor());
        }
        pointerEvent(event);
    }

    public void mouseClicked(MouseEvent event) {

        if (!this.hasFocus()) {
            this.requestFocusInWindow();
        }
    }

    public void mouseMoved(MouseEvent event) {
        pointerEvent(event);
    }

    public void mouseDragged(MouseEvent event) {
        pointerEvent(event);
    }

    /**
     * @see MouseWheelListener#mouseWheelMoved
     */
    public void mouseWheelMoved(MouseWheelEvent event) {
        if (this.hasFocus()) {
            int x = event.getX();
            int y = event.getY();
            int r = event.getWheelRotation();

            stream_.pointerWheelEvent(TranslateRev(x), TranslateRev(y), r);
            event.consume();
        }
    }

    public void keyTyped(KeyEvent event) {
        event.consume();
    }

    public void sendCtrlAltDel() {
        stream_.sendCtrlAltDelete();
    }

    private void key(KeyEvent event, boolean pressed) {
        if (this.hasFocus()) {
            if (event.getKeyCode() == KeyEvent.VK_ALT && event.isControlDown()
                    && isFullscreen) {
                // Special case for Ctrl-Alt-Ins to be mapped to
                // Ctrl-Alt-Delete.
                // stream_.keyEvent(pressed,
                // keyMap.getMappedKey(KeyEvent.VK_DELETE));

                // Exitfullscreen
                screen.dispose();
                isFullscreen = false;
                // screen = null;
            } else if (event.getKeyCode() == KeyEvent.VK_ALT
                    && event.isControlDown()) {
                /* For now disabling Ctrl-Alt fullscreen support
                final Container root = getParent().getParent();
                root.remove(getParent());
                VNCFullscreen fc = new VNCFullscreen((JPanel) getParent(),
                        this, true, getParent().getBackground());
                fc.addWindowListener(new WindowListener() {
                    public void windowActivated(WindowEvent e) {
                    };

                    public void windowClosed(WindowEvent e) {
                        root.add(getParent());
                        setMaxHeight(getParent().getHeight());
                        setMaxWidth(getParent().getWidth());
                        root.invalidate();
                        root.validate();
                        invalidate();
                        repaint();
                        requestFocusInWindow();
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
                requestFocusInWindow(); */
            } else {
                int keysym = keyMap.getKeysym(event);
                if (keysym != -1) {
                    stream_.keyEvent(pressed, keysym);
                }
            }
            event.consume();
        }
    }

    public void keyReleased(KeyEvent event) {
        key(event, false);
    }

    public void clientBell() {
        // TODO Auto-generated method stub

    }

    public void keyPressed(KeyEvent event) {
        key(event, true);
    }

    public JFrame Frame;
    public JScrollPane ScrollPane;

    /**
     * @see VNCClient#clientDesktopSize
     */
    public void clientDesktopSize(final int width, final int height) {
        _console.writeline("Desktop size is now " + width + "; " + height);
        _streamHeight = height;
        _streamWidth = width;

        if (_windowHeight != 0 && _windowWidth != 0) {
            setMaxHeight(_windowHeight);
            setMaxWidth(_windowWidth);
        }

        final BufferedImage image2 = new BufferedImage(width, height,
                BufferedImage.TYPE_INT_RGB);
        imageGraphics_ = (Graphics2D) image2.getGraphics();

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                image_ = image2;
                java.awt.Container parent = getParent();
                if (parent != null) {
                    parent.invalidate();
                    parent.validate();
                }
            }
        });

    }

    /**
     * Expects to be on the stream thread.
     */
    private void damage(int x, int y, int width, int height) {
        x += BW;
        y += BW;

        if (damageStreamside.isEmpty()) {
            damageStreamside.x = Translate(x);
            damageStreamside.y = Translate(y);
            damageStreamside.width = Translate(width);
            damageStreamside.height = Translate(height);
        } else {
            damageStreamside.add(new Rectangle(Translate(x), Translate(y),
                    Translate(width), Translate(height)));
        }
    }

    /**
     * Expects to be on the event thread.
     */
    private void repair() {
        if (!damageEventside.isEmpty()) {
            // repaint causes strange window effects so force paint straight
            // away, slowness added :(
            paintImmediately(damageEventside);
            damageEventside.width = 0;
        }
    }

    /**
     * Expects to be on the event thread.
     */
    private void repairThumbnail() {
        Image i = image_;
        if (i != null) {
            // int w = Translate(_streamWidth);
            // int h = Translate(_streamHeight);
            // thumbnailGraphics.drawImage(
            // i,
            // 0, 0, (int)(w * thumbnailRatio), (int)(h * thumbnailRatio),
            // 0, 0, w, h, null);
        }
    }

    /**
     * @see VNCClient#clientFrameBufferUpdate
     */
    public void clientFrameBufferUpdate() {
        try {
            // Note that we block here so that we don't lose any updates to
            // damageStreamside while the current rectangle is being copied.
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    damageEventside.add(damageStreamside);
                    damageStreamside.width = 0;
                    repair();
                }
            });
        } catch (InterruptedException exn) {
            _console.writeline(exn.getMessage());
        } catch (InvocationTargetException exn) {
            _console.writeline(exn.getMessage());
        }
    }

    /**
     * @see VNCClient#clientDrawImage
     */
    public void clientDrawImage(final Image image, final int x, final int y,
            final int width, final int height) {
        // this doesn't give as accurate graphics as we would like but it will
        // get replaced when the window repaints with a nicer looking image
        imageGraphics_.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
        imageGraphics_.setRenderingHint(RenderingHints.KEY_RENDERING,
                RenderingHints.VALUE_RENDER_SPEED);
        imageGraphics_.drawImage(image, x, y, null);
        damage(x, y, width, height);
    }

    /**
     * @see VNCClient#clientSetCursor
     */
    public void clientSetCursor(Image image, int x, int y) {

        /*
         * If the image size does not match the platform's preferred cursor
         * size, then pad out with some transparency. We don't need to do this
         * on Linux clients it seems, but Windows throws
         * "java.awt.image.ImagingOpException: Unable to transform src image"
         * when trying to do the scaling itself.
         */
        int imageWidth = image.getWidth(null);
        int imageHeight = image.getHeight(null);
        Dimension cursorSize = toolkit.getBestCursorSize(imageWidth,
                imageHeight);

        if (!(imageWidth > cursorSize.width || imageHeight > cursorSize.height)
                && (imageWidth < cursorSize.width || imageHeight < cursorSize.height)) {
            BufferedImage bi = new BufferedImage(cursorSize.width,
                    cursorSize.height, BufferedImage.TYPE_INT_ARGB);
            Graphics2D g = (Graphics2D) bi.getGraphics();
            g.setRenderingHint(RenderingHints.KEY_RENDERING,
                    RenderingHints.VALUE_RENDER_SPEED);
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                    RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
            g.setColor(new Color(0, 0, 0, 0));
            g.fillRect(0, 0, cursorSize.width, cursorSize.height);
            g.drawImage(image, 0, 0, null);
            image = bi;
        }

        final Cursor cursor = toolkit.createCustomCursor(image,
                new Point(x, y), "");

        _cursor = cursor;

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                setCursor(cursor);
            }
        });

    }

    /**
     * @see VNCClient#clientCopyRectangle
     */
    public void clientCopyRectangle(final int x, final int y, final int width,
            final int height, final int dx, final int dy) {
        imageGraphics_.copyArea(x, y, width, height, dx - x, dy - y);
        damage(dx, dy, width, height);
    }

    /**
     * @see VNCClient#clientFillRectangle
     */
    public void clientFillRectangle(final int x, final int y, final int width,
            final int height, final Color color) {
        imageGraphics_.setColor(color);
        imageGraphics_.fillRect(x, y, width, height);
        damage(x, y, width, height);
    }

    /**
     * @see VNCClient#clientCutText
     */
    public void clientCutText(final String text) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                serverText_ = text;
            }
        });
    }

    public void getClipboard() {
        StringSelection stringSelection = new StringSelection(serverText_);
        Clipboard clipboard = toolkit.getSystemClipboard();
        try {
            clipboard.setContents(stringSelection, this);
        } catch (IllegalStateException e) {
            _console.writeline(e.getMessage());
        }
    }

    public void setClipboard() {
        Clipboard clipboard = toolkit.getSystemClipboard();
        Transferable contents = clipboard.getContents(null);
        if ((contents != null)
                && contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
            try {
                String text = (String) contents
                        .getTransferData(DataFlavor.stringFlavor);
                stream_.clientCutText(text);
            } catch (Throwable t) {
                _console.writeline(t.getMessage());
            }
        }
    }

    public void lostOwnership(Clipboard clipboard, Transferable contents) {
    }

    /**
     * @see FocusListener#focusGained
     */
    public void focusGained(FocusEvent e) {
        // Send release events for all the modifier keys.

        // setBorder(focusBorder);
        if (_cursor != null) {
            setCursor(_cursor);
        } else {
            int[] pixels = new int[16 * 16];
            Image image = Toolkit.getDefaultToolkit()
                    .createImage(
                            new java.awt.image.MemoryImageSource(16, 16,
                                    pixels, 0, 16));
            setCursor(Toolkit.getDefaultToolkit().createCustomCursor(image,
                    new Point(0, 0), "invisibleCursor"));
        }
        if (!isFullscreen) {
            this.setBorder(BorderFactory.createCompoundBorder(BorderFactory
                    .createLineBorder(Color.BLUE, 1), BorderFactory
                    .createLineBorder(getParent().getBackground(), 2)));
        } else {
            this.setBorder(BorderFactory.createEmptyBorder());
        }
        this.setFocusable(true);

        sendRelease(KeyEvent.VK_ALT);
        sendRelease(KeyEvent.VK_CONTROL);
        sendRelease(KeyEvent.VK_SHIFT);
    }

    private void sendRelease(int keycode) {
        stream_.keyEvent(false, keyMap.getMappedKey(keycode));
    }

    /**
     * @see FocusListener#focusLost
     */
    public void focusLost(FocusEvent e) {
        if (this.getParent() != null) {
            if (!isFullscreen) {
                this.setBorder(BorderFactory.createCompoundBorder(BorderFactory
                        .createLineBorder(getParent().getBackground(), 1),
                        BorderFactory.createLineBorder(getParent()
                                .getBackground(), 2)));
            } else {
                this.setBorder(BorderFactory.createEmptyBorder());
            }
            setCursor(Cursor.getDefaultCursor());
        }
    }

    public ImageIcon getThumbnailIcon() {
        return thumbnailIcon;
    }
}
