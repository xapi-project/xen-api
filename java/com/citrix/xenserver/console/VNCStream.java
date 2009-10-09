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

import java.awt.Color;
// import java.awt.Graphics;
// import java.awt.Dimension;
import java.awt.Image;
// import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
// import java.awt.image.IndexColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DirectColorModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
// import java.io.Console;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
// import java.io.InputStream;
import java.net.Socket;
import java.util.Arrays;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESKeySpec;
import javax.swing.SwingUtilities;

/**
 * Implements "The RFB Protocol" Version 3.3. Includes support for the Cursor
 * pseudo-encoding, for client-side cursor rendering.
 * 
 * <p>
 * See <a href="http://realvnc.com/docs/rfbproto.pdf" /> for the specification.
 */
public class VNCStream {
    private static final Logger logger = Logger.getLogger(VNCStream.class.getName());

    public static final int NO_IP = 0;
    public static final int INCORRECT_PASSWORD = 1;
    public static final int CONNECTION_FAILED = 2;
    public static final int CONNECTION_SUCCEEDED = 4;

    private static final int THUMBNAIL_SLEEP_TIME = 500;

    private static final ColorSpace colorSpace_ = ColorSpace
            .getInstance(ColorSpace.CS_sRGB);

    public static class VNCException extends IOException {
        static final long serialVersionUID = 0;

        VNCException(String msg) {
            super(msg);
        }
    }

    private static class ProtocolVersion {
        int major_;
        int minor_;

        ProtocolVersion(int major_, int minor_) {
            this.major_ = major_;
            this.minor_ = minor_;
        }

    }

    private static final int RAW_ENCODING = 0;
    private static final int COPY_RECTANGLE_ENCODING = 1;
    private static final int RRE_ENCODING = 2;
    private static final int CORRE_ENCODING = 4;
    private static final int HEXTILE_ENCODING = 5;
    private static final int CURSOR_PSEUDO_ENCODING = -239;
    private static final int DESKTOP_SIZE_PSEUDO_ENCODING = -223;

    private static final int SET_PIXEL_FORMAT = 0;
    private static final int SET_ENCODINGS = 2;
    private static final int FRAMEBUFFER_UPDATE_REQUEST = 3;
    private static final int KEY_EVENT = 4;
    private static final int POINTER_EVENT = 5;
    private static final int CLIENT_CUT_TEXT = 6;

    private static final int RAW_SUBENCODING = 1;
    private static final int BACKGROUND_SPECIFIED_SUBENCODING = 2;
    private static final int FOREGROUND_SPECIFIED_SUBENCODING = 4;
    private static final int ANY_SUBRECTS_SUBENCODING = 8;
    private static final int SUBRECTS_COLORED_SUBENCODING = 16;

    private static final int FRAME_BUFFER_UPDATE = 0;
    private static final int BELL = 2;
    private static final int SERVER_CUT_TEXT = 3;

    private static final int MAX_STRING_LENGTH = 1 << 16;

    private boolean _connected = false;

    /**
     * The encodings used when the display is 32bpp. Note that these are
     * ordered: preferred encoding first.
     */
    private static final int[] encodings_32 = new int[] { HEXTILE_ENCODING,
            CORRE_ENCODING, RRE_ENCODING, COPY_RECTANGLE_ENCODING,
            RAW_ENCODING, CURSOR_PSEUDO_ENCODING, DESKTOP_SIZE_PSEUDO_ENCODING };

    /**
     * The encodings used when the display is 8bpp. We don't support client-side
     * cursors in this mode. Note that these are ordered: preferred encoding
     * first.
     */
    private static final int[] encodings_8 = new int[] { HEXTILE_ENCODING,
            CORRE_ENCODING, RRE_ENCODING, COPY_RECTANGLE_ENCODING,
            RAW_ENCODING, DESKTOP_SIZE_PSEUDO_ENCODING };

    /*
     * By the RFB spec, the keycodes are those used by X: see
     * /usr/include/X11/keysymdef.h. These are the three that we need for
     * ctrl-alt-delete.
     */
    private static final int KEY_CTRL_L = 0xffe3;
    private static final int KEY_ALT_L = 0xffe9;
    private static final int KEY_DELETE = 0xffff;

    static {
        try {
            // Force early class loading.
            byte[] keyBytes = new byte[8];
            byte[] challenge = new byte[16];
            Cipher cipher = Cipher.getInstance("DES");
            cipher.init(Cipher.ENCRYPT_MODE, SecretKeyFactory
                    .getInstance("DES")
                    .generateSecret(new DESKeySpec(keyBytes)));
            cipher.doFinal(challenge);
        } catch (Exception e) {
            assert false : e;
        }
    }

    private final VNCClient client_;

    public Helper helper = null;
    private ConnectionListener _listener;
    private ConsoleListener _console;

    public VNCStream(VNCClient client, ConnectionListener listener,
            ConsoleListener console) {
        client_ = client;
        _listener = listener;
        _console = console;
    }

    /**
     * Takes responsibility for closing the given socket.
     */
    /*
     * public int testConnectivity(Socket socket, char [] password) { try { try {
     * new Helper(client_, socket, password).connect(); } finally {
     * _console.writeline("VNCStream.testConnectivity closing socket");
     * socket.close(); } logger.error("Connection succeeded"); return
     * CONNECTION_SUCCEEDED; } catch (VNCException exn) {
     * logger.error("Connection failed (VNC): " + exn); return
     * INCORRECT_PASSWORD; } catch (IOException exn) { logger.error("Connection
     * failed: " + exn); return CONNECTION_FAILED; } }
     */

    /**
     * 
     */
    public void connect(RawHTTP rawHttp, char[] password) throws IOException {
        Helper h;
        _console.writeline("Connecting...");
        synchronized (this) {
            helper = h = new Helper(client_, rawHttp, password, _listener,
                    _console);
        }

        h.start();
        _connected = true;
        _console.writeline("Connected");
    }

    public void connectSocket(Socket socket, char[] password)
            throws IOException {
        Helper h;
        _console.writeline("Connecting...");
        synchronized (this) {
            helper = h = new Helper(client_, socket, password, _listener,
                    _console);
        }

        h.start();
        _connected = true;
        _console.writeline("Connected");
    }

    /**
     * Nothrow guarantee.
     */
    public void disconnect() {
        Helper h;

        synchronized (this) {
            h = helper;
            helper = null;
        }

        if (h != null) {
            h.terminate();
        }
        _connected = false;
    }

    public synchronized void pause() {
        if (helper != null) {
            helper.pause();
        }
    }

    public boolean isConnected() {
        return this._connected;
    }

    public synchronized void unpause() {
        if (helper != null) {
            helper.unpause();
        }
    }

    public synchronized void setUpdateThumbnail(boolean updateThumbnail) {
        if (helper != null) {
            helper.setUpdateThumbnail(updateThumbnail);
        }
    }

    public synchronized void keyEvent(boolean down, int key) {
        if (helper != null) {
            helper.keyEvent(down, key);
        }
    }

    public synchronized void pointerEvent(int buttonMask, int x, int y) {
        if (helper != null) {
            helper.pointerEvent(buttonMask, x, y);
        }
    }

    public synchronized void sendCtrlAltDelete() {
        if (helper != null) {
            helper.sendCtrlAltDelete();
        }
    }

    /**
     * Pass a wheel scroll event to the server, at the given co-ordinates.
     * 
     * @param r
     *            The number of clicks to scroll, negative for up/away, positive
     *            for down/towards.
     */
    public synchronized void pointerWheelEvent(int x, int y, int r) {
        if (helper != null) {
            helper.pointerWheelEvent(x, y, r);
        }
    }

    public synchronized void clientCutText(String text) {
        if (helper != null) {
            helper.clientCutText(text);
        }
    }

    private static final class Helper extends Thread {

        /*
         * To write, you must synchronize on out_. All the methods named
         * writeXyz expect to be synchronized on out_ already. Reading is only
         * allowed from the thread inside run(), and only one thread should be
         * in that function at any one time. Do not read whilst holding the lock
         * on out_, because reads can block indefinitely, and the KeyEvent
         * handling needs to be able to write from the AWT thread.
         * 
         * paused and updateThumbnail may only be accessed under synchronisation
         * on this object. All other non-final fields may only be accessed from
         * the thread inside run().
         * 
         */

        private final VNCClient client_;
        private final char[] password;

        private final DataInputStream in_;
        private final DataOutputStream out_;
        private final Socket socket;

        private volatile boolean running = true;
        private volatile Thread runThread = null;

        private boolean paused = false;
        private boolean updateThumbnail = false;

        private int width_;
        private int height_;
        private VNCPixelFormat pixelFormat_;
        private int[] bandOffsets32_;
        private ColorModel colorModel32_;
        private int[] bitMasks8_;
        private ColorModel colorModel8_;

        /**
         * This buffer is used by readFillRectangles (part of the Hextile
         * handling), and readColor. Only the first few bytes are used in
         * readColor. In readFillRectangles, the maximum space required is 1530 ==
         * 6 * 255 -- 4 bytes per colour plus two bytes for x,y,w,h, multiplied
         * by 255, the maximum number of rectangles to be sent in one go.
         */
        private byte[] data = new byte[1530];

        private void writePadding(int n) throws IOException {
            for (int i = 0; i < n; ++i) {
                out_.write(0);
            }
        }

        private void writeFlag(boolean v) throws IOException {
            out_.writeByte(v ? 1 : 0);
        }

        private void writeInt8(int v) throws IOException {
            out_.writeByte(v);
        }

        private void writeInt16(int v) throws IOException {
            out_.writeShort(v);
        }

        private void writeInt32(int v) throws IOException {
            out_.writeInt(v);
        }

        private void writeString(String s) throws IOException {
            writeInt32(s.length());
            out_.write(s.getBytes("US-ASCII"));
        }

        private void readPadding(int n) throws IOException {
            in_.skip(n);
        }

        private void readFully(byte b[], int off, int len) throws IOException {
            int n = 0;
            while (n < len) {
                int count = in_.read(b, off + n, len - n);
                if (count < 0) {
                    throw new EOFException();
                }
                n += count;
            }
        }

        private boolean readFlag() throws IOException {
            return readCard8() == 0 ? false : true;
        }

        private int readCard8() throws IOException {
            int v = in_.read();
            if (v < 0) {
                throw new EOFException();
            }
            return v;
        }

        private int readCard16() throws IOException {
            int b1 = readCard8();
            int b0 = readCard8();
            return (short) ((b1 << 8) | b0);
        }

        private int readCard32() throws IOException {
            int b3 = readCard8();
            int b2 = readCard8();
            int b1 = readCard8();
            int b0 = readCard8();
            return (b3 << 24) | (b2 << 16) | (b1 << 8) | b0;
        }

        private String readString() throws IOException {
            int length = readCard32();
            if (length < 0 || length >= MAX_STRING_LENGTH) {
                throw new VNCException("Invalid string length: " + length);
            }
            byte[] buffer = new byte[length];
            readFully(buffer, 0, length);
            return new String(buffer, "US-ASCII");
        }

        private ProtocolVersion getProtocolVersion() throws IOException {
            byte[] buffer = new byte[12];
            readFully(buffer, 0, 12);
            String s = new String(buffer, "US-ASCII");
            Pattern pattern = Pattern.compile("RFB ([0-9]{3})\\.([0-9]{3})\n");
            Matcher matcher = pattern.matcher(s);
            if (!matcher.matches()) {
                throw new VNCException("expected protocol version: " + s);
            }

            return new ProtocolVersion(Integer.parseInt(matcher.group(1)),
                    Integer.parseInt(matcher.group(2)));
        }

        private void sendProtocolVersion() throws IOException {
            synchronized (out_) {
                out_.write("RFB 003.003\n".getBytes("US-ASCII"));
                out_.flush();
            }
        }

        private VNCPixelFormat readPixelFormat() throws IOException {
            VNCPixelFormat pixelFormat = new VNCPixelFormat();
            pixelFormat.bitsPerPixel_ = readCard8();
            pixelFormat.depth_ = readCard8();
            pixelFormat.bigEndian_ = readFlag();
            pixelFormat.trueColor_ = readFlag();
            pixelFormat.redMax_ = readCard16();
            pixelFormat.greenMax_ = readCard16();
            pixelFormat.blueMax_ = readCard16();
            pixelFormat.redShift_ = readCard8();
            pixelFormat.greenShift_ = readCard8();
            pixelFormat.blueShift_ = readCard8();
            readPadding(3);
            _console.writeline("readPixelFormat " + pixelFormat.bitsPerPixel_
                    + " " + pixelFormat.depth_);
            return pixelFormat;
        }

        private void writePixelFormat(VNCPixelFormat pixelFormat)
                throws IOException {
            _console.writeline("writePixelFormat " + pixelFormat.bitsPerPixel_
                    + " " + pixelFormat.depth_);

            writeInt8(pixelFormat.bitsPerPixel_);
            writeInt8(pixelFormat.depth_);
            writeFlag(pixelFormat.bigEndian_);
            writeFlag(pixelFormat.trueColor_);
            writeInt16(pixelFormat.redMax_);
            writeInt16(pixelFormat.greenMax_);
            writeInt16(pixelFormat.blueMax_);
            writeInt8(pixelFormat.redShift_);
            writeInt8(pixelFormat.greenShift_);
            writeInt8(pixelFormat.blueShift_);
            writePadding(3);
        }

        private void writePixelFormat() throws IOException {
            // set pixel format
            writeInt8(SET_PIXEL_FORMAT);
            writePadding(3);
            writePixelFormat(pixelFormat_);
        }

        private void force32bpp() throws IOException {
            _console.writeline("force32bpp()");

            pixelFormat_.bitsPerPixel_ = 32;
            pixelFormat_.depth_ = 24;
            pixelFormat_.trueColor_ = true;
            pixelFormat_.redMax_ = 255;
            pixelFormat_.greenMax_ = 255;
            pixelFormat_.blueMax_ = 255;
            pixelFormat_.redShift_ = 16;
            pixelFormat_.greenShift_ = 8;
            pixelFormat_.blueShift_ = 0;
            // Note that we keep the big endian value from the server.

            setupPixelFormat();

            synchronized (out_) {
                writePixelFormat();
            }
        }

        private void setupPixelFormat() throws IOException {
            _console.writeline("setupPixelFormat(" + pixelFormat_.bitsPerPixel_
                    + ")");

            if (pixelFormat_.bitsPerPixel_ == 32) {
                bandOffsets32_ = pixelFormat_.bigEndian_ ? (new int[] {
                        3 - (pixelFormat_.redShift_ >> 3),
                        3 - (pixelFormat_.greenShift_ >> 3),
                        3 - (pixelFormat_.blueShift_ >> 3), 0 }) : (new int[] {
                        pixelFormat_.redShift_ >> 3,
                        pixelFormat_.greenShift_ >> 3,
                        pixelFormat_.blueShift_ >> 3, 3 });
                colorModel32_ = new ComponentColorModel(colorSpace_, true, // hasAlpha
                        true, // isAlphaPremultiplied
                        Transparency.OPAQUE, DataBuffer.TYPE_BYTE);
            } else if (pixelFormat_.bitsPerPixel_ == 8) {
                bitMasks8_ = new int[] {
                        pixelFormat_.redMax_ << pixelFormat_.redShift_,
                        pixelFormat_.greenMax_ << pixelFormat_.greenShift_,
                        pixelFormat_.blueMax_ << pixelFormat_.blueShift_, };
                colorModel8_ = new DirectColorModel(8, bitMasks8_[0],
                        bitMasks8_[1], bitMasks8_[2]);
            } else {
                throw new IOException("unexpected bits per pixel: "
                        + pixelFormat_.bitsPerPixel_);
            }
        }

        private void writeSetEncodings() throws IOException {
            _console.writeline("writeSetEncodings");

            int[] encodings;

            if (pixelFormat_.bitsPerPixel_ == 8) {
                _console.writeline("Disabling local cursors for VNC.");
                encodings = encodings_8;
            } else {
                encodings = encodings_32;
            }

            writeSetEncodings(encodings);
        }

        private void writeSetEncodings(int[] encodings) throws IOException {
            writeInt8(SET_ENCODINGS);
            writePadding(1);
            writeInt16(encodings.length);
            for (int i = 0; i < encodings.length; ++i) {
                writeInt32(encodings[i]);
            }
        }

        private void writeFramebufferUpdateRequest(int x, int y, int width,
                int height, boolean incremental) throws IOException {
            writeInt8(FRAMEBUFFER_UPDATE_REQUEST);
            writeFlag(incremental);
            writeInt16(x);
            writeInt16(y);
            writeInt16(width);
            writeInt16(height);
        }

        private static byte reverse(byte v) {
            byte r = 0;
            if ((v & 0x01) != 0)
                r |= 0x80;
            if ((v & 0x02) != 0)
                r |= 0x40;
            if ((v & 0x04) != 0)
                r |= 0x20;
            if ((v & 0x08) != 0)
                r |= 0x10;
            if ((v & 0x10) != 0)
                r |= 0x08;
            if ((v & 0x20) != 0)
                r |= 0x04;
            if ((v & 0x40) != 0)
                r |= 0x02;
            if ((v & 0x80) != 0)
                r |= 0x01;
            return r;
        }

        private void handshake() throws IOException {
            ProtocolVersion protocolVersion = getProtocolVersion();
            if (protocolVersion.major_ < 3) {
                throw new VNCException("don't know protocol version "
                        + protocolVersion.major_);
            }
        }

        private void authenticationExchange() throws IOException {
            // _console.writeline("authenticationExchange");

            int scheme = readCard32();
            if (scheme == 0) {
                String reason = readString();
                throw new VNCException("connection failed: " + reason);
            } else if (scheme == 1) {
                // no authentication needed
            } else if (scheme == 2) {
                try {
                    byte[] keyBytes = new byte[8];
                    for (int i = 0; (i < 8) && (i < password.length); ++i) {
                        keyBytes[i] = reverse((byte) password[i]);
                    }
                    Arrays.fill(password, '\0');
                    DESKeySpec keySpec = new DESKeySpec(keyBytes);
                    SecretKeyFactory keyFactory = SecretKeyFactory
                            .getInstance("DES");
                    SecretKey secretKey = keyFactory.generateSecret(keySpec);
                    Cipher cipher = Cipher.getInstance("DES");
                    cipher.init(Cipher.ENCRYPT_MODE, secretKey);
                    Arrays.fill(keyBytes, (byte) 0);
                    byte[] challenge = new byte[16];
                    readFully(challenge, 0, 16);
                    byte[] response = cipher.doFinal(challenge);
                    synchronized (out_) {
                        out_.write(response, 0, 16);
                        out_.flush();
                    }
                } catch (Exception e) {
                    throw new VNCException("authentication: DES error");
                }
                int status = readCard32();
                if (status == 0) {
                    // ok
                } else if (status == 1) {
                    throw new VNCException("authentication failed");
                } else if (status == 2) {
                    throw new VNCException("too many authentication failures");
                }
            } else {
                throw new VNCException("unexpected authentication scheme: "
                        + scheme);
            }
        }

        private void clientInitialization() throws IOException {
            _console.writeline("clientInitialisation");
            synchronized (out_) {
                writeFlag(true); // shared
                out_.flush();
            }
        }

        private void serverInitialization() throws IOException {
            _console.writeline("serverInitialisation");
            int width = readCard16();
            int height = readCard16();
            pixelFormat_ = readPixelFormat();
            readString(); /* The desktop name -- we don't care. */

            /*
             * if (pixelFormat_.bitsPerPixel_ == 16) { // N.B. Only 8, 16, and
             * 32 are allowed by the RFB spec. logger.info("Forcing VNC server
             * to 32 bpp.");
             * 
             * force32bpp(); } else { setupPixelFormat(); }
             */

            force32bpp();

            desktopSize(width, height);
            synchronized (out_) {
                writeSetEncodings();
            }
        }

        /**
         * Expects to be synchronized on out_.
         */
        private void writeKey(boolean down, int key) throws IOException {
            writeInt8(KEY_EVENT);
            writeFlag(down);
            writePadding(2);
            writeInt32(key);
        }

        void keyEvent(boolean down, int key) {
            synchronized (out_) {
                try {
                    writeKey(down, key);
                    out_.flush();
                } catch (IOException e) {
                    // _console.writeline(e);
                }
            }
        }

        void pointerEvent(int buttonMask, int x, int y) {
            if (x < 0) {
                x = 0;
            } else if (x >= width_) {
                x = width_ - 1;
            }

            if (y < 0) {
                y = 0;
            } else if (y >= height_) {
                y = height_ - 1;
            }

            synchronized (out_) {
                try {
                    pointerEvent_(buttonMask, x, y);
                    out_.flush();
                } catch (IOException e) {
                    _console.writeline(e.getMessage());
                }
            }
        }

        public void sendCtrlAltDelete() {
            synchronized (out_) {
                try {
                    writeKey(true, KEY_CTRL_L);
                    writeKey(true, KEY_ALT_L);
                    writeKey(true, KEY_DELETE);
                    writeKey(false, KEY_CTRL_L);
                    writeKey(false, KEY_ALT_L);
                    writeKey(false, KEY_DELETE);
                    out_.flush();
                } catch (IOException e) {
                    _console.writeline(e.getMessage());
                }
            }
        }

        void pointerWheelEvent(int x, int y, int r) {
            synchronized (out_) {
                try {
                    /*
                     * The RFB protocol specifies a down-up pair for each scroll
                     * of the wheel, on button 4 for scrolling up, and button 5
                     * for scrolling down.
                     */

                    int m;

                    if (r < 0) {
                        r = -r;
                        m = 8;
                    } else {
                        m = 16;
                    }
                    for (int i = 0; i < r; i++) {
                        pointerEvent_(m, x, y);
                        pointerEvent_(0, x, y);
                    }

                    out_.flush();
                } catch (IOException e) {
                    _console.writeline(e.getMessage());
                }
            }
        }

        private void pointerEvent_(int buttonMask, int x, int y)
                throws IOException {
            writeInt8(POINTER_EVENT);
            writeInt8(buttonMask);
            writeInt16(x);
            writeInt16(y);
        }

        void clientCutText(String text) {
            _console.writeline("cutEvent");

            synchronized (out_) {
                try {
                    writeInt8(CLIENT_CUT_TEXT);
                    writePadding(3);
                    writeString(text);
                    out_.flush();
                } catch (IOException e) {
                    _console.writeline(e.getMessage());
                }
            }
        }

        /**
         * @param mask_data
         *            should be a cursor mask, as specified by the RFB protocol
         *            specification for the Cursor pseudo-encoding (1-bpp,
         *            packed). If null, the mask is assumed to be totally opaque
         *            (as used by normal "raw" packets). Masks are not supported
         *            for 8-bpp images.
         */
        private Image createImage(int width, int height, byte[] data,
                int length, byte[] mask_data) {
            if (pixelFormat_.bitsPerPixel_ == 32) {
                SampleModel sampleModel = new PixelInterleavedSampleModel(
                        DataBuffer.TYPE_BYTE, width, height, 4, width * 4,
                        bandOffsets32_);

                if (mask_data == null) {
                    int index = 3;
                    for (int i = 0; i < width * height; ++i) {
                        data[index] = (byte) 0xff;
                        index += 4;
                    }
                } else {
                    int stride = (width + 7) >> 3;
                    int index = 3;
                    int mask_index = 0;
                    for (int y = 0; y < height; y++) {
                        for (int x = 0; x < width; x++) {
                            data[index] = ((mask_data[mask_index + (x / 8)] & (1 << (7 - (x & 7)))) == 0) ? (byte) 0
                                    : (byte) 0xff;
                            index += 4;
                        }
                        mask_index += stride;
                    }
                }

                DataBuffer dataBuffer = new DataBufferByte(data, length);
                WritableRaster raster = Raster.createWritableRaster(
                        sampleModel, dataBuffer, null);
                return new BufferedImage(colorModel32_, raster, true, null);
            } else if (pixelFormat_.bitsPerPixel_ == 8) {
                SampleModel sampleModel = new SinglePixelPackedSampleModel(
                        DataBuffer.TYPE_BYTE, width, height, width, bitMasks8_);
                DataBuffer dataBuffer = new DataBufferByte(data, length);
                WritableRaster raster = Raster.createWritableRaster(
                        sampleModel, dataBuffer, null);
                return new BufferedImage(colorModel8_, raster, true, null);
            } else {
                throw new RuntimeException("unexpected bits per pixel");
            }
        }

        private void readRawEncoding(int x, int y, int width, int height)
                throws IOException {
            client_.clientDrawImage(readRawEncoding_(width, height, false), x,
                    y, width, height);
        }

        /**
         * @param mask
         *            If true, read a mask after the raw data, as used by the
         *            Cursor pseudo-encoding.
         */
        private Image readRawEncoding_(int width, int height, boolean mask)
                throws IOException {
            if (width < 0 || height < 0) {
                throw new VNCException("Invalid size: " + width + " x "
                        + height);
            }
            int pixelSize = (pixelFormat_.bitsPerPixel_ + 7) >> 3;
            int length = width * height * pixelSize;
            byte[] data = new byte[length];
            readFully(data, 0, length);

            byte[] mask_data = null;
            if (mask) {
                int scanline = (width + 7) >> 3;
                int mask_length = scanline * height;

                mask_data = new byte[mask_length];
                readFully(mask_data, 0, mask_length);
            }

            return createImage(width, height, data, length, mask_data);
        }

        private void readCopyRectangleEncoding(int dx, int dy, int width,
                int height) throws IOException {
            int x = readCard16();
            int y = readCard16();
            client_.clientCopyRectangle(x, y, width, height, dx, dy);
        }

        private Color readColor() throws IOException {
            if (pixelFormat_.bitsPerPixel_ == 32) {
                readFully(data, 0, 4);
                return new Color(data[bandOffsets32_[0]] & 0xff,
                        data[bandOffsets32_[1]] & 0xff,
                        data[bandOffsets32_[2]] & 0xff);
            } else if (pixelFormat_.bitsPerPixel_ == 8) {
                int p = readCard8();
                int r = colorModel8_.getRed(p);
                int g = colorModel8_.getGreen(p);
                int b = colorModel8_.getBlue(p);
                return new Color(r, g, b);
            } else {
                throw new RuntimeException("unexpected bits per pixel");
            }
        }

        private void readRREEncoding(int x, int y, int width, int height)
                throws IOException {
            int n = readCard32();
            Color background = readColor();
            client_.clientFillRectangle(x, y, width, height, background);
            for (int i = 0; i < n; ++i) {
                Color foreground = readColor();
                int rx = readCard16();
                int ry = readCard16();
                int rw = readCard16();
                int rh = readCard16();
                client_.clientFillRectangle(x + rx, y + ry, rw, rh, foreground);
            }
        }

        private void readCoRREEncoding(int x, int y, int width, int height)
                throws IOException {
            int n = readCard32();
            Color background = readColor();
            client_.clientFillRectangle(x, y, width, height, background);
            for (int i = 0; i < n; ++i) {
                Color foreground = readColor();
                int rx = readCard8();
                int ry = readCard8();
                int rw = readCard8();
                int rh = readCard8();
                client_.clientFillRectangle(x + rx, x + ry, rw, rh, foreground);
            }
        }

        private void readFillRectangles(int rx, int ry, int n)
                throws IOException {
            int pixelSize = (pixelFormat_.bitsPerPixel_ + 7) >> 3;
            int length = n * (pixelSize + 2);
            readFully(data, 0, length);
            int index = 0;
            for (int i = 0; i < n; ++i) {
                Color foreground;
                if (pixelFormat_.bitsPerPixel_ == 32) {
                    foreground = new Color(
                            data[index + bandOffsets32_[0]] & 0xff, data[index
                                    + bandOffsets32_[1]] & 0xff, data[index
                                    + bandOffsets32_[2]] & 0xff);
                    index += 4;
                } else if (pixelFormat_.bitsPerPixel_ == 8) {
                    int p = data[index++];
                    int r = colorModel8_.getRed(p);
                    int g = colorModel8_.getGreen(p);
                    int b = colorModel8_.getBlue(p);
                    foreground = new Color(r, g, b);
                } else {
                    throw new RuntimeException("unexpected bits per pixel");
                }
                int sxy = data[index++] & 0xff;
                int sx = sxy >> 4;
                int sy = sxy & 0xf;
                int swh = data[index++] & 0xff;
                int sw = (swh >> 4) + 1;
                int sh = (swh & 0xf) + 1;
                client_.clientFillRectangle(rx + sx, ry + sy, sw, sh,
                        foreground);
            }
        }

        private void readRectangles(int rx, int ry, int n, Color foreground)
                throws IOException {
            for (int i = 0; i < n; ++i) {
                int sxy = readCard8();
                int sx = sxy >> 4;
                int sy = sxy & 0xf;
                int swh = readCard8();
                int sw = (swh >> 4) + 1;
                int sh = (swh & 0xf) + 1;
                client_.clientFillRectangle(rx + sx, ry + sy, sw, sh,
                        foreground);
            }
        }

        private void readHextileEncoding(int x, int y, int width, int height)
                throws IOException {
            Color background = Color.BLACK;
            Color foreground = Color.WHITE;
            int xCount = (width + 15) >> 4;
            int yCount = (height + 15) >> 4;
            for (int yi = 0; yi < yCount; ++yi) {
                int ry = y + (yi << 4);
                int rh = (yi == (yCount - 1)) ? height & 0xf : 16;
                if (rh == 0) {
                    rh = 16;
                }
                for (int xi = 0; xi < xCount; ++xi) {
                    int rx = x + (xi << 4);
                    int rw = (xi == (xCount - 1)) ? width & 0xf : 16;
                    if (rw == 0) {
                        rw = 16;
                    }
                    int mask = readCard8();
                    if ((mask & RAW_SUBENCODING) != 0) {
                        readRawEncoding(rx, ry, rw, rh);
                    } else {
                        if ((mask & BACKGROUND_SPECIFIED_SUBENCODING) != 0) {
                            background = readColor();
                        }
                        client_.clientFillRectangle(rx, ry, rw, rh, background);
                        if ((mask & FOREGROUND_SPECIFIED_SUBENCODING) != 0) {
                            foreground = readColor();
                        }
                        if ((mask & ANY_SUBRECTS_SUBENCODING) != 0) {
                            int n = readCard8();
                            if ((mask & SUBRECTS_COLORED_SUBENCODING) != 0) {
                                readFillRectangles(rx, ry, n);
                            } else {
                                readRectangles(rx, ry, n, foreground);
                            }
                        }
                    }
                }
            }
        }

        private void readCursorPseudoEncoding(int x, int y, int width,
                int height) throws IOException {
            try {
                client_.clientSetCursor(readRawEncoding_(width, height, true),
                        x, y);
            } catch (Throwable exn) {
                /*
                 * We were getting "java.awt.image.ImagingOpException: Unable to
                 * transform src image" inside the Toolkit.createCustomCursor
                 * call when using VNC from a Windows client to a Debian VM.
                 * This should have been fixed by the "getBestCursorSize" code
                 * that's gone into VNCCanvas.clientSetCursor, but in case a
                 * similar problem arises, I've left this workaround in: we
                 * disable client-side cursors immediately.
                 */
                _console.writeline(exn.getMessage());
                _console
                        .writeline("Disabling local cursors for VNC due to exception.");
                synchronized (out_) {
                    writeSetEncodings(encodings_8);
                }
            }
        }

        private void readFrameBufferUpdate() throws IOException {
            readPadding(1);
            int n = readCard16();
            // _console.writeline("reading " + n + " rectangles");
            for (int i = 0; i < n; ++i) {
                int x = readCard16();
                int y = readCard16();
                int width = readCard16();
                int height = readCard16();
                int encoding = readCard32();
                // _console.writeline("read " + x + " " + y + " " + width + " "
                // +
                // height + " " + encoding);
                switch (encoding) {
                case RAW_ENCODING:
                    readRawEncoding(x, y, width, height);
                    break;
                case RRE_ENCODING:
                    readRREEncoding(x, y, width, height);
                    break;
                case CORRE_ENCODING:
                    readCoRREEncoding(x, y, width, height);
                    break;
                case COPY_RECTANGLE_ENCODING:
                    readCopyRectangleEncoding(x, y, width, height);
                    break;
                case HEXTILE_ENCODING:
                    readHextileEncoding(x, y, width, height);
                    break;

                case CURSOR_PSEUDO_ENCODING:
                    readCursorPseudoEncoding(x, y, width, height);
                    break;

                case DESKTOP_SIZE_PSEUDO_ENCODING:
                    desktopSize(width, height);
                    break;

                default:
                    throw new VNCException("unimplemented encoding: "
                            + encoding);
                }
            }
            client_.clientFrameBufferUpdate();
        }

        private void desktopSize(int width, int height) throws IOException {
            width_ = width;
            height_ = height;
            client_.clientDesktopSize(width, height);
        }

        private void readServerCutText() throws IOException {
            readPadding(3);
            String text = readString();
            client_.clientCutText(text);
        }

        private void readServerMessage() throws IOException {
            // _console.writeline("readServerMessage");
            // java.lang._console.writeline(in_.available());
            int type = readCard8();
            switch (type) {
            case FRAME_BUFFER_UPDATE:
                // _console.writeline("Update");
                readFrameBufferUpdate();
                break;
            case BELL:
                client_.clientBell();
                break;
            case SERVER_CUT_TEXT:
                _console.writeline("Cut text");
                readServerCutText();
                break;
            default:
                throw new VNCException("unknown server message: " + type);
            }
        }

        private ConnectionListener _listener;
        private ConsoleListener _console;

        Helper(VNCClient client, RawHTTP rawHttp, char[] password,
                ConnectionListener listener, ConsoleListener console)
                throws IOException {
            this.client_ = client;
            this.password = password;
            this.socket = rawHttp.getSocket();
            _listener = listener;
            _console = console;
            socket.setReceiveBufferSize(65536);
            socket.setTcpNoDelay(true);

            in_ = new DataInputStream(new BufferedInputStream(rawHttp
                    .getInputStream()));

            out_ = new DataOutputStream(new BufferedOutputStream(rawHttp
                    .getOutputStream()));
        }

        Helper(VNCClient client, Socket socket, char[] password,
                ConnectionListener listener, ConsoleListener console)
                throws IOException {
            this.client_ = client;
            this.password = password;
            this.socket = socket;
            _listener = listener;
            _console = console;
            socket.setReceiveBufferSize(65536);
            socket.setTcpNoDelay(true);

            in_ = new DataInputStream(new BufferedInputStream(socket
                    .getInputStream()));

            out_ = new DataOutputStream(new BufferedOutputStream(socket
                    .getOutputStream()));
        }

        void connect() throws IOException {
            handshake();
            sendProtocolVersion();
            authenticationExchange();
        }

        public void run() {
            runThread = Thread.currentThread();

            try {
                handshake();
                sendProtocolVersion();
                authenticationExchange();
                clientInitialization();
                serverInitialization();

                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        _listener.ConnectionMade();
                    }
                });

                boolean incremental = false;
                while (running) {
                    synchronized (this) {
                        while (paused) {
                            if (updateThumbnail) {
                                wait(THUMBNAIL_SLEEP_TIME);
                                break;
                            } else {
                                wait();
                            }
                        }
                    }

                    synchronized (out_) {
                        writeFramebufferUpdateRequest(0, 0, width_, height_,
                                incremental);
                        out_.flush();
                    }
                    incremental = true;
                    readServerMessage();
                }
            } catch (final IOException e) {
                final String message = e != null ? e.getMessage() : "Unknown";
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        _listener.ConnectionLost(message);
                    }
                });
            } catch (InterruptedException exn) {
                // We're being terminated.
                _console.writeline(exn.getMessage());
            } finally {
                close();
                runThread = null;
            }
        }

        /**
         * Nothrow guarantee.
         */
        private void close() {
            _console.writeline("VNCStream.Helper.close");
            try {
                try {
                    try {
                        in_.close();
                    } finally {
                        out_.close();
                    }
                } finally {
                    socket.close();
                }
            } catch (IOException exn) {
            } catch (RuntimeException exn) {
            }
        }

        /**
         * @see Object#finalize
         */
        protected void finalize() {
            /*
             * SwingUtilities.invokeLater( new Runnable() { public void run() {
             * _listener.ConnectionClosed(); } }); close();
             */
        }

        /**
         * Nothrow guarantee.
         */
        synchronized void pause() {
            paused = true;
        }

        /**
         * Nothrow guarantee.
         */
        synchronized void unpause() {
            paused = false;
            notify();
        }

        /**
         * Nothrow guarantee.
         */
        synchronized void setUpdateThumbnail(boolean updateThumbnail) {
            this.updateThumbnail = updateThumbnail;
            notify();
        }

        /**
         * Nothrow guarantee.
         */
        void terminate() {
            running = false;
            Thread t = runThread;
            if (t != null) {
                t.interrupt();
            }
        }
    }

}
