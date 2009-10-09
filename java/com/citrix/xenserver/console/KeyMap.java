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

import java.awt.event.KeyEvent;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

public class KeyMap {
    private static final Logger logger = Logger.getLogger(KeyMap.class.getName());

    public static KeyMap getInstance() {
        return instance_;
    }

    static KeyMap instance_ = new KeyMap();

    Map<Integer, Integer> map_ = new HashMap<Integer, Integer>();

    public KeyMap() {
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(getClass()
                    .getResourceAsStream("KeyMap.properties")));
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.length() == 0 || line.startsWith("#")) {
                    continue;
                }
                int index = line.indexOf("=0x");
                if (index == -1) {
                    throw new IOException("malformed line: " + line);
                }
                String keycode_str = line.substring(0, index);
                int keycode = KeyEvent.class.getField(keycode_str).getInt(null);
                int keysym = Integer.parseInt(line.substring(index + 3), 16);
                if (map_.containsKey(keycode)) {
                    throw new IOException("Duplicate entry " + line);
                }
                map_.put(keycode, keysym);
            }
        } catch (Throwable t) {
            logger.log(Level.WARNING, t.getMessage(), t);
        }
    }

    int unicodeToKeysym(int c) {
        if (c < 0x20) {
            // The Ctrl- block. By the RFB spec, these should be
            // transmitted as separate Ctrl and lower-case letter
            // keystrokes. We've already sent the down-Ctrl, so now we
            // just need to send the code for the letter. Add 0x60 to move
            // from the 0-0x1f ASCII range into 0x60-0x7f.
            return c + 0x60;
        } else if ((c <= 0x7e) || ((0xa0 <= c) && (c <= 0xff))) {
            return c;
        }
        return -1;
    }

    public int getKeysym(KeyEvent event) {
        int result = getMappedKey(event.getKeyCode());
        if (result == -1) {
            char c = event.getKeyChar();
            return c == KeyEvent.CHAR_UNDEFINED ? -1 : unicodeToKeysym(c);
        } else {
            return result;
        }
    }

    /**
     * @param keycode
     *            One of the KeyEvent VK_ constants.
     * @return The keysym for that code, if it is in the map, or -1 otherwise.
     */
    public int getMappedKey(int keycode) {
        Integer ks = map_.get(keycode);
        return ks == null ? -1 : ks;
    }

}
