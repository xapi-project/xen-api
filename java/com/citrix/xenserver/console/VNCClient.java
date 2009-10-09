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
import java.awt.Image;

/**
 * All methods in this interface will be called from the VNC connection handler
 * thread.
 */
public interface VNCClient {

    public void clientDrawImage(Image image, int x, int y, int width, int height);

    /**
     * Set the client-side cursor to the given image. The parameters x and y
     * specify the cursor hotspot.
     */
    public void clientSetCursor(Image image, int x, int y);

    public void clientCopyRectangle(int x, int y, int width, int height,
            int dx, int dy);

    public void clientFillRectangle(int x, int y, int width, int height,
            Color color);

    public void clientFrameBufferUpdate();

    public void clientBell();

    public void clientCutText(String text);

    public void clientDesktopSize(int width, int height);

}
