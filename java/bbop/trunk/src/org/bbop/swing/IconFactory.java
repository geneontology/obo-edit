/**
 * 
 */
package org.bbop.swing;

import java.net.URL;

import javax.swing.Icon;

public interface IconFactory {
	public Icon createIcon(URL url, int width, int height);
}