package org.bbop.framework;

import java.awt.Image;
import java.awt.Toolkit;

public class FrameworkUtil {

	protected static ClassLoader getClassLoader() {
		return FrameworkUtil.class.getClassLoader();
	}

	public static Image getResourceImage(String name) {
		return Toolkit.getDefaultToolkit().getImage(
				getClassLoader().getResource(
						"org/bbop/framework/resources/" + name));
	}
}
