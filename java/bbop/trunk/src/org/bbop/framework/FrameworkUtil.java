package org.bbop.framework;

import java.awt.Image;
import java.awt.Toolkit;

import org.apache.log4j.*;

public class FrameworkUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FrameworkUtil.class);

	protected static ClassLoader getClassLoader() {
		return FrameworkUtil.class.getClassLoader();
	}

	public static Image getResourceImage(String name) {
		return Toolkit.getDefaultToolkit().getImage(
				getClassLoader().getResource(
						"org/bbop/framework/resources/" + name));
	}
}
