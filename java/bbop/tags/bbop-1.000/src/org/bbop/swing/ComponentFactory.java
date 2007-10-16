package org.bbop.swing;

import java.awt.Component;

public interface ComponentFactory {

	public Component getComponent(String id);
	public Object getConstraint(String name);
}
