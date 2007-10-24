package org.oboedit.gui.components;


import javax.swing.*;

import org.bbop.framework.AbstractGUIComponent;

public class HelloWorldComponent extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public HelloWorldComponent(String id) {
		super(id);
	}

	@Override
	public void init() {
		add(new JLabel("Hello world"));
	}

	@Override
	public String getName() {
		return "Hello World";
	}
}
