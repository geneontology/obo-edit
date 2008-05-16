package org.oboedit.gui.components;


import javax.swing.*;

import org.bbop.framework.AbstractGUIComponent;

import org.apache.log4j.*;

public class HelloWorldComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HelloWorldComponent.class);

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
