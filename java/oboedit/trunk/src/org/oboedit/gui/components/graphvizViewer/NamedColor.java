package org.oboedit.gui.components.graphvizViewer;

import java.awt.Color;
import java.io.Serializable;

import org.apache.log4j.*;

public class NamedColor implements Serializable, Cloneable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NamedColor.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected String name;
	protected Color color;

	public NamedColor() {
	}

	public NamedColor(String name, Color color) {
		this.name = name;
		this.color = color;
	}

	public Color getColor() {
		return color;
	}

	public String getName() {
		return name;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String toString() {
		return name;
	}
}

