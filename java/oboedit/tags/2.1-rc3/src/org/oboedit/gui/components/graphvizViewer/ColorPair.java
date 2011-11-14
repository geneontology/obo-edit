package org.oboedit.gui.components.graphvizViewer;

import java.awt.Color;
import java.io.Serializable;


import org.apache.log4j.*;

public class ColorPair implements Cloneable, Serializable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ColorPair.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	Color edge;
	Color label;
	int id = GraphvizCanvas.idgen++;

	public ColorPair() {
	}

	public ColorPair(Color edge, Color label) {
		this.edge = edge;
		this.label = label;
	}

	public Object clone() {
		try {
			ColorPair out = (ColorPair) super.clone();
			out.id = GraphvizCanvas.idgen++;
			return out;
		} catch (Exception ex) {
			return null;
		}
	}

	public Color getEdgeColor() {
		return edge;
	}

	public Color getLabelColor() {
		return label;
	}

	public void setEdgeColor(Color edge) {
		this.edge = edge;
	}

	public void setLabelColor(Color label) {
		this.label = label;
	}

	public String toString() {
		return "edgeColor = " + edge + ", labelColor = " + label;
	}
}
