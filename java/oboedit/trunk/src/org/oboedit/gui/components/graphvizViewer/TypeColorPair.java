package org.oboedit.gui.components.graphvizViewer;

import java.io.Serializable;

public class TypeColorPair implements Serializable, Cloneable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	ColorPair pair;
	String typeid;

	public TypeColorPair() {
	}

	public TypeColorPair(ColorPair pair, String typeid) {
		this.pair = pair;
		this.typeid = typeid;
	}

	public ColorPair getPair() {
		return pair;
	}

	public String getTypeID() {
		return typeid;
	}

	public void setPair(ColorPair pair) {
		this.pair = pair;
	}

	public void setTypeID(String typeid) {
		this.typeid = typeid;
	}

	public String toString() {
		return "Link color: " + typeid + ", pair=" + pair;
	}
}

