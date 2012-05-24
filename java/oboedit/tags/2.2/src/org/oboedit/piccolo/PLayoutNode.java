package org.oboedit.piccolo;

import edu.umd.cs.piccolo.PNode;

import org.apache.log4j.*;

public class PLayoutNode extends PNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PLayoutNode.class);
	
	protected PiccoloLayoutManager layoutManager;

	protected void layoutChildren() {
		if (layoutManager != null) {
			layoutManager.layoutChildren(this);
		}
	}

	public PiccoloLayoutManager getLayoutManager() {
		return layoutManager;
	}

	public void setLayoutManager(PiccoloLayoutManager layoutManager) {
		this.layoutManager = layoutManager;
	}
}
