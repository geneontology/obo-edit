package org.oboedit.piccolo;

import edu.umd.cs.piccolo.PNode;

public class PLayoutNode extends PNode {
	
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
