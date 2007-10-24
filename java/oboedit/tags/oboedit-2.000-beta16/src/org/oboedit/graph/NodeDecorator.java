package org.oboedit.graph;

import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;

public interface NodeDecorator {
	public PActivity decorate(PNode node, boolean noAnimation);
	
	public boolean onlyDecorateAfterLayout();

}
