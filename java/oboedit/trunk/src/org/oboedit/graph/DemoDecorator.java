package org.oboedit.graph;

import java.awt.Color;

import org.obo.datamodel.LinkedObject;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;

public class DemoDecorator implements NodeDecorator {

	public DemoDecorator() {
	}
	
	public PActivity decorate(PNode node, boolean noAnimation) {
		if (node instanceof OENode) {
			LinkedObject lo = (LinkedObject) ((OENode) node).getObject();
			if (lo.getName().contains("s"))
				((OENode) node).setPaint(Color.blue);
		}
		return null;
	}

	public boolean onlyDecorateAfterLayout() {
		// TODO Auto-generated method stub
		return false;
	}

}
