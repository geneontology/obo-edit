package org.oboedit.graph;

import java.awt.Dimension;

import org.obo.datamodel.IdentifiedObject;
import org.oboedit.gui.NodeLabelProvider;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.piccolo.ViewRenderedStyleText;

public class LabelBasedNodeSizeProvider implements NodeSizeProvider {

	protected ViewRenderedStyleText text = null;
	protected int initialNodeWidth = 200;
	protected NodeLabelProvider labelProvider;
	protected int xmargin = 0;
	protected int ymargin = 0;

	public LabelBasedNodeSizeProvider() {
	}
	
	public LabelBasedNodeSizeProvider(int xmargin, int ymargin) {
		super();
		this.xmargin = xmargin;
		this.ymargin = ymargin;
	}

	public String getLabel(ObjectSelector selector, IdentifiedObject io) {
		return labelProvider.getLabel(selector, io);
	}

	public Dimension getSize(ObjectSelector selector, IdentifiedObject io) {
		if (text == null)
			text = new ViewRenderedStyleText();
		text.setWidth(initialNodeWidth);
		text.setText(getLabel(selector, io), true);
		return new Dimension((int) text.getWidth() + xmargin, (int) text
				.getHeight()
				+ ymargin);
	}

	public void setLabelProvider(NodeLabelProvider nodeLabelProvider) {
		this.labelProvider = nodeLabelProvider;
	}

	public int getXmargin() {
		return xmargin;
	}

	public void setXmargin(int xmargin) {
		this.xmargin = xmargin;
	}

	public int getYmargin() {
		return ymargin;
	}

	public void setYmargin(int ymargin) {
		this.ymargin = ymargin;
	}
}
