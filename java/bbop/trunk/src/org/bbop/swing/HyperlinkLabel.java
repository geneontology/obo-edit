package org.bbop.swing;

import javax.swing.JLabel;
import javax.swing.event.HyperlinkListener;

public class HyperlinkLabel extends JLabel {

	protected LabelMouseHyperlinkBridge bridge = new LabelMouseHyperlinkBridge(this);
	
	{
		addMouseListener(bridge);
		addMouseMotionListener(bridge);
	}
	
	public HyperlinkLabel() {
		super();
	}

	public HyperlinkLabel(String text) {
		super(text);
	}

	
	public void addStringLinkListener(StringLinkListener listener) {
		bridge.addStringLinkListener(listener);
	}
	
	public void removeStringLinkListener(StringLinkListener listener) {
		bridge.removeStringLinkListener(listener);
	}

	public void addHyperLinkListener(HyperlinkListener listener) {
		bridge.addHyperLinkListener(listener);
	}

	public void removeHyperlinkListener(HyperlinkListener listener) {
		bridge.removeHyperlinkListener(listener);
	}
}
