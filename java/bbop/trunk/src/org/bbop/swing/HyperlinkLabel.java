package org.bbop.swing;

import java.awt.event.ActionListener;

import javax.swing.JLabel;
import javax.swing.event.HyperlinkListener;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;

public class HyperlinkLabel extends HTMLLabel {

	protected LabelMouseHyperlinkBridge bridge = new LabelMouseHyperlinkBridge(
			this);

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

	public void addActionListener(String command, ActionListener listener) {
		bridge.add(command, listener);
	}
	
	public void removeActionListener(String command, ActionListener listener) {
		bridge.remove(command, listener);
	}
	

	public void addStringLinkListener(StringLinkListener listener) {
		bridge.addStringLinkListener(listener);
	}

	public void removeStringLinkListener(StringLinkListener listener) {
		bridge.removeStringLinkListener(listener);
	}

	public void addHyperlinkListener(HyperlinkListener listener) {
		bridge.addHyperLinkListener(listener);
	}

	public void removeHyperlinkListener(HyperlinkListener listener) {
		bridge.removeHyperlinkListener(listener);
	}
}
