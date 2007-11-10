package org.bbop.swing;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.HyperlinkEvent.EventType;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.AttributeSet;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;

public class LabelMouseHyperlinkBridge implements MouseListener,
		MouseMotionListener {

	protected JLabel label;

	protected Collection<HyperlinkListener> hyperlinkListeners = new ArrayList<HyperlinkListener>();

	protected Collection<StringLinkListener> stringListeners = new ArrayList<StringLinkListener>();

	protected MultiMap<String, ActionListener> actionMap = new MultiHashMap<String, ActionListener>();

	public LabelMouseHyperlinkBridge(JLabel label) {
		this.label = label;
	}

	protected static String getURL(JLabel lookAtMe, MouseEvent e) {
		View view = BasicHTML.createHTMLView(lookAtMe, lookAtMe.getText());
		if (view != null) {
			int pos = view.viewToModel(e.getX(), e.getY(), SwingUtil
					.getTextRectangle(lookAtMe));
			HTMLDocument d = (HTMLDocument) view.getDocument();
			Element emt = d.getCharacterElement(pos);
			AttributeSet a = emt.getAttributes();
			AttributeSet anchor = (AttributeSet) a.getAttribute(HTML.Tag.A);
			if (anchor != null) {
				return (String) anchor.getAttribute(HTML.Attribute.HREF);
			}
		}
		return null;
	}

	public void addStringLinkListener(StringLinkListener listener) {
		stringListeners.add(listener);
	}

	public void removeStringLinkListener(StringLinkListener listener) {
		stringListeners.remove(listener);
	}

	protected void fireStringlinkListener(String href) {
		for (StringLinkListener listener : stringListeners)
			listener.link(href);
	}

	public void addHyperLinkListener(HyperlinkListener listener) {
		hyperlinkListeners.add(listener);
	}

	public void removeHyperlinkListener(HyperlinkListener listener) {
		hyperlinkListeners.remove(listener);
	}

	protected void fireHyperlinkListener(HyperlinkEvent e) {
		for (HyperlinkListener listener : hyperlinkListeners)
			listener.hyperlinkUpdate(e);
	}

	public void mouseClicked(MouseEvent e) {
		String url = getURL(label, e);
		if (url != null) {
			fireStringlinkListener(url);
			try {
				fireHyperlinkListener(new HyperlinkEvent(e.getSource(),
						EventType.ACTIVATED, new URL(url)));
			} catch (MalformedURLException e1) {
			}
			fireActionListener(new ActionEvent(e.getSource(),
					ActionEvent.ACTION_PERFORMED, url));
		}
	}

	protected void fireActionListener(ActionEvent event) {
		Collection<ActionListener> listeners = actionMap.get(event
				.getActionCommand());
		for (ActionListener listener : listeners) {
			listener.actionPerformed(event);
		}
	}

	public void mouseEntered(MouseEvent e) {
		setCursor(e);
	}

	public void mouseExited(MouseEvent e) {
		setCursor(e);
	}

	public void mousePressed(MouseEvent e) {
	}

	public void mouseReleased(MouseEvent e) {
	}

	public void mouseDragged(MouseEvent e) {
	}

	public void mouseMoved(MouseEvent e) {
		setCursor(e);
	}

	protected void setCursor(MouseEvent e) {
		if (getURL(label, e) != null)
			label.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		else
			label.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
	}

	public void add(String command, ActionListener listener) {
		actionMap.add(command, listener);
	}

	public void remove(String command, ActionListener listener) {
		actionMap.add(command, listener);
	}

}
