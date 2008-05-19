package org.bbop.swing;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

import org.bbop.expression.ExpressionException;

import org.apache.log4j.*;

public class XMLLayoutPanel extends JPanel implements XMLLayoutRoot {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(XMLLayoutPanel.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4373420458011869480L;

	protected XMLLayout layout = new XMLLayout();

	protected JPanel windowButtonPanel;

	protected Exception parseException;

	protected LinkedList windowList = new LinkedList();

	public XMLLayoutPanel() {
		setLayout(new BorderLayout());
		layout.setLayoutRoot(this);
	}

	public void setIsLayoutRoot(boolean isLayoutRoot) {
		if (isLayoutRoot) {
			windowButtonPanel = new JPanel();
			TitledBorder titledBorder = new TitledBorder(
					"Minimized Application Windows");
			if (layout.getDefaultFont() != null)
				titledBorder.setTitleFont(layout.getDefaultFont());
			windowButtonPanel.setBorder(titledBorder);
		}
	}

	public void minimizeWindow(final Window window) {
		if (windowButtonPanel != null) {
			window.setVisible(false);
			String title = null;
			if (window instanceof Dialog) {
				title = ((Dialog) window).getTitle();
			} else if (window instanceof Frame) {
				title = ((Frame) window).getTitle();
			}
			if (title == null || title.length() == 0)
				title = "Window";
			final JButton maximizeButton = new JButton(title);
			if (layout.getDefaultTabColor() != null)
				maximizeButton.setBackground(layout.getDefaultTabColor());
			if (layout.getDefaultFont() != null)
				maximizeButton.setFont(layout.getDefaultFont());
			windowButtonPanel.add(maximizeButton);
			maximizeButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					maximizeWindow(window, maximizeButton);
				}
			});
			if (windowList.size() == 0) {
				add(windowButtonPanel, "South");
				validate();
			}
			windowList.add(window);
		} else {
			window.dispose();
		}
	}

	protected void maximizeWindow(Window window, JButton maximizeButton) {
		window.setVisible(true);
		windowButtonPanel.remove(maximizeButton);
		windowList.remove(window);
		if (windowList.size() == 0) {
			remove(windowButtonPanel);
		}
	}

	public void guiupdate() throws ExpressionException {
		// do nothing
	}

	public boolean reload() {
		try {
			if (windowButtonPanel != null)
				windowButtonPanel.setBackground(layout.getDefaultBGColor());
			windowList.clear();
			windowButtonPanel.removeAll();
			Iterator it = layout.windows.iterator();
			while (it.hasNext()) {
				XMLLayout.WindowElement window = (XMLLayout.WindowElement) it
						.next();
				window.clear();
			}
			removeAll();
			layout.cached = false;
			layout.resolver.startParseNotify();
			if (layout.getRoot().getComponents().size() > 0)
				add((Component) layout.getRoot().getComponents().get(0),
						"Center");
			/*
			it = layout.windows.iterator();
			while (it.hasNext()) {
				XMLLayout.WindowElement window = (XMLLayout.WindowElement) it
						.next();
				// window.getComponent();
				if (window.getComponents().size() > 0)
					guiupdateAll((XMLLayoutComponent) window.getComponents()
							.get(0));
			}
			*/

			layout.resolver.endParseNotify();
			parseException = null;
			validate();
			repaint();
			return true;
		} catch (Exception ex) {
			ex.printStackTrace();
			parseException = ex;
			return false;
		}
	}

	public Exception getParseException() {
		return parseException;
	}

	public boolean setXMLLayout(String xmllayout) {
		layout.setLayout(xmllayout);
		return setXMLLayout(layout);
	}

	public void setDefaultTabColor(Color defaultTabColor) {
		layout.setDefaultTabColor(defaultTabColor);
	}

	public void setDefaultBGColor(Color defaultBGColor) {
		layout.setDefaultBGColor(defaultBGColor);
	}

	public void setDefaultFont(Font defaultFont) {
		layout.setDefaultFont(defaultFont);
	}

	public void setComponentNameResolver(ComponentNameResolver resolver) {
		layout.setComponentNameResolver(resolver);
	}

	public boolean setXMLLayout(XMLLayout layout) {
		this.layout = layout;
		layout.setLayoutRoot(this);
		return reload();
	}

	public XMLLayout getXMLLayout() {
		return layout;
	}
}
