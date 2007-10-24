package org.oboedit.gui.widget;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.text.html.*;

public class MiniBrowser extends JDialog {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Vector history;
	protected JEditorPane htmlPane;

	protected int historyPos;

	protected JPopupMenu rightClickMenu;
	protected JMenuItem backItem;
	protected JMenuItem forwardItem;
	protected JMenuItem reloadItem;

	protected JProgressBar statusField = new JProgressBar();

	protected HyperlinkListener linkListener = new HyperlinkListener() {
		public void hyperlinkUpdate(HyperlinkEvent e) {
			if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
				JEditorPane pane = (JEditorPane) e.getSource();
				if (e instanceof HTMLFrameHyperlinkEvent) {
					HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent) e;
					HTMLDocument doc = (HTMLDocument) pane.getDocument();
					doc.processHTMLFrameHyperlinkEvent(evt);
				} else {
					try {
						if (e.getURL() != null)
							load(e.getURL().toString());
					} catch (Throwable t) {
						t.printStackTrace();
					}
				}
			}
		}
	};

	protected void formatRightClickMenu() {
		forwardItem.setEnabled(canForward());
		backItem.setEnabled(canBack());
		reloadItem.setEnabled(false);
	}

	protected void setString(final String val) {
		Runnable r = new Runnable() {
			public void run() {
				statusField.setString(val);
			}
		};
		try {
			SwingUtilities.invokeLater(r);
		} catch (Exception ex) {
		}
	}

	public MiniBrowser(JFrame frame) {
		super(frame);
		statusField.setStringPainted(true);
		statusField.setBorderPainted(true);
		// statusField.setBorder(null);
		history = new Vector();
		htmlPane = new JEditorPane();
		htmlPane.setEditable(false);
		htmlPane.addHyperlinkListener(linkListener);

		htmlPane.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (SwingUtilities.isRightMouseButton(e)) {
					formatRightClickMenu();
					rightClickMenu.show(htmlPane, e.getX(), e.getY());
				}
			}
		});
		htmlPane.setContentType("text/html");

		rightClickMenu = new JPopupMenu("Nav Menu");
		backItem = new JMenuItem("Back");
		forwardItem = new JMenuItem("Forward");
		reloadItem = new JMenuItem("Reload");
		rightClickMenu.add(backItem);
		rightClickMenu.add(forwardItem);
		rightClickMenu.addSeparator();
		rightClickMenu.add(reloadItem);

		backItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				back();
			}
		});
		forwardItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				forward();
			}
		});
		reloadItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				reload();
			}
		});

		historyPos = 0;
		JScrollPane scrollPane = new JScrollPane(htmlPane,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(scrollPane, "Center");
		// panel.add(statusField, "South");
		setContentPane(panel);
	}

	public void setMenuFont(Font font) {
		rightClickMenu.setFont(font);
		forwardItem.setFont(font);
		backItem.setFont(font);
		reloadItem.setFont(font);
	}

	public void load(String url) {
		historyPos = history.size();
		history.add(url);
		try {
			htmlPane.setPage(url);
		} catch (Exception e) {
			System.err.println("could not load " + url);
		}
	}

	protected void loadAtPos() {
		String url = (String) history.get(historyPos);
		try {
			htmlPane.setPage(url);
			String title = (String) htmlPane.getDocument().getProperty(
					Document.TitleProperty);
			setTitle(title);
			htmlPane.validate();
		} catch (Exception e) {
			System.err.println("could not load " + url);
		}
	}

	public void reload() {
		htmlPane.updateUI();
	}

	public void back() {
		historyPos--;
		loadAtPos();
	}

	public boolean canForward() {
		return historyPos < history.size() - 1;
	}

	public boolean canBack() {
		return historyPos > 0;
	}

	public void forward() {
		historyPos++;
		loadAtPos();
	}
}
