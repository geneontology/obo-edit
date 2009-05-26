package org.oboedit.gui.components;

import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.framework.IOManager;
import org.bbop.swing.*;
import org.bbop.util.*;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.SessionHistoryList;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.URL;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;

import org.apache.log4j.*;

public class HistoryBrowser extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HistoryBrowser.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 2066665063180432495L;

	public final static int TAB_SIZE = 3;

	Vector histories;
	JTree sessionList;
	HistoryTreeModel model;
	HistoryListener historyListener;
	RootChangeListener rootListener;
	TreeSelectionListener selectListener;
	JButton saveButton = new JButton("Save History");

	Component lastGUI = null;
	HTMLHistoryDumper htmlDumper = new HTMLHistoryDumper();

	protected class HistoryRenderer extends DefaultTreeCellRenderer {
		/**
		 * 
		 */
		private static final long serialVersionUID = -7969060810210949236L;

		@Override
		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean sel, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			Component c = super.getTreeCellRendererComponent(tree, value, sel,
					expanded, leaf, row, hasFocus);

			if (SessionManager.getManager().getSession() != null
					&& (value instanceof HistoryList)
					&& value.equals(SessionManager.getManager().getSession()
							.getCurrentHistory())) {
				setForeground(Color.blue);
				setText("Current session");
			} else {
				setForeground(Color.black);
				setText(value.toString());
			}
			return c;
		}
	}

	public HistoryBrowser(String id) {
		super(id);
		sessionList = new JTree();
		sessionList.setFont(new Font("Dialog", 0, 10));
		sessionList.putClientProperty("JTree.lineStyle", "Angled");
		sessionList.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		// htmlDumper.setGenerateLink(false);
		DefaultTreeCellRenderer rend = new HistoryRenderer();
		rend.setLeafIcon(null);
		rend.setClosedIcon(null);
		rend.setOpenIcon(null);
		sessionList.setCellRenderer(rend);
	}

	@Override
	public String getName() {
		return "History browser";
	}

	@Override
	public void init() {
		histories = new Vector();
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		layoutPlugin();
		attachListeners();
	}

	protected void saveHistory() {
		HistoryList historyList = (HistoryList) sessionList.getSelectionPath()
				.getLastPathComponent();

		try {
			DataAdapterRegistry registry = IOManager.getManager()
					.getAdapterRegistry();

			GraphicalAdapterChooser gac = new GraphicalAdapterChooser(registry,
					OBOAdapter.WRITE_HISTORY, GUIManager.getManager()
							.getScreenLockQueue(), GUIManager.getManager()
							.getFrame(), Preferences.getPreferences()
							.getUseModalProgressMonitors(), historyList);
			gac.setHistoryPath(IOManager.getManager().getHistoryFilePath());
			gac.showDialog("Save history", null);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	protected void printHistory(PrintWriter writer, int indent, Object obj) {
		String str = StringUtil.repeat(" ", indent);
		writer.println(str + "* " + obj.toString());
		int childCount = model.getChildCount(obj);
		for (int i = 0; i < childCount; i++) {
			Object child = model.getChild(obj, i);
			printHistory(writer, indent + TAB_SIZE, child);
		}
	}

	private void attachListeners() {

		rootListener = new RootChangeListener() {
			public void changeRoot(RootChangeEvent e) {
				layoutPlugin();
				sessionList.expandPath(model.getActiveHistoryPath());
			}
		};

		historyListener = new HistoryListener() {
			public void reload() {
				model.reloadActiveHistory();
				sessionList.expandPath(model.getActiveHistoryPath());
			}

			public void applied(HistoryAppliedEvent event) {
				reload();
			}

			public void reversed(HistoryAppliedEvent event) {
				reload();
			}
		};

		selectListener = new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent e) {
				updateGUI();
			}
		};

		sessionList.addTreeSelectionListener(selectListener);
		SessionManager.getManager().addHistoryListener(historyListener);
		SessionManager.getManager().addRootChangeListener(rootListener);
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveHistory();
			}
		});
	}

	protected void updateGUI() {
		if (lastGUI != null)
			remove(lastGUI);
		if (sessionList.getSelectionPath() != null) {
			lastGUI = getComponentForItem(sessionList.getSelectionPath());
			add(lastGUI, "South");
		} else {
			lastGUI = null;
		}
		validate();
		repaint();
	}

	protected Component getComponentForItem(TreePath path) {
		Object object = path.getLastPathComponent();

		// if (object instanceof OBOSession) {
		if (object instanceof SessionHistoryList) {
			JPanel panel = new JPanel();
			panel.setBackground(Color.white);
			panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
			JLabel label = new JLabel();
			JLabel commentLabel = new JLabel("Comment");
			JTextArea descTextArea = new JTextArea(5, 30);
			descTextArea.setEditable(false);
			descTextArea.setWrapStyleWord(true);
			descTextArea.setLineWrap(true);
			JScrollPane descPane = new JScrollPane(descTextArea,
					JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
					JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

			label.setText(((SessionHistoryList) object).getTitle());
			descTextArea.setText(((SessionHistoryList) object).getComment());
			label.setAlignmentX(LEFT_ALIGNMENT);
			commentLabel.setAlignmentX(LEFT_ALIGNMENT);
			descPane.setAlignmentX(LEFT_ALIGNMENT);

			JPanel buttonBox = new JPanel();
			buttonBox.setLayout(new BoxLayout(buttonBox, BoxLayout.X_AXIS));
			buttonBox.add(Box.createHorizontalGlue());
			buttonBox.add(saveButton);
			buttonBox.add(Box.createHorizontalGlue());
			buttonBox.setAlignmentX(LEFT_ALIGNMENT);
			buttonBox.setOpaque(false);

			panel.add(label);
			panel.add(Box.createVerticalStrut(10));
			panel.add(commentLabel);
			panel.add(descPane);
			panel.add(buttonBox);
			return panel;
		} else if (object instanceof HistoryItem) {
			HistoryList history = null;
			while (path != null) {
				if (path.getLastPathComponent() instanceof HistoryList) {
					history = (HistoryList) path.getLastPathComponent();
				}
				path = path.getParentPath();
			}

			JEditorPane editorPane = new JEditorPane("text/html",
					"<html></html>");
			editorPane.setEditable(false);
			editorPane.setPreferredSize(new Dimension(300, 100));
			editorPane.addHyperlinkListener(new HyperlinkListener() {
				public void hyperlinkUpdate(HyperlinkEvent e) {
					if (e.getEventType().equals(
							HyperlinkEvent.EventType.ACTIVATED)) {
						selectTerm(e.getURL(), SessionManager.getManager()
								.getSession());
					}
				}
			});

			JScrollPane descPane = new JScrollPane(editorPane,
					JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
					JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			editorPane.setText(htmlDumper.getItemDesc((HistoryItem) object,
					SessionManager.getManager().getSession()));
			return descPane;
		} else {
			JEditorPane editorPane = new JEditorPane("text/html",
					"<html></html>");
			editorPane.setEditable(false);
			editorPane.setPreferredSize(new Dimension(300, 100));

			JScrollPane descPane = new JScrollPane(editorPane,
					JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
					JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			editorPane.setText(object.toString());
			return descPane;
		}
	}

	public void selectTerm(URL url, OBOSession history) {
		String id = url.getPath();
		OBOClass t = (OBOClass) history.getObject(id);
		SelectionManager.selectTerm(this, t);
	}

	@Override
	public void cleanup() {
		SessionManager.getManager().removeHistoryListener(historyListener);
		SessionManager.getManager().removeRootChangeListener(rootListener);
	}

	protected void layoutPlugin() {
		removeAll();
		setLayout(new BorderLayout());
		/*
		 * try { DEDataAdapterI adapter = controller.getLastAdapter(); histories =
		 * adapter.getHistories(); } catch (Exception e) { JLabel label = new
		 * JLabel("Note: ancient history not supported"); add(label, "North"); }
		 */
		model = new HistoryTreeModel(SessionManager.getManager().getSession());
		sessionList.setModel(model);
		JScrollPane pane = new JScrollPane(sessionList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		SwingUtil.setPreferredWidth(pane, 200);
		add(pane, "Center");
		validate();
	}
}
