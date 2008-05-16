package org.oboedit.gui.widget;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeListener;
import java.net.URL;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.text.BadLocationException;

import org.bbop.swing.*;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.obo.util.HTMLUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.Preferences;
import org.oboedit.verify.*;

import org.apache.log4j.*;

public class CheckWarningComponent extends JEditorPane {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CheckWarningComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected List warnings = new LinkedList();

	protected boolean showFatal = true;

	protected boolean showWarnings = true;

	protected boolean separateWarningErrorSections = true;

	protected boolean showTermNames = true;

	protected String header;

	protected String footer;

	protected boolean allowRerun = false;

	protected OBOSession session;

	protected IdentifiedObject currentObject;

	protected byte condition;

	protected boolean hyperlinksEnabled = false;

	protected static URL errorIconURL = Preferences
			.getLibraryIconURL("error.gif");

	protected static URL warningIconURL = Preferences
			.getLibraryIconURL("warning.gif");

	protected static URL quickFixIconURL = Preferences
			.getLibraryIconURL("quickfix.gif");

	protected class ActionWrapper extends AbstractAction {
		protected QuickFix action;

		public ActionWrapper(QuickFix action) {
			super(action.getDesc());
			this.action = action;
		}

		public void actionPerformed(ActionEvent e) {
			if (action instanceof ImmediateQuickFix) {
				((ImmediateQuickFix) action).run();
			} else if (action instanceof HistoryQuickFix) {
				SessionManager.getManager().apply(
						((HistoryQuickFix) action).getItem());
			}
			CheckWarning warning = (CheckWarning) action.getWarning();
			boolean globalReload = action.getLevel().equals(
					QuickFix.ReloadLevel.GLOBAL);
			if (globalReload)
				redoCheck();
			else
				redoCheck(warning.getSource());
		}
	}

	protected HyperlinkListener hyperlinkListener = new HyperlinkListener() {
		public void hyperlinkUpdate(HyperlinkEvent e) {
			if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
				if (e.getURL().getPath().startsWith("__warningindex")) {
					String path = e.getURL().getPath();
					int index = Integer.parseInt(path.substring(14, path
							.length()));
					CheckWarning warning = (CheckWarning) warnings.get(index);
					JPopupMenu menu = new JPopupMenu();
					Iterator<QuickFix> it = warning.getFixes().iterator();
					while (it.hasNext()) {
						JMenuItem item = menu.add(new ActionWrapper(it.next()));
					}
					Rectangle pos;
					try {
						pos = modelToView(e.getSourceElement().getStartOffset());
						menu.show(CheckWarningComponent.this, (int) pos.getX(),
								(int) pos.getY());
					} catch (BadLocationException e1) {
					}
				} else
					selectTerm(e.getURL(), SessionManager.getManager()
							.getSession());
			}
		}
	};

	protected boolean locked = false;

	public CheckWarningComponent() {
		super("text/html", "<html></html>");
		setOpaque(false);
		setEditable(false);
	}

	public void setRerunInfo(OBOSession session,
			IdentifiedObject currentObject, byte condition) {
		if (locked)
			return;
		allowRerun = true;
		this.session = session;
		this.currentObject = currentObject;
		this.condition = condition;
	}

	public void redoCheck() {
		Thread thread = new Thread() {
			@Override
			public void run() {
				VerificationManager.getManager().runChecks(session,
						currentObject, condition);
			}
		};
		thread.start();
	}

	public void redoCheck(final Check source) {
		Thread thread = new Thread() {
			@Override
			public void run() {
				locked = true;
				Collection oldWarnings = new LinkedList(warnings);
				Iterator it = warnings.iterator();
				while (it.hasNext()) {
					CheckWarning warning = (CheckWarning) it.next();
					if (warning.getSource().equals(source)) {
						it.remove();
					}
				}

				Collection newWarnings = VerificationManager.getManager()
						.runCheck(source, session, currentObject, condition);
				warnings.addAll(newWarnings);
				logger.error("oldWarnings = " + oldWarnings.size());
				logger.error("warnings = " + warnings.size());
				logger.error("newWarnings = " + newWarnings.size());

				Runnable r = new Runnable() {
					public void run() {
						updateGUI();
						locked = false;
					}
				};
				SwingUtilities.invokeLater(r);
			}
		};
		thread.start();
	}

	public void setHyperlinksEnabled(boolean enabled) {
		if (hyperlinksEnabled != enabled) {
			this.hyperlinksEnabled = enabled;
			if (enabled)
				addHyperlinkListener(hyperlinkListener);
			else
				removeHyperlinkListener(hyperlinkListener);
		}
	}

	public void setWarnings(Collection warnings) {
		setWarnings(warnings, null, null, true, true, true);
	}

	public void setWarnings(Collection warnings, String header, String footer,
			boolean showFatal, boolean showWarnings, boolean showTermNames) {
		if (locked)
			return;
		if (warnings == null)
		    return;
		this.warnings = new ArrayList(warnings);
		this.header = header;
		this.footer = footer;
		this.showFatal = showFatal;
		this.showWarnings = showWarnings;
		this.showTermNames = showTermNames;
		updateGUI();
	}

	public static final int YES_NO_OPTION = 1;

	public static final int OK_OPTION = 2;

	public static final int NO_VALUE = 0;

	public static final int YES_VALUE = 1;

	public int showDialog(JFrame parent, String title, int type) {
		final JDialog dialog = new JDialog(parent, title, true);
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		JPanel buttonPanel = new JPanel();
		buttonPanel.setOpaque(false);
		mainPanel.add(new JScrollPane(this,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), "Center");
		mainPanel.add(buttonPanel, "South");
		dialog.setContentPane(mainPanel);

		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));

		final int[] output = new int[1];
		if (type == YES_NO_OPTION)
			output[0] = NO_VALUE;
		else if (type == OK_OPTION)
			output[0] = YES_VALUE;

		ActionListener okListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				output[0] = YES_VALUE;
				dialog.dispose();
			}
		};
		ActionListener cancelListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				output[0] = NO_VALUE;
				dialog.dispose();
			}
		};

		JButton yesButton = new JButton((type == OK_OPTION ? "Ok" : "Proceed"));
		JButton noButton = new JButton("Cancel");

		yesButton.addActionListener(okListener);
		noButton.addActionListener(cancelListener);

		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(yesButton);
		if (type == YES_NO_OPTION) {
			buttonPanel.add(Box.createHorizontalStrut(10));
			buttonPanel.add(noButton);
		}
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.setBorder(new EmptyBorder(5, 5, 5, 5));

		dialog.pack();
		SwingUtil.center(dialog);
		dialog.show();
		return output[0];
	}

	public void setShowFatal(boolean showFatal) {
		this.showFatal = showFatal;
		updateGUI();
	}

	public void setShowWarnings(boolean showWarnings) {
		this.showWarnings = showWarnings;
		updateGUI();
	}

	public void setShowTermNames(boolean showTermNames) {
		this.showTermNames = showTermNames;
		updateGUI();
	}

	protected Comparator warningSorter = new Comparator() {
		public int compare(Object a, Object b) {
			CheckWarning ca = (CheckWarning) a;
			CheckWarning cb = (CheckWarning) b;
			if (ca.isFatal() && cb.isFatal())
				return 0;
			else if (ca.isFatal())
				return -1;
			else
				return 1;
		}
	};

	protected static Comparator<IdentifiedObject> termSorter = new Comparator<IdentifiedObject>() {
		public int compare(IdentifiedObject ioa, IdentifiedObject iob) {
			if (ioa == null && iob == null)
				return 0;
			if (ioa == null)
				return -1;
			if (iob == null)
				return 1;

			String namea = ioa.getName();
			String nameb = iob.getName();
			if (namea == null && nameb == null)
			    return 0;
			if (namea == null)
			    return -1;
			if (nameb == null)
			    return 1;

			return ioa.getName().compareToIgnoreCase(iob.getName());
		}
	};

	protected static void generateHTMLList(StringBuffer html, Collection c,
			List warnings, IdentifiedObject io, boolean showTermNames,
			boolean showWarnings, boolean showErrors,
			boolean deleteShownWarnings, boolean allowRerun) {
		int errorCount = 0;
		int warningCount = 0;
		if (showWarnings && showErrors) {
			errorCount = VerificationManager.countFatal(c);
			warningCount = c.size() - errorCount;
		} else if (showErrors) {
			errorCount = VerificationManager.countFatal(c);
		} else if (showWarnings) {
			warningCount = c.size() - VerificationManager.countFatal(c);
		}
		if (errorCount == 0 && warningCount == 0)
			return;

		if (showTermNames) {
			StringBuffer warningMessage = new StringBuffer();
			if (io == null)
				warningMessage.append("The entire ontology generated ");
			else
				warningMessage
						.append("<a href='file:" + io.getID() + "'>"
								+ io.getName() + " (" + io.getID()
								+ ")</a> generated ");

			if (errorCount > 0)
				warningMessage.append(errorCount + " error"
						+ (errorCount != 1 ? "s" : ""));
			if (errorCount > 0 && warningCount > 0)
				warningMessage.append(" and ");
			if (warningCount > 0)
				warningMessage.append(warningCount + " warning"
						+ (warningCount != 1 ? "s" : ""));
			warningMessage.append(".");
			html.append("<li>" + warningMessage);
			html.append("<ul style='list-style-type:none;'>");
		}
		Iterator it2 = c.iterator();
		while (it2.hasNext()) {
			CheckWarning warning = (CheckWarning) it2.next();
			if (!showWarnings && !warning.isFatal())
				continue;
			if (!showErrors && warning.isFatal())
				continue;
			if (deleteShownWarnings)
				it2.remove();
			URL iconURL = (warning.isFatal() ? errorIconURL : warningIconURL);

			html.append("<li>");
			html.append("<img src=\"" + iconURL + "\"> ");
			html.append("&nbsp;");
			html.append(warning.getMessage());
			if (warning.getFixes().size() > 0 && allowRerun)
				html.append(" <a href='file:__warningindex"
						+ warnings.indexOf(warning) + "'><img src=\""
						+ quickFixIconURL
						+ "\" border=\"0\" alt=\"Quick fix\"></a>");
		}
		if (showTermNames) {
			html.append("</ul>");
		}
	}

	public void selectTerm(URL url, OBOSession session) {
		String id = url.getPath();
		IdentifiableObject t = session.getObject(id);
		if (t instanceof LinkedObject) {
			Collection<LinkedObject> terms = new LinkedList<LinkedObject>();
			terms.add((LinkedObject) t);
			SelectionManager.selectTerms(CheckWarningComponent.this, terms);
		}
	}

	public static String getHTML(List<CheckWarning> warnings, String header,
			String footer, boolean separateWarningErrorSections,
			boolean allowRerun, boolean showTermNames, boolean hyperlinksEnabled) {
		MultiMap<IdentifiedObject, CheckWarning> warningMap = new MultiHashMap<IdentifiedObject, CheckWarning>();
		int globalErrors = 0;
		int globalWarnings = 0;
		for (CheckWarning cw : warnings) {
			warningMap.add(cw.getPath().getObject(), cw);
			if (cw.isFatal())
				globalErrors++;
			else
				globalWarnings++;
		}
		return getHTML(warningMap, warnings, header, footer, globalWarnings,
				globalErrors, separateWarningErrorSections, allowRerun,
				showTermNames, hyperlinksEnabled);
	}

	public static String getHTML(
			Map<IdentifiedObject, Collection<CheckWarning>> warningMap,
			List warnings, String header, String footer,
			int globalWarningCount, int globalErrorCount,
			boolean separateWarningErrorSections, boolean allowRerun,
			boolean showTermNames, boolean hyperlinksEnabled) {
		StringBuffer html = new StringBuffer();
		if (globalWarningCount > 0 || globalErrorCount > 0) {
			if (header != null)
				html.append(header);

			java.util.List<IdentifiedObject> termList = new ArrayList<IdentifiedObject>(
					warningMap.keySet());
			Collections.sort(termList, termSorter);

			// if we need separate sections for warnings and errors, show the
			// errors first, and delete each error as it's shown
			if (separateWarningErrorSections) {
				if (globalErrorCount > 0) {
					html.append("<p style='color: red;'>" + globalErrorCount
							+ " fatal error"
							+ (globalErrorCount != 1 ? "s" : "") + ":</p>\n");
					html.append("<ul style='list-style-type:none;'>");
					Iterator it = termList.iterator();
					while (it.hasNext()) {
						IdentifiedObject io = (IdentifiedObject) it.next();
						Collection c = (Collection) warningMap.get(io);
						generateHTMLList(html, c, warnings, io, showTermNames
								|| warningMap.size() > 1, false, true, true,
								allowRerun);
					}
					html.append("</ul>");
					if (globalWarningCount > 0) {
						html.append("<hr>");
					}
				}
				if (globalWarningCount > 0) {
					html.append("<p>" + globalWarningCount
							+ " non-critical warning"
							+ (globalWarningCount != 1 ? "s" : "") + ":</p>\n");
				}
			}

			html.append("<ul style='list-style-type:none;'>");
			Iterator it = termList.iterator();
			while (it.hasNext()) {
				IdentifiedObject io = (IdentifiedObject) it.next();
				Collection c = (Collection) warningMap.get(io);
				generateHTMLList(html, c, warnings, io, showTermNames
						|| warningMap.size() > 1, true, true, false, allowRerun);
			}
			html.append("</ul>");
			if (footer != null)
				html.append(footer);
		} else {
			html.append("No problems found.");
		}
		String fontStyle = SwingUtil.getHTMLFontStyle(Preferences
				.getPreferences().getFont());
		String out = "<html><head><style type='text/css'>\nli {"
				+ fontStyle
				+ "}\np {font-weight: bold;\n"
				+ fontStyle
				+ "}</style></head><body bgcolor='#"
				+ ColorUtil.getHTMLCode(Preferences.getPreferences()
						.getBackgroundColor()) + "'>" + html.toString()
				+ "</body></html>";
		if (!hyperlinksEnabled)
			out = HTMLUtil.removeHyperlinks(out);
		return out;
	}

	protected void initComponent(
			Map<IdentifiedObject, Collection<CheckWarning>> warningMap,
			List warnings, String header, String footer,
			int globalWarningCount, int globalErrorCount,
			boolean separateWarningErrorSections, boolean allowRerun,
			boolean showTermNames, boolean hyperlinksEnabled) {
		String out = getHTML(warningMap, warnings, header, footer,
				globalWarningCount, globalErrorCount,
				separateWarningErrorSections, allowRerun, showTermNames,
				hyperlinksEnabled);
		setText(out);
		setCaretPosition(0);
	}

	protected void updateGUI() {
		Map warningMap = new LinkedHashMap();

		int warningCount = 0;
		int fatalCount = 0;
		Iterator it = warnings.iterator();
//		logger.error("updategui warnings = " + warnings.size());
		while (it.hasNext()) {
			CheckWarning warning = (CheckWarning) it.next();
			if (warning.isFatal() && !showFatal) {
				continue;
			}
			if (!warning.isFatal() && !showWarnings) {
				continue;
			}
			java.util.List c = (java.util.List) warningMap.get(warning
					.getObject());
			if (c == null) {
				c = new ArrayList();
				warningMap.put(warning.getObject(), c);
			}
			int pos = Collections.binarySearch(c, warning, warningSorter);
			if (pos < 0) {
				c.add(-(pos + 1), warning);
				if (warning.isFatal())
					fatalCount++;
				else
					warningCount++;
			}
		}

		// JComponent warningComponent = getComponent(warningMap);
		initComponent(warningMap, warnings, header, footer, warningCount,
				fatalCount, separateWarningErrorSections, allowRerun,
				showTermNames, hyperlinksEnabled);
		// setPreferredSize(new Dimension(640, 640));

	}
}
