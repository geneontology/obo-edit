package org.oboedit.gui.components;

import java.awt.Color;
import java.awt.Component;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import org.bbop.expression.ExpressionException;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.IOEvent;
import org.bbop.framework.IOListener;
import org.bbop.framework.IOManager;
import org.bbop.swing.SwingUtil;
import org.bbop.swing.XMLLayoutUtil;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.postcomp.PostcompUtil;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.AbstractXMLOBOEditComponent;
import org.oboedit.gui.DefaultErrorDecoratorFactory;
import org.oboedit.gui.ErrorDecorator;
import org.oboedit.gui.ErrorDecoratorFactory;
import org.oboedit.gui.OBOTextEditComponent;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.RootTextEditComponent;
import org.oboedit.gui.SelectionDrivenComponent;
import org.oboedit.gui.TextComponentNameResolver;
import org.oboedit.gui.event.IncrementalVerificationEvent;
import org.oboedit.gui.event.IncrementalVerificationListener;
import org.oboedit.gui.event.PreSelectionEvent;
import org.oboedit.gui.event.PreSelectionListener;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.gui.event.TermLoadEvent;
import org.oboedit.gui.event.TermLoadListener;
import org.oboedit.gui.factory.VerificationManagerFactory;
import org.oboedit.gui.widget.CheckWarningComponent;
import org.oboedit.script.TextEditorScriptDelegate;
import org.oboedit.util.GUIUtil;
import org.oboedit.verify.CheckWarning;

import org.apache.log4j.*;

public class TextEditor extends AbstractXMLOBOEditComponent implements
RootTextEditComponent, SelectionDrivenComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextEditor.class);

	protected Icon warningIcon = Preferences.loadLibraryIcon("warning_icon.gif");

	protected Icon errorIcon = Preferences.loadLibraryIcon("error_icon.gif");

	protected IdentifiedObject currentObject;

	protected MultiMap<FieldPath, CheckWarning> warningMap = new MultiHashMap<FieldPath, CheckWarning>();

	protected Collection<TermLoadListener> loadListeners = new LinkedList<TermLoadListener>();

	protected List<FieldPath> dirtyPaths = new LinkedList<FieldPath>();

	protected MultiMap<FieldPathSpec, JComponent> fieldToComponentMap = new MultiHashMap<FieldPathSpec, JComponent>();

	// protected MultiMap<JComponent, FieldPathSpec> componentToFieldMap = new
	// MultiHashMap<JComponent, FieldPathSpec>();

	protected Map<JComponent, ErrorDecorator> decoratorMap = new HashMap<JComponent, ErrorDecorator>();

	protected ErrorDecoratorFactory errorDecoratorFactory = new DefaultErrorDecoratorFactory(
			true);

	protected ReconfigListener reconfigListener = new ReconfigListener() {

		public void configReloaded(ReconfigEvent e) {
			reload();
			setObject(getObject());
		}

	};

	private Color titlebarErrorColor = Color.red.darker();

	protected ObjectSelector selector;

	protected Timer checkTimer;

	public static final int TIMER_DELAY = 1500;  // was 1000

	protected ActionListener checkTask = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			// Could we maybe be more restrictive about doing timed text checks--only for certain events?
			doTimedTextChecks(true, false);
		}
	};

	public void setObjectSelector(ObjectSelector selector) {
		if (this.selector != null)
			selector.removeSelectionListener(selectionListener);

		this.selector = selector;

		selector.addSelectionListener(selectionListener);
	}

	public ObjectSelector getObjectSelector() {
		return selector;
	}

	// Note: if this method returns false, then this component continues
	// to run in the background even when it's not in the current layout.
	// This is the desired behavior for TextEditor because otherwise if
	// there are warning messages from the Verification Manager and you
	// click on the term name in the warning report, it brings up a fresh
	// TextEditor and loses your current edits.
	public boolean teardownWhenHidden() {
		return false;
	}

	protected void startTimer() {
		if (checkTimer == null)
			checkTimer = new Timer(TIMER_DELAY, checkTask);
		checkTimer.start();
	}

	public Map<FieldPathSpec, Collection<JComponent>> getComponentMap() {
		return fieldToComponentMap;
	}

	protected void stopTimer() {
		if (checkTimer != null)
			checkTimer.stop();
		checkTimer = null;
	}

	protected void doTimedTextChecks(boolean requireDirtyPaths, boolean onCommit) {
		if (currentObject == null)
			return;
		if (requireDirtyPaths && 
				(this.dirtyPaths.size() == 0 || !hasChanges()))
			return;
//		logger.debug("TextEditor.doTimedTextChecks: object = " + currentObject + ", requiredirty = " + requireDirtyPaths + ", onCommit = " + onCommit + ", " + dirtyPaths.size() + " dirtyPaths"); // DEL

		// Why can't we just clear dirtyPaths rather than making a new one?  (Not yet tested)
//		dirtyPaths.clear();
		dirtyPaths = new LinkedList<FieldPath>();
		IdentifiedObject clone = (IdentifiedObject) currentObject.clone();
		populateFields(clone);
		for (ErrorDecorator decorator : decoratorMap.values()) {
			decorator.clearWarnings();
		}
		// Why are these final?
		final Collection<CheckWarning> warnings = VerificationManager
		.getManager().runChecks(
				SessionManager.getManager().getSession(),
				new FieldPath(clone),
				onCommit ?
						VerificationManager.TEXT_EDIT_COMMIT :
							VerificationManager.TEXT_EDIT_THREAD);
		final MultiMap<FieldPath, CheckWarning> allWarnings = new MultiHashMap<FieldPath, CheckWarning>();
		for (CheckWarning w : warnings) {
			allWarnings.add(w.getPath(), w);
		}
		for (FieldPath path : allWarnings.keySet()) {
			displayWarnings(path, allWarnings.get(path));
		}
		setWarningMap(allWarnings);
		repaint();
	}

	protected void displayWarnings(final FieldPath path,
			final Collection<CheckWarning> warnings) {
		FieldPathSpec s = path.getSpec();
		do {
			for (JComponent comp : fieldToComponentMap.get(s)) {
				final ErrorDecorator d = decoratorMap.get(comp);
				if (d != null) {
					Runnable r = new Runnable() {
						public void run() {
							d.setWarnings(path, warnings);
						}
					};
					SwingUtilities.invokeLater(r);
				}
			}
		} while ((s = s.getParent()) != null);
	}

	protected Collection<IncrementalVerificationListener> incrementalListeners = new LinkedList<IncrementalVerificationListener>();

	public void addIncrementalVerificationListener(
			IncrementalVerificationListener listener) {
		incrementalListeners.add(listener);
	}

	public void removeIncrementalVerificationListener(
			IncrementalVerificationListener listener) {
		incrementalListeners.remove(listener);
	}

	protected void fireIncrementalVerificationEvent(
			IncrementalVerificationEvent event) {
		for (IncrementalVerificationListener listener : incrementalListeners) {
			listener.cycleComplete(event);
		}
	}

	public void addMapping(FieldPathSpec path, OBOTextEditComponent parent,
			JComponent component) {
		fieldToComponentMap.add(path, component);

		ErrorDecorator decorator = errorDecoratorFactory.install(path, parent,
				component);
		if (decorator != null) {
			decoratorMap.put(component, decorator);
		}
	}

	public void removeMapping(FieldPathSpec path, JComponent component) {
		Collection<JComponent> c = fieldToComponentMap.get(path);
		if (c != null) {
			c.remove(component);
			if (c.size() == 0)
				fieldToComponentMap.remove(path);
		}

		ErrorDecorator decorator = decoratorMap.get(component);
		if (decorator != null) {
			errorDecoratorFactory.uninstall(decorator);
		}
	}

	public void addDirtyPath(FieldPath dirty) {
		dirtyPaths.add(dirty);
	}

	public void addLoadListener(TermLoadListener listener) {
		loadListeners.add(listener);
	}

	public void removeLoadListener(TermLoadListener listener) {
		loadListeners.remove(listener);
	}

	protected void fireLoadEvent(TermLoadEvent e) {
		for (TermLoadListener listener : loadListeners) {
			listener.load(e);
		}
	}

	protected IncrementalVerificationListener listener = new IncrementalVerificationListener() {

		public void cycleComplete(IncrementalVerificationEvent e) {
			if (e.getWarnings() != null) {
				for (FieldPath path : e.getWarnings().keySet()) {
					warningMap.put(path, e.getWarnings().get(path));
				}
			}
			setWarningMap(warningMap);
		}
	};

	protected SelectionListener selectionListener = new SelectionListener() {

		public void selectionChanged(SelectionEvent e) {
//			logger.debug("TextEditor.selectionChanged: setObject " + e.getSelection().getTermSubSelection()); // DEL
			setObject(e.getSelection().getTermSubSelection());
		}
	};

	protected boolean global = false;
	protected MultiMap<FieldPath, CheckWarning> oldWarningMap;

	protected void autocommit() {
		uninstallAutocommitListener();
		commit();
		installAutocommitListener();
	}

	protected void setWarningMap(MultiMap<FieldPath, CheckWarning> warningMap) {
		boolean unchanged = ObjectUtil.equals(oldWarningMap, warningMap);
		this.oldWarningMap = warningMap;
		if (unchanged) {
			return;
		}
		if (warningMap.isEmpty()) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					ComponentManager.getManager().setLabel(TextEditor.this,
					"Text Editor");
					ComponentManager.getManager().setTitlebarTooltip(
							TextEditor.this, null);
					ComponentManager.getManager().setTitlebarColor(
							TextEditor.this, Color.black);
					errorPanel.setVisible(false);
					validate();
					repaint();
				}
			});
		}
		else {
			int warningCount = 0;
			int errorCount = 0;
			final List<CheckWarning> warnings = new ArrayList<CheckWarning>();
			for (CheckWarning cw : warningMap.singleValues()) {
				if (!cw.getPath().getObject().equals(currentObject))
					continue;
				warnings.add(cw);
				if (cw.isFatal())
					errorCount++;
				else
					warningCount++;
			}
			String color = "red";
			final Icon icon;
			if (errorCount > 0) {
				icon = errorIcon;
			} else {
				color = "#FFAA33";
				icon = warningIcon;
			}
			final StringBuffer htmlWrapperBuffer = new StringBuffer();
			final StringBuffer buffer = new StringBuffer();
			htmlWrapperBuffer.append("<html>");
			htmlWrapperBuffer.append("<font color='" + color + "'>");
			if (errorCount > 0) {
				buffer.append(errorCount + " fatal error"
						+ (errorCount != 1 ? "s" : ""));
				if (warningCount > 0)
					buffer.append(" and ");
			}
			if (warningCount > 0)
				buffer.append(warningCount + " warning"
						+ (warningCount != 1 ? "s" : ""));
			htmlWrapperBuffer.append(buffer);
			htmlWrapperBuffer.append(".");
			htmlWrapperBuffer.append("</font>");
			htmlWrapperBuffer.append(" <i>Click for more details</i>");
			htmlWrapperBuffer.append("</html>");
			final int errorCountFinal = errorCount;
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					String tooltip = CheckWarningComponent.getHTML(warnings,
							"", "", true, false, false, false);
					// Color warningColor = Color.orange;
//					Color warningColor = new Color(255, 153, 51);
					Color warningColor = Color.black;  // for now
					ComponentManager.getManager().setLabel(TextEditor.this,
							"Text Editor (" + buffer.toString() + ")");
					ComponentManager.getManager().setTitlebarTooltip(
							TextEditor.this, tooltip);
					if (errorCountFinal > 0)
						ComponentManager.getManager().setTitlebarColor(
								TextEditor.this, titlebarErrorColor);
					else
						ComponentManager.getManager().setTitlebarColor(
								TextEditor.this, warningColor);
					errorLabel.setText(htmlWrapperBuffer.toString());
					errorLabel.setIcon(icon);
					errorLabel.setToolTipText(tooltip);
					errorPanel.setVisible(true);
					validate();
					repaint();
				}
			});
		}
	}

	protected JPanel errorPanel = new JPanel();

	protected JButton errorLabel = new JButton();

	public ConfigurationPanel getConfigurationPanel() {
		return null;
	}

	public TextEditor(String id) {
		super(id);
		SwingUtil.mapAction(this,
				JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, KeyStroke
				.getKeyStroke(KeyEvent.VK_ENTER, Toolkit
						.getDefaultToolkit().getMenuShortcutKeyMask()),
						new AbstractAction("commit") {

			public void actionPerformed(ActionEvent e) {
				commit();
			}

		});
		setObjectSelector(SelectionManager.getManager());
		errorPanel.add(errorLabel);
		errorPanel.setVisible(false);
		errorPanel.setOpaque(false);
		errorLabel.setBorderPainted(false);
		errorLabel.setFocusPainted(false);
		errorLabel.setOpaque(false);
		errorLabel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				activateVerifyPerspective();
			}
		});
		try {
			setup(new TextComponentNameResolver(this) {
				@Override
				public Component resolveName(String id,
						java.util.Properties props, String xml) {
					if (id.equals("ERROR_PANEL"))
						return errorPanel;
					else
						return super.resolveName(id, props, xml);
				}
			}, ExpressionManager.getManager().createSubContext("TextEditor",
					new TextEditorScriptDelegate(this)));
		} catch (ExpressionException e1) {
			e1.printStackTrace();
		}
		// setAutocommit(Preferences.getPreferences().getAutoCommitTextEdits());
		installAutocommitListener();
	}

	@Override
	protected void installListeners() {
		addMapping(new FieldPathSpec(), this, this);
		addIncrementalVerificationListener(listener);
		Preferences.getPreferences().addReconfigListener(reconfigListener);
		startTimer();
	}

	@Override
	protected void uninstallListeners() {
		removeMapping(new FieldPathSpec(), this);
		removeIncrementalVerificationListener(listener);
		uninstallAutocommitListener();
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
		stopTimer();
	}

	protected PreSelectionListener preSelectListener = new PreSelectionListener() {
		public boolean isPreSelectOkay(PreSelectionEvent e) {
			return runChecksAndCommit();
		}
	};

	protected IOListener ioListener = new IOListener() {

		public boolean willExecuteOperation(IOEvent e) {
			return runChecksAndCommit();
		}

		public void operationExecuted(IOEvent e) {
		}
	};

	protected boolean runChecksAndCommit() {
//		logger.debug("runChecksAndCommit: object = " + currentObject + ", hasChanges = " + hasChanges()); // DEL
		if (currentObject == null || !hasChanges())
			return true;
		PostcompUtil.getNameExpression(currentObject, true);
		// Why do we have to clone it?  I guess in case the user decides not to commit?
		IdentifiedObject clone = (IdentifiedObject) currentObject.clone();
		populateFields(clone);
		Collection<CheckWarning> warnings = VerificationManager.getManager()
		.runChecks(SessionManager.getManager().getSession(),
				new FieldPath(clone),
				VerificationManager.TEXT_EDIT_COMMIT);
		boolean fatal = false;
		for (CheckWarning warning : warnings) {
			if (warning.isFatal()) {
				fatal = true;
				logger.info("Fatal warning on " + currentObject.getName() + ": " + warning);
				break;
			}
		}
		if (fatal) {
			Object[] options = { "Open Verify perspective to display errors",
					"Correct errors without changing the display",
			"Abandon pending edits" };
			Object choice = JOptionPane.showInputDialog(this,
					"There are pending text edits that "
					+ "cannot be autocommitted because of "
					+ "fatal errors. How do you wish to correct this?",
					"Fatal errors", JOptionPane.ERROR_MESSAGE, null, options,
					options[0]);
			if (ObjectUtil.equals(choice, options[2])) {
				return true;
			}

			if (ObjectUtil.equals(choice, options[0])) {
				activateVerifyPerspective();
			}
		} else {
			if (Preferences.getPreferences().getAutoCommitTextEdits()) {
				autocommit();
			} else if (Preferences.getPreferences()
					.getWarnBeforeDiscardingEdits()
					&& hasChanges()) {
				// Should we change this to "Commit these edits?"  (yes=keep, no=discard)
				// I'm worried that would confuse the experienced OBO-Editors.
				int val = JOptionPane.showConfirmDialog(GUIManager.getManager()
						.getFrame(), "There are uncommitted text edits.\n"
						+ "Discard these edits?", "Pending edits",
						JOptionPane.YES_NO_OPTION);
				return val == JOptionPane.YES_OPTION;
			}
		}
		logger.info("Committed text edit(s) to " + currentObject.getName() + ".  There " +
				(fatal ? "were" : "were no") +
		" fatal errors.");
		// Now can we release the clone?
		clone = null;
		return !fatal;
	}

	protected void activateVerifyPerspective() {
//		ComponentManager.getManager().setPerspective("verify");
		GUIComponent c = ComponentManager.getManager().getActiveComponent(
		"VERIFICATION_MANAGER:main");
		if (c == null || !((JComponent) c).isVisible()) {
			ComponentManager.getManager().showComponent(
					new VerificationManagerFactory(), null);
		}
		else {
			// This doesn't seem to bring it to the front if it's an undocked component
			ComponentManager.getManager().focusComponent(c);
			// A bit brutal, but seems to be the only way to make it visible if
			// it's undocked and hiding behind the main panel
			ComponentManager.getManager().setFloating(c, false);
		}
	}

	protected void uninstallAutocommitListener() {
		SelectionManager.getManager().removePreSelectionListener(
				preSelectListener);
		IOManager.getManager().removeIOListener(ioListener);
	}

	protected void installAutocommitListener() {
		SelectionManager.getManager()
		.addPreSelectionListener(preSelectListener);
		IOManager.getManager().addIOListener(ioListener);
	}

	public TextComponentNameResolver getMyResolver() {
		return (TextComponentNameResolver) getResolver();
	}




	@Override
	public String getDefaultLayout() {
		String defaultLayout = "<grid cols='1' rows='1' margins='5'>"
			+

			"<if eval='TextEditor.getObject() != null'>"
			+

			"<box orientation='vert'>"
			// put this back if we need it
			// + "<component id='ERROR_PANEL'/>"
			+

			"<compactgrid cols='2' yPad='3'>"
			+ "<label text='ID'/>"
			+ "<component id='ID_EDITOR' leftmargin='10'>"
			+ " <component id='id_label'/>"
			+ "</component>"
			+

			"<if eval='size(TextEditor.getObject().getSecondaryIDs()) > 0'>"
			+ "<label text='Secondary IDs'/>"
			+ "<component id='ID_EDITOR' leftmargin='10'>"
			+ " <component id='secondary_id_label'/>"
			+ "</component>"
			+ "</if>"
			+

			"<label text='Namespace'/>"
			+ "<component id='NAMESPACE_EDITOR' leftmargin='10'>"
			+ " <component id='list'/>"
			+ "</component>"
			+

			"<if eval='TermUtil.isProperty(TextEditor.getObject())'>"
			+ "<label text='Range'/>"
			+ "<component id='RANGE_EDITOR' leftmargin='10'>"
			+ " <component id='range_button'/>"
			+ "</component>"
			+ "<label text='Domain'/>"
			+ "<component id='DOMAIN_EDITOR' leftmargin='10'>"
			+ " <component id='domain_button'/>"
			+ "</component>"
			+ "</if>"
			+

			"<label text='Name'/>"
			+ "<component id='NAME_EDITOR' leftmargin='10'>"
			+ " <component id='field'/>"
			+ "</component>"
			+

			"</compactgrid>"
			+

			"<panel>"
			+ "<center>"
			+ "<panel>"
			+ "<north>"
			+ "<if eval='TermUtil.isProperty(TextEditor.getObject())'>"
			+ "<component id='PROPERTY_BOXES_EDITOR' leftmargin='10'/>"
			+ "</if>"
			+ "</north>"
			+ "<center>"
			+

			"<grid rows='2' cols='1'>"
			+

			"<tabs>"
			+ "<tab name='Definition`if (TextEditor.getObject().getDefinition().length() > 0) \" *\"; else \"\";`' selected='`TextEditor.getObject().getDefinition().length() > 0`'>"
			+ "<component id='DEFINITION_EDITOR'/>"
			+ "</tab>"
			+ "<tab name='Comment`if (TextEditor.getObject().getComment().length() > 0) \" *\"; else \"\";`' selected='`TextEditor.getObject().getComment().length() > 0`'>"
			+ "<component id='COMMENT_EDITOR'/>"
			+ "</tab>"
			+

//			"<tab name=' Cross Products`if (TermUtil.isIntersection(TextEditor.getObject())) \" *\"; else \"\";`' selected='`TermUtil.isIntersection(TextEditor.getObject())`'>"
//			+ "<if eval='!TermUtil.isProperty(TextEditor.getObject())'>"
//			+ "<component id='INTERSECTION_EDITOR'/>"
//			+ "</if>"
//			+ "</tab>"
//			+

			"<tab name=' Cross Products`if (TermUtil.isIntersection(TextEditor.getObject())) \" *\"; else \"\";`' selected='`TermUtil.isIntersection(TextEditor.getObject())`'>"
			+ "<if eval='!TermUtil.isProperty(TextEditor.getObject())'>"
			+ "<component id='CROSSPRODUCT_EDITOR'/>"
			+ "</if>"
			+ "</tab>"
			+
			
			"</tabs>"
			+

			"<tabs>"
			+ "<tab name='Dbxrefs`if (size(TextEditor.getObject().getDbxrefs()) > 0) \" *\"; else \"\";`' selected='`size(TextEditor.getObject().getDbxrefs()) > 0`'>"
			+ "<component id='DBXREF_EDITOR'/>"
			+ "</tab>"
			+

			"<tab name='Synonyms`if (size(TextEditor.getObject().getSynonyms()) > 0) \" *\"; else \"\";`' selected='`size(TextEditor.getObject().getSynonyms()) > 0`'>"
			+ "<component id='SYNONYM_EDITOR'/>"
			+ "</tab>"
			+

			"<tab name='Subsets`if (size(TextEditor.getObject().getSubsets()) > 0) \" *\"; else \"\";`' selected='`size(TextEditor.getObject().getSubsets()) > 0`'>"
			+ "<component id='SUBSET_EDITOR'/>"
			+ "</tab>"
			+

			"</tabs>"
			+

			"</grid>"
			+ "</center>"
			+ "</panel>"
			+

			"</center>"
			+ "<south>"
			+ "<if eval='!GUI.autoCommitTextEdits()'>"
			+ "<box orientation='horz' topmargin='5'>"
			+ "<glue/><component id='TEXT_COMMIT'/>"
			+ "<spacer orientation='horz' size='30'/>"
			+ "<component id='TEXT_REVERT'/><glue/>"
			+ "</box>"
			+ "</if>"
			+ "</south>"
			+ "</panel>"
			+ "</box>"
			+ "</if>"
			+

			"<if eval='TextEditor.getObject() == null'>"
			+ "<label text='Select a term to edit the text' halign='CENTER' />"
			+ "</if>" + "</grid>";


		return defaultLayout;
	}



	public List<HistoryItem> getChanges() {
		List<HistoryItem> out = new LinkedList<HistoryItem>();
		for (OBOTextEditComponent c : getMyResolver().getRegisteredComponents()) {
			//Collection<HistoryItem> changes = c.getChanges();
			List<HistoryItem> changes = c.getChanges();
			out.addAll(changes);
		}
		return out;
	}

	public boolean hasChanges() {
		for (OBOTextEditComponent c : getMyResolver().getRegisteredComponents()) {
			if (c.hasChanges())
				return true;
		}
		return false;
	}

	public void populateFields(IdentifiedObject io) {
		for (OBOTextEditComponent c : getMyResolver().getRegisteredComponents()) {
			c.populateFields(io);
		}
	}

	public void revert() {
		for (OBOTextEditComponent c : getMyResolver().getRegisteredComponents()) {
			c.revert();
		}
	}

	public void setObject(IdentifiedObject io) {
		this.currentObject = io;
		XMLLayoutUtil.guiupdateTree(this);
		for (OBOTextEditComponent c : getMyResolver().getRegisteredComponents()) {
			c.setObject(io);
		}

		// Why can't we just clear dirtyPaths?  (Not yet tested)
//		dirtyPaths.clear();
		dirtyPaths = new LinkedList<FieldPath>();
		warningMap.clear();
		setWarningMap(warningMap);
		doTimedTextChecks(false, false);
		fireLoadEvent(new TermLoadEvent(this, io));
	}

	public IdentifiedObject getObject() {
		return currentObject;
	}

	public void commit() {
		if (checkComponents()){
			flushEdits();
			Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
		}
	}

	public void setRoot(RootTextEditComponent root) {
		// do nothing; this is always its own root
	}

	public RootTextEditComponent getRoot() {
		return this;
	}

	protected boolean checkComponents() {
		return true;
	}

	public void flushEdits() {
		// Trigger verification checks that should happen on text commit
		doTimedTextChecks(false, true);
		TermMacroHistoryItem item = new TermMacroHistoryItem("Text edit");
		for (HistoryItem subItem : getChanges()) {
			item.addItem(subItem);
		}
//		logger.debug("> item.size(): " + item.size());
		if (item.size() > 0) {
//			logger.debug("> item: " + item);
			GUIUtil.setSelections(item, SelectionManager.getGlobalSelection(),
					SelectionManager.getGlobalSelection());

			SessionManager.getManager().apply(item, false);

		}
	}
}
