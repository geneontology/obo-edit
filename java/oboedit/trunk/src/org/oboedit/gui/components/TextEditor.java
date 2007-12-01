package org.oboedit.gui.components;

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

public class TextEditor extends AbstractXMLOBOEditComponent implements
		RootTextEditComponent, SelectionDrivenComponent {

	protected Icon warningIcon = Preferences
			.loadLibraryIcon("warning_icon.gif");

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
			System.err.println("reloaded config");
			reload();
			setObject(getObject());
		}
		
	};

	protected ObjectSelector selector;

	protected Timer checkTimer;

	public static final int TIMER_DELAY = 1000;

	protected ActionListener checkTask = new ActionListener() {

		public void actionPerformed(ActionEvent e) {
			doTimedTextChecks();
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

	public boolean teardownWhenHidden() {
		return false;
	}

	protected void startTimer() {
		checkTimer = new Timer(TIMER_DELAY, checkTask);
	}

	public Map<FieldPathSpec, Collection<JComponent>> getComponentMap() {
		return fieldToComponentMap;
	}

	protected void stopTimer() {
		if (checkTimer != null)
			checkTimer.stop();
		checkTimer = null;
	}

	protected void doLoadTextChecks() {
		if (currentObject == null)
			return;
		final Collection<CheckWarning> warnings = VerificationManager
				.getManager().runChecks(
						SessionManager.getManager().getSession(),
						new FieldPath(currentObject),
						VerificationManager.TEXT_EDIT_COMMIT);
		final MultiMap<FieldPath, CheckWarning> allWarnings = new MultiHashMap<FieldPath, CheckWarning>();
		for (CheckWarning w : warnings) {
			allWarnings.add(w.getPath(), w);
		}
		for (FieldPath path : allWarnings.keySet()) {
			displayWarnings(path, allWarnings.get(path));
		}
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				fireIncrementalVerificationEvent(new IncrementalVerificationEvent(
						this, allWarnings));
			}
		});
	}

	protected void doTimedTextChecks() {
		if (this.dirtyPaths.size() == 0)
			return;
		if (currentObject == null)
			return;
		dirtyPaths = new LinkedList<FieldPath>();
		IdentifiedObject clone = (IdentifiedObject) currentObject.clone();
		populateFields(clone);
		for (ErrorDecorator decorator : decoratorMap.values()) {
			decorator.clearWarnings();
		}
		final Collection<CheckWarning> warnings = VerificationManager
				.getManager().runChecks(
						SessionManager.getManager().getSession(),
						new FieldPath(clone),
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
		/*
		 * List<FieldPath> dirtyPaths = this.dirtyPaths; this.dirtyPaths = new
		 * LinkedList<FieldPath>(); FieldPath.coalesce(dirtyPaths); MultiMap<FieldPath,
		 * CheckWarning> allWarnings = null; Iterator<FieldPath> it =
		 * dirtyPaths.iterator(); try { while (it.hasNext()) { FieldPath path =
		 * it.next(); it.remove();
		 * 
		 * if (fieldToComponentMap.containsKey(path.getSpec())) { final
		 * Collection<CheckWarning> warnings = VerificationManager
		 * .getManager().runChecks( SessionManager.getManager().getSession(),
		 * path, VerificationManager.TEXT_EDIT_THREAD); if (allWarnings == null)
		 * allWarnings = new MultiHashMap<FieldPath, CheckWarning>();
		 * allWarnings.put(path, warnings); displayWarnings(path, warnings); } }
		 * final MultiMap<FieldPath, CheckWarning> aw = allWarnings;
		 * SwingUtilities.invokeLater(new Runnable() { public void run() {
		 * fireIncrementalVerificationEvent(new IncrementalVerificationEvent(
		 * this, aw)); } }); } catch (ConcurrentModificationException ex) { }
		 */
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

	/*
	 * protected PropertyChangeListener focusChangeListener = new
	 * PropertyChangeListener() { protected Component lastFocusedTextRoot;
	 * 
	 * public void propertyChange(PropertyChangeEvent evt) { Component
	 * currentFocusedComponent = KeyboardFocusManager
	 * .getCurrentKeyboardFocusManager().getPermanentFocusOwner(); if
	 * (currentFocusedComponent == null) return; Component currentTextRoot =
	 * SwingUtilities.getAncestorOfClass( RootTextEditComponent.class,
	 * currentFocusedComponent); if (lastFocusedTextRoot != null &&
	 * !ObjectUtil.equals(lastFocusedTextRoot, currentTextRoot)) { autocommit(); }
	 * this.lastFocusedTextRoot = currentTextRoot; } };
	 */
	protected SelectionListener selectionListener = new SelectionListener() {

		public void selectionChanged(SelectionEvent e) {
			setObject(e.getSelection().getTermSubSelection());
		}
	};

	protected boolean global = false;

	protected void autocommit() {
		uninstallAutocommitListener();
		commit();
		installAutocommitListener();
	}

	protected void setWarningMap(MultiMap<FieldPath, CheckWarning> warningMap) {
		if (warningMap.isEmpty())
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					errorPanel.setVisible(false);
					validate();
					repaint();
				}
			});
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
			final StringBuffer buffer = new StringBuffer();
			buffer.append("<html>");
			buffer.append("<font color='" + color + "'>");
			if (errorCount > 0) {
				buffer.append(errorCount + " fatal error"
						+ (errorCount != 1 ? "s" : ""));
				if (warningCount > 0)
					buffer.append(" and ");
			}
			if (warningCount > 0)
				buffer.append(warningCount + " warning"
						+ (warningCount != 1 ? "s" : ""));
			buffer.append(".");
			buffer.append("</font>");
			buffer.append(" <i>Click for more details</i>");
			buffer.append("</html>");
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					errorLabel.setText(buffer.toString());
					errorLabel.setIcon(icon);
					errorLabel.setToolTipText(CheckWarningComponent.getHTML(
							warnings, "", "", true, false, false, false));
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
		if (currentObject == null || !hasChanges())
			return true;
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
				int val = JOptionPane.showConfirmDialog(GUIManager.getManager()
						.getFrame(), "There are uncommitted text edits.\n"
						+ "Discard these edits?", "Pending edits",
						JOptionPane.YES_NO_OPTION);
				return val == JOptionPane.YES_OPTION;
			}
		}
		return !fatal;
	}

	protected void activateVerifyPerspective() {
		ComponentManager.getManager().setPerspective("verify");
		GUIComponent c = ComponentManager.getManager().getActiveComponent(
				"VERIFICATION_MANAGER:main");
		if (c == null || !((JComponent) c).isVisible()) {
			ComponentManager.getManager().showComponent(
					new VerificationManagerFactory(), null);
		}
	}

	protected void uninstallAutocommitListener() {
		SelectionManager.getManager().removePreSelectionListener(
				preSelectListener);
		IOManager.getManager().removeIOListener(ioListener);

		/*
		 * KeyboardFocusManager.getCurrentKeyboardFocusManager()
		 * .removePropertyChangeListener(focusChangeListener);
		 */
	}

	protected void installAutocommitListener() {
		SelectionManager.getManager()
				.addPreSelectionListener(preSelectListener);
		IOManager.getManager().addIOListener(ioListener);
		/*
		 * KeyboardFocusManager.getCurrentKeyboardFocusManager()
		 * .addPropertyChangeListener(focusChangeListener);
		 */

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
				+ "<component id='ERROR_PANEL'/>"
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

				"<tab name='Cross Products`if (TermUtil.isIntersection(TextEditor.getObject())) \" *\"; else \"\";`' selected='`TermUtil.isIntersection(TextEditor.getObject())`'>"
				+ "<if eval='!TermUtil.isProperty(TextEditor.getObject())'>"
				+ "<component id='INTERSECTION_EDITOR'/>"
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

				"<tab name='Categories`if (size(TextEditor.getObject().getCategories()) > 0) \" *\"; else \"\";`' selected='`size(TextEditor.getObject().getCategories()) > 0`'>"
				+ "<component id='CATEGORY_EDITOR'/>"
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
			Collection<HistoryItem> changes = c.getChanges();
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

		dirtyPaths = new LinkedList<FieldPath>();
		warningMap.clear();
		// doLoadTextChecks();
		fireLoadEvent(new TermLoadEvent(this, io));
	}

	public IdentifiedObject getObject() {
		return currentObject;
	}

	public void commit() {
		if (checkComponents())
			flushEdits();
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
		TermMacroHistoryItem item = new TermMacroHistoryItem("Text edit");
		for (HistoryItem subItem : getChanges()) {
			item.addItem(subItem);
		}
		if (item.size() > 0) {
			GUIUtil.setSelections(item, SelectionManager.getGlobalSelection(),
					SelectionManager.getGlobalSelection());

			SessionManager.getManager().apply(item, false);
		}
	}
}
