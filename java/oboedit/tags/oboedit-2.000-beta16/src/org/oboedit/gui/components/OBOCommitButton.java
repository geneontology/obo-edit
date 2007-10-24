package org.oboedit.gui.components;

import java.awt.event.*;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.swing.*;

import org.bbop.expression.JexlContext;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.gui.OBOTextEditComponent;
import org.oboedit.gui.RootTextEditComponent;
import org.oboedit.gui.event.TermLoadEvent;
import org.oboedit.gui.event.TermLoadListener;

public class OBOCommitButton extends JButton implements OBOTextEditComponent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected String title;

	protected IdentifiedObject currentObject;

	protected Collection<TermLoadListener> loadListeners = new LinkedList<TermLoadListener>();

	protected RootTextEditComponent root;

	public void setRoot(RootTextEditComponent root) {
		this.root = root;
	}

	public RootTextEditComponent getRoot() {
		return root;
	}

	public boolean teardownWhenHidden() {
		return true;
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

	/*
	 * protected PreSelectionListener listener = new PreSelectionListener() {
	 * public boolean isPreSelectOkay(PreSelectionEvent e) { return
	 * Controller.getController().getTextEditManager(). checkUncommittedEdits(); } };
	 */
	public OBOCommitButton() {
		super("Commit");
	}

	public ConfigurationPanel getConfigurationPanel() {
		return null;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getTitle() {
		if (title == null)
			return getID();
		return title;
	}

	public void init() {
		addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				doCommit();
			}
		});
		setMinimumSize(getPreferredSize());
	}

	protected void doCommit() {
		RootTextEditComponent root = (RootTextEditComponent) SwingUtilities
				.getAncestorOfClass(RootTextEditComponent.class, this);
		root.commit();
	}

	public void cleanup() {
	}

	public boolean isSingleton() {
		return false;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}

	public void setConfiguration(ComponentConfiguration config) {
	}

	public String getID() {
		return "TEXT_COMMIT";
	}

	public JComponent getComponent() {
		return this;
	}

	public void setXML(String xml) {
		// do nothing
	}

	public List getChanges() {
		// do nothing, this is just a wrapper
		return Collections.EMPTY_LIST;
	}

	public boolean hasChanges() {
		return false;
	}

	public void populateFields(IdentifiedObject io) {
		// do nothing, this is just a wrapper
	}

	public void revert() {
		// do nothing, this is just a wrapper

	}

	public void setObject(IdentifiedObject io) {
		this.currentObject = io;
		fireLoadEvent(new TermLoadEvent(this, io));
	}

	public IdentifiedObject getObject() {
		return currentObject;
	}

	public void setContext(JexlContext context) {
	}

	public boolean isXMLSettable() {
		return false;
	}
}
