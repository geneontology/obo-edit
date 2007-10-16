package org.oboedit.gui;

import org.bbop.expression.JexlContext;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.oboedit.gui.event.TermLoadEvent;
import org.oboedit.gui.event.TermLoadListener;

import java.awt.Component;
import java.awt.font.*;
import java.util.*;
import javax.swing.*;

public abstract class AbstractTextEditComponent extends XMLLayoutPanel
		implements OBOTextEditComponent, ComponentNameResolver {

	protected Collection<TermLoadListener> loadListeners = new LinkedList<TermLoadListener>();

	protected FontRenderContext defaultContext = new FontRenderContext(null,
			false, false);

	protected IdentifiedObject currentObject;

	protected JexlContext context;

	protected String title;

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

	public Component resolveName(String id, Properties props, String xml) {
		return new JButton(id);
	}

	public ConfigurationPanel getConfigurationPanel() {
		return null;
	}

	public JexlContext getContext() {
		return context;
	}

	public void setContext(JexlContext context) {
		this.context = context;
	}

	public void startParseNotify() {
	}

	public void endParseNotify() {
	}

	public void init() {
		setDefaultFont(Preferences.getPreferences().getFont());

		if (useSubLayout()) {
			getXMLLayout().setComponentNameResolver(getResolver());
			getXMLLayout().setContext(getContext());
			setXMLLayout(getDefaultLayout());
		}

		installListeners();
		initializeGUI();
		loadGUI();
	}

	protected void installListeners() {
	}

	protected void uninstallListeners() {
	}

	public void revert() {
		loadGUI();
	}

	protected String getDefaultLayout() {
		return "";
	}

	protected boolean useSubLayout() {
		return false;
	}

	public void setXML(String xml) {
		if (useSubLayout() && xml != null && xml.trim().length() > 0) {
			if (!setXMLLayout(xml))
				setXMLLayout(getDefaultLayout());
		}
	}

	public boolean isXMLSettable() {
		return useSubLayout();
	}

	public ComponentNameResolver getResolver() {
		return this;
	}

	protected void initializeGUI() {
	}

	@Override
	public boolean reload() {
		if (useSubLayout())
			return super.reload();
		else
			return true;
	}

	public void cleanup() {
		uninstallListeners();
	}

	public boolean isSingleton() {
		return false;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}

	public void setConfiguration(ComponentConfiguration c) {
	}

	public JComponent getComponent() {
		return this;
	}
	
	public boolean hasChanges() {
		return getChanges().size() > 0;
	}

	public void setObject(IdentifiedObject currentObject) {
		this.currentObject = currentObject;
		loadGUI();
		fireLoadEvent(new TermLoadEvent(this, currentObject));
	}

	public IdentifiedObject getObject() {
		return currentObject;
	}

	protected abstract void loadGUI();

	public String getTitle() {
		if (title == null)
			return getID();
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

}
