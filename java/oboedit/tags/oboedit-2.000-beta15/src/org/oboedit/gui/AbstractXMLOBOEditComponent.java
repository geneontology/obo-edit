package org.oboedit.gui;

import java.awt.Container;
import java.util.Collection;

import javax.swing.JComponent;

import org.bbop.expression.JexlContext;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.GUIComponent;
import org.bbop.swing.ComponentNameResolver;
import org.bbop.swing.XMLLayoutPanel;
import org.oboedit.gui.event.TermLoadEvent;
import org.oboedit.gui.event.TermLoadListener;

public abstract class AbstractXMLOBOEditComponent extends XMLLayoutPanel
		implements GUIComponent {
	public static class XMLConfiguration {
		protected String xml;

		public XMLConfiguration() {
		}

		public String getXml() {
			return xml;
		}

		public void setXml(String xml) {
			this.xml = xml;
		}
	}

	protected String id;

	protected String title;

	protected ComponentNameResolver nameResolver;
	
	public AbstractXMLOBOEditComponent(String id,
			ComponentNameResolver resolver, JexlContext context) {
		this(id);

		setup(resolver, context);
	}
	
	public AbstractXMLOBOEditComponent(String id) {
		super();
		this.id = id;
		setOpaque(true);
	}
	
	public void setup(ComponentNameResolver resolver, JexlContext context) {
		setResolver(resolver);
		setContext(context);
		setXMLLayout(getDefaultLayout());		
	}
	
	public void setResolver(ComponentNameResolver resolver) {
		getXMLLayout().setComponentNameResolver(resolver);
	}
	
	public ComponentNameResolver getResolver() {
		return getXMLLayout().getComponentNameResolver();
	}
	
	public JexlContext getContext() {
		return getXMLLayout().getContext();
	}
	
	public void setContext(JexlContext context) {
		getXMLLayout().setContext(context);
	}

	public void cleanup() {
		uninstallListeners();
	}

	protected void uninstallListeners() {
	}

	public JComponent getComponent() {
		return this;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}

	public String getID() {
		return id;
	}

	public String getTitle() {
		return title;
	}

	public void init() {
		installListeners();
		initializeGUI();
	}

	protected void initializeGUI() {
		// TODO Auto-generated method stub

	}

	protected void installListeners() {
		// TODO Auto-generated method stub

	}

	public boolean isXMLSettable() {
		return true;
	}

	protected abstract String getDefaultLayout();

	public void setConfiguration(ComponentConfiguration config) {
		if (config instanceof XMLConfiguration && isXMLSettable()) {
			String xml = ((XMLConfiguration) config).getXml();

			setXML(xml);
		}
	}

	public void setTitle(String name) {
		this.title = name;
	}

	public void setXML(String xml) {
		if (xml != null && xml.trim().length() > 0) {
			if (!setXMLLayout(xml))
				setXMLLayout(getDefaultLayout());
		}
	}
}
