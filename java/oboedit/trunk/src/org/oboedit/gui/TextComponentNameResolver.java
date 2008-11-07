package org.oboedit.gui;

import java.awt.Component;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Properties;

import org.bbop.swing.ComponentNameResolver;
import org.oboedit.gui.components.SubsetEditorComponent;
import org.oboedit.gui.components.CommentEditorComponent;
import org.oboedit.gui.components.CompleteDefPanel;
import org.oboedit.gui.components.CrossProductEditorComponent;
import org.oboedit.gui.components.DefinitionEditorComponent;
import org.oboedit.gui.components.DomainEditorComponent;
import org.oboedit.gui.components.GeneralDbxrefEditorComponent;
import org.oboedit.gui.components.IDEditorComponent;
import org.oboedit.gui.components.NameEditorComponent;
import org.oboedit.gui.components.NamespaceEditorComponent;
import org.oboedit.gui.components.OBOCommitButton;
import org.oboedit.gui.components.OBORevertButton;
import org.oboedit.gui.components.PropertyBoxesEditorComponent;
import org.oboedit.gui.components.RangeEditorComponent;
import org.oboedit.gui.components.SynonymEditorComponent;
import org.oboedit.gui.widget.IntersectionPanel;

import org.apache.log4j.*;

public class TextComponentNameResolver implements
ComponentNameResolver {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextComponentNameResolver.class);

	protected Collection<OBOTextEditComponent> components = new LinkedList<OBOTextEditComponent>();

	protected RootTextEditComponent root;

	public TextComponentNameResolver(RootTextEditComponent root) {
		setRoot(root);
	}

	public void setRoot(RootTextEditComponent root) {
		this.root = root;
	}

	public Component createComponent(String id) {
		Component out = null;
		if (id.equals("NAME_EDITOR")) {
			out = new NameEditorComponent();
		} else if (id.equals("NAMESPACE_EDITOR")) {
			out = new NamespaceEditorComponent();
		} else if (id.equals("ID_EDITOR")) {
			out = new IDEditorComponent();
		} else if (id.equals("COMMENT_EDITOR")) {
			out = new CommentEditorComponent();
		} else if (id.equals("DEFINITION_EDITOR")) {
			out = new DefinitionEditorComponent();
		} else if (id.equals("CROSSPRODUCT_EDITOR")) {
			out = new CrossProductEditorComponent();
		} else if (id.equals("DBXREF_EDITOR")) {
			out = new GeneralDbxrefEditorComponent();
		} else if (id.equals("SYNONYM_EDITOR")) {
			out = new SynonymEditorComponent();
		} else if (id.equals("COMPLETE_DEF_EDITOR")) {
			out = new CompleteDefPanel();
		} else if (id.equals("INTERSECTION_EDITOR")) {
			out = new IntersectionPanel();
		} else if (id.equals("TEXT_COMMIT")) {
			out = new OBOCommitButton();
		} else if (id.equals("TEXT_REVERT")) {
			out = new OBORevertButton();
		} else if (id.equals("SUBSET_EDITOR")) {
			out = new SubsetEditorComponent();
		} else if (id.equals("RANGE_EDITOR")) {
			out = new RangeEditorComponent();
		} else if (id.equals("DOMAIN_EDITOR")) {
			out = new DomainEditorComponent();
		} else if (id.equals("PROPERTY_BOXES_EDITOR")) {
			out = new PropertyBoxesEditorComponent();
		} else
			return null;
		((OBOTextEditComponent) out).setRoot(root);
		return out;		
	}

	public Component resolveName(String id, Properties props, String xml) {
		Component out = createComponent(id);
		registerComponent((OBOTextEditComponent) out);
		((OBOTextEditComponent) out).setXML(xml);
		return out;
	}

	protected void registerComponent(OBOTextEditComponent c) {
		c.init();
		components.add(c);
	}

	public Collection<OBOTextEditComponent> getRegisteredComponents() {
		return components;
	}

	public void endParseNotify() {
	}

	public void startParseNotify() {
		for (OBOTextEditComponent c : new LinkedList<OBOTextEditComponent>(
				getRegisteredComponents())) {
			unregisterComponent(c);
		}
	}

	protected void unregisterComponent(OBOTextEditComponent c) {
		components.remove(c);
		c.cleanup();
	}

}
