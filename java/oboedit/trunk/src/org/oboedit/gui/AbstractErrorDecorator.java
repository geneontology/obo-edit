package org.oboedit.gui;

import java.awt.Component;
import java.util.Collection;
import java.util.Collections;

import javax.swing.JComponent;
import javax.swing.JTable;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.util.TextEditUtil;
import org.oboedit.verify.CheckWarning;

public abstract class AbstractErrorDecorator<T extends JComponent> implements
		ErrorDecorator {

	protected FieldPathSpec spec;

	protected OBOTextEditComponent parent;

	protected MultiMap<FieldPath, CheckWarning> warnings = new MultiHashMap<FieldPath, CheckWarning>();

	protected T component;

	protected void markDirty() {
		TextEditUtil.addDirtyPaths(parent, getPaths());
	}

	public Collection<FieldPath> getPaths() {
		if (parent.getObject() == null)
			return Collections.emptySet();
		IdentifiedObject object = (IdentifiedObject) parent.getObject().clone();
		parent.populateFields(object);
		FieldPath queryPath = FieldPathSpec.createQueryPath(spec, object);
		Collection<FieldPath> out = queryPath.resolve();
		return out;
	}

	public AbstractErrorDecorator(FieldPathSpec spec,
			OBOTextEditComponent parent, T c) {
		setSpec(spec);
		setParent(parent);
		setComponent(c);
		init();
	}

	public void setParent(OBOTextEditComponent parent) {
		this.parent = parent;
	}

	public void setSpec(FieldPathSpec spec) {
		this.spec = spec;
	}

	public void setComponent(T c) {
		this.component = c;
	}
	
	public T getComponent() {
		return component;
	}

	public abstract void init();

	public void clearWarnings() {
		warnings.clear();
		component.validate();
		component.repaint();
	}

	public void setWarnings(FieldPath path, Collection<CheckWarning> warnings) {
		this.warnings.put(path, warnings);
		component.validate();
		component.repaint();
	}
}
