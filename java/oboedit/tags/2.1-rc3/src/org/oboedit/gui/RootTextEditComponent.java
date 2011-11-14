package org.oboedit.gui;

import javax.swing.JComponent;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;

public interface RootTextEditComponent extends OBOTextEditComponent {
	public void commit();

	public void addDirtyPath(FieldPath path);

	public void addMapping(FieldPathSpec path, OBOTextEditComponent parent,
			JComponent component);
	public void removeMapping(FieldPathSpec path, JComponent component);
}
