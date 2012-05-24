package org.oboedit.gui;

import javax.swing.JComponent;

import org.obo.datamodel.FieldPathSpec;

public interface ErrorDecoratorFactory {

	public ErrorDecorator install(FieldPathSpec spec, OBOTextEditComponent parent,
			JComponent c);
	
	public void uninstall(ErrorDecorator e);
}
