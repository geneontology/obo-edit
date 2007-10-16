package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.text.JTextComponent;

import org.obo.datamodel.FieldPathSpec;

public class TextErrorDecoratorFactory implements ErrorDecoratorFactory {

	public ErrorDecorator install(FieldPathSpec spec, OBOTextEditComponent parent,
			JComponent c) {
		if (c instanceof JTextComponent) {
			return new TextErrorDecorator(spec, parent, (JTextComponent) c);
		} else
			return null;
	}

	public void uninstall(ErrorDecorator e) {
		e.cleanup();
	}

}
