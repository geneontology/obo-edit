package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.text.JTextComponent;

import org.obo.datamodel.FieldPathSpec;

public class JListErrorDecoratorFactory implements ErrorDecoratorFactory {

	public ErrorDecorator install(FieldPathSpec spec, OBOTextEditComponent parent,
			JComponent c) {
		if (c instanceof JList) {
			return new JListErrorDecorator(spec, parent, (JList) c);
		} else
			return null;
	}

	public void uninstall(ErrorDecorator e) {
		e.cleanup();
	}

}
