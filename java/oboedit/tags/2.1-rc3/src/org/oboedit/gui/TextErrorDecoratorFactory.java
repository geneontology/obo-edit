package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.text.JTextComponent;

import org.obo.datamodel.FieldPathSpec;

import org.apache.log4j.*;

public class TextErrorDecoratorFactory implements ErrorDecoratorFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextErrorDecoratorFactory.class);

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
