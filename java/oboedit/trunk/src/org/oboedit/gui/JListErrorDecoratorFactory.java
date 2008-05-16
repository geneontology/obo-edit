package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.text.JTextComponent;

import org.obo.datamodel.FieldPathSpec;

import org.apache.log4j.*;

public class JListErrorDecoratorFactory implements ErrorDecoratorFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(JListErrorDecoratorFactory.class);

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
