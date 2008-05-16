package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.text.JTextComponent;

import org.obo.datamodel.FieldPathSpec;

import org.apache.log4j.*;

public class JTableErrorDecoratorFactory implements ErrorDecoratorFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(JTableErrorDecoratorFactory.class);

	public ErrorDecorator install(FieldPathSpec spec, OBOTextEditComponent parent,
			JComponent c) {
		if (c instanceof JTable) {
			return new JTableErrorDecorator(spec, parent, (JTable) c);
		} else
			return null;
	}

	public void uninstall(ErrorDecorator e) {
		e.cleanup();
	}

}
