package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.text.JTextComponent;

import org.bbop.swing.widget.TableList;
import org.obo.datamodel.FieldPathSpec;

import org.apache.log4j.*;

public class TableListErrorDecoratorFactory implements ErrorDecoratorFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TableListErrorDecoratorFactory.class);

	public ErrorDecorator install(FieldPathSpec spec, OBOTextEditComponent parent,
			JComponent c) {
		if (c instanceof TableList) {
			return new TableListErrorDecorator(spec, parent, (TableList<?>) c);
		} else
			return null;
	}

	public void uninstall(ErrorDecorator e) {
		e.cleanup();
	}

}
