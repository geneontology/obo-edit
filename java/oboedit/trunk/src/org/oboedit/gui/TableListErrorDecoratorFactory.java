package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.text.JTextComponent;

import org.bbop.swing.widget.TableList;
import org.obo.datamodel.FieldPathSpec;

public class TableListErrorDecoratorFactory implements ErrorDecoratorFactory {

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
