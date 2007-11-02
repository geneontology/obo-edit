package org.oboedit.gui.filter;

import java.awt.GridLayout;

import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JSpinner;

public class DropdownListSpecEditor<T> extends JPanel implements
		GeneralRendererSpecFieldEditor<T> {

	protected JComboBox valueBox = new JComboBox();

	public DropdownListSpecEditor(T... values) {
		for(T v : values) {
			valueBox.addItem(v);
		}
		add(valueBox);
	}

	public T getValue() {
		return (T) valueBox.getSelectedItem();
	}

	public void setValue(T o) {
		valueBox.setSelectedItem(o);
	}
}
