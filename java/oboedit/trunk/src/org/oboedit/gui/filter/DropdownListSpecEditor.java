package org.oboedit.gui.filter;

import java.awt.GridLayout;

import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JSpinner;

public class DropdownListSpecEditor<T> extends JPanel implements
		GeneralRendererSpecFieldEditor<T> {

	protected JComboBox valueBox = new JComboBox();

	public DropdownListSpecEditor(T... values) {
		for (T v : values) {
			valueBox.addItem(v);
		}
		if (valueBox.getItemCount() > 0)
			valueBox.setSelectedIndex(0);
		add(valueBox);
	}

	public T getValue() {
		return (T) valueBox.getSelectedItem();
	}

	public void setValue(T o) {
		if (o == null && valueBox.getItemCount() > 0)
			valueBox.setSelectedIndex(0);
		else
			valueBox.setSelectedItem(o);
	}
}
