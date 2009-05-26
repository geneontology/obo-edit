package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Component;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

public class ButtonRenderer extends JButton implements TableCellRenderer
{
	private static final long serialVersionUID = -5256351123337745486L;

	public ButtonRenderer() 
	{
		setOpaque(true);
		this.setText("+");
		this.setMargin(new Insets(1,1,1,1));
	}
	
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
			int row, int column)
	{
		return this;
	}
}

class ButtonEditor extends DefaultCellEditor 
{
	private static final long serialVersionUID = -8076884317274517175L;

	protected JButton button;

	public ButtonEditor(JCheckBox checkBox) {
		super(checkBox);
		button = new JButton("+");
		button.setMargin(new Insets(1, 1, 1, 1));
		button.setOpaque(true);
		button.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				fireEditingStopped();
			}
		});
	}

	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
		return button;
	}

	public Object getCellEditorValue() {
		return new String("+");
	}

	public boolean stopCellEditing() {
		return super.stopCellEditing();
	}

	protected void fireEditingStopped() {
		super.fireEditingStopped();
	} 
}