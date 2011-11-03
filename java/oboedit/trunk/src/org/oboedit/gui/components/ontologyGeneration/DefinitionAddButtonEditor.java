package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Component;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JTable;

class DefinitionAddButtonEditor extends DefaultCellEditor {
	private static final long serialVersionUID = -8076884317274517175L;

	protected JButton button;

	public DefinitionAddButtonEditor(JCheckBox checkBox) {
		super(checkBox);
		button = new JButton("+");
		button.setToolTipText(Messages.getString("ButtonRenderer.AddDefinitionButton.tooltip")); //$NON-NLS-1$
		button.setFont(button.getFont().deriveFont(12.0f));
		button.setMargin(new Insets(1, 1, 1, 1));
		button.setOpaque(true);
		button.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				fireEditingStopped();
			}
		});
	}

	@Override
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
		return button;
	}

	@Override
	public Object getCellEditorValue() {
		return new String("+");
	}

	@Override
	public boolean stopCellEditing() {
		return super.stopCellEditing();
	}

	@Override
	protected void fireEditingStopped() {
		super.fireEditingStopped();
	}
}