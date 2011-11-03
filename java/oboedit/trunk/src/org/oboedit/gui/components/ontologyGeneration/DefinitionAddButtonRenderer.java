package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Component;
import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

class DefinitionAddButtonRenderer extends JButton implements TableCellRenderer
{
	private static final long serialVersionUID = -5256351123337745486L;

	public DefinitionAddButtonRenderer() 
	{
		setOpaque(true);
		this.setText("+");
		this.setToolTipText(Messages.getString("ButtonRenderer.AddDefinitionButton.tooltip")); //$NON-NLS-1$
		this.setFont(this.getFont().deriveFont(12.0f));
		this.setMargin(new Insets(1,1,1,1));
	}
	
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
			int row, int column)
	{
		return this;
	}
}