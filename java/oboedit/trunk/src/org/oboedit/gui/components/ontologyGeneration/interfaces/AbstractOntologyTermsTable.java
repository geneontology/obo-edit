package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.util.Collection;
import java.util.List;

import javax.swing.DefaultCellEditor;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.oboedit.gui.components.ontologyGeneration.JTableHelper;
import org.oboedit.gui.components.ontologyGeneration.TermsTable;

/**
 * {@link JTable} to display {@link List} of T.
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public abstract class AbstractOntologyTermsTable<T, R> extends JTable
{
	private static final long serialVersionUID = -5517462579527283469L;
	private int minScrollableViewPortHeight;
	private int maxScrollableViewPortHeight;
	// private String lastRegex = new String();

	/**
	 * Constructs a {@link AbstractOntologyTermsTable}
	 */
	public AbstractOntologyTermsTable(AbstractOntologyTermsTableModel<T, R> tableModel)
	{
		super(tableModel);
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() + 4);
		
		TableColumnModel columnModel = this.getColumnModel();
		
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Selector.ordinal()).setMinWidth(40);
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Selector.ordinal()).setMaxWidth(40);
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Selector.ordinal()).setResizable(false);

		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Open.ordinal()).setMinWidth(20);
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Open.ordinal()).setMaxWidth(20);
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Open.ordinal()).setResizable(false);

		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Term.ordinal()).setMinWidth(150);
		TableColumn termColumn = columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Term.ordinal());
		termColumn.setCellRenderer(new TermCellRenderer());
		
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Identifier.ordinal()).setMinWidth(50);

		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Relation.ordinal()).setMinWidth(70);
		
		TableColumn relationColumn = columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Relation.ordinal());
		relationColumn.setCellEditor(new RelationComboBoxEditor());
		relationColumn.setCellRenderer(new RelationComboBoxRenderer());

		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Predicted.ordinal()).setMinWidth(50);

		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Comment.ordinal()).setMinWidth(100);
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Comment.ordinal()).setMaxWidth(100);
		columnModel.getColumn(AbstractOntologyTermsTableModel.columns.Comment.ordinal()).setResizable(false);

		columnModel.getSelectionModel().addListSelectionListener(this);
		tableHeader.setReorderingAllowed(false);
	}

	public abstract String nameFor(Object arg1);

	/**
	 * Set the {@link List} of {@link T} to be contained in the {@link TermsTable} and resize table if necessary.
	 * 
	 * @param results
	 */

	public void setTerms(Collection<T> results)
	{
		getModel().setTerms(results);
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Remove all instances of {@link T} from the {@link TermsTable} and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeTerms(Collection<T> terms)
	{
		getModel().removeAll(terms);
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Remove all instances of {@link T} from the {@link TermsTable} and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeAllTerms()
	{
		getModel().removeAll();
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Set the minimal height of the visible area for the {@link TermsTable}
	 * 
	 * @param minHeight
	 */
	public void setMinimumPreferedeScrollableViewportHeight(int minHeight)
	{
		this.minScrollableViewPortHeight = minHeight;
	}

	/**
	 * Set the maximal height of the visible area for the {@link TermsTable}
	 * 
	 * @param maxHeight
	 */
	public void setMaximumPreferedeScrollableViewportHeight(int maxHeight)
	{
		this.maxScrollableViewPortHeight = maxHeight;
	}

	@SuppressWarnings("unchecked")
	@Override
	public AbstractOntologyTermsTableModel<T, R> getModel()
	{
		return (AbstractOntologyTermsTableModel<T, R>) super.getModel();
	}

	private class RelationComboBoxRenderer extends DefaultTableCellRenderer
	{
		@Override
		public Component getTableCellRendererComponent(JTable arg0, Object arg1, boolean arg2, boolean arg3, int arg4, int arg5)
		{
			super.getTableCellRendererComponent(arg0, arg1, arg2, arg3, arg4, arg5);
			this.setText(nameFor(arg1));
			return this;
		}

		private static final long serialVersionUID = -1853474959828414991L;
	}

	public class RelationComboBoxEditor extends DefaultCellEditor implements TableModelListener
	{

		private static final long serialVersionUID = 6608118736812902848L;

		public RelationComboBoxEditor()
		{
			super(new JComboBox(getModel().getRelationTypes()));
			JComboBox component = (JComboBox) this.getComponent();
			component.setEnabled(true);
			component.setRenderer(new DefaultListCellRenderer()
			{
				private static final long serialVersionUID = 640070988606722883L;

				@Override
				public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus)
				{

					super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
					this.setText(nameFor(value));
					return this;
				}
			});

			getModel().addTableModelListener(this);
		}

		@Override
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column)
		{
			JComboBox component = (JComboBox) this.getComponent();
			component.setSelectedItem(value);
			return component;
		}

		public void tableChanged(TableModelEvent e)
		{
			if (e.getSource() == getModel()) {

				int column = e.getColumn();
				getModel();
				if (column == AbstractOntologyTermsTableModel.columns.Relation.ordinal()) {
					JComboBox component = (JComboBox) this.getComponent();
					component.removeAllItems();
					R[] relationTypes = getModel().getRelationTypes();
					for (Object relationType : relationTypes) {
						component.addItem(relationType);
					}
				}
			}
			else {
				throw new RuntimeException("Not my event");
			}
		}
	}
	
	private class TermCellRenderer extends DefaultTableCellRenderer
	{
        private static final long serialVersionUID = -6619355461941335706L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {    	
        	Component component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        	
        	component.setFont(component.getFont().deriveFont(Font.BOLD));
        	
        	return component;
        }
        
	}
}
