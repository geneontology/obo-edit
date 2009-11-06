package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.awt.Color;
import java.awt.Component;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.DefaultCellEditor;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;

import org.oboedit.gui.components.ontologyGeneration.JTableHelper;
import org.oboedit.gui.components.ontologyGeneration.TermsTable;

import edu.emory.mathcs.backport.java.util.Arrays;

/**
 * {@link JTable} to display {@link List} of {@link T}.
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public abstract class AbstractOntologyTermsTable<T,R> extends JTable
{
	private static final long serialVersionUID = -5517462579527283469L;
	private int minScrollableViewPortHeight;
	private int maxScrollableViewPortHeight;
	private String lastRegex = new String();

	/**
	 * Constructs a {@link AbstractOntologyTermsTable}
	 */
	public AbstractOntologyTermsTable(AbstractOntologyTermsTableModel<T> tableModel)
	{
		super(tableModel);
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() + 4);
		getColumnModel().getColumn(0).setMaxWidth(50);
		getColumnModel().getColumn(0).setResizable(false);

		getColumnModel().getColumn(1).setMinWidth(250);

		getColumnModel().getColumn(2).setMinWidth(80);
		getColumnModel().getColumn(2).setMaxWidth(80);
		getColumnModel().getColumn(2).setResizable(false);
		TableColumn col = this.getColumnModel().getColumn(2);

		col.setCellEditor(new RelationComboBoxEditor());
		col.setCellRenderer(new RelationComboBoxRenderer());

		getColumnModel().getColumn(3).setMinWidth(80);
		getColumnModel().getColumn(3).setMaxWidth(80);
		getColumnModel().getColumn(3).setResizable(false);

		getColumnModel().getColumn(4).setMinWidth(130);
		getColumnModel().getColumn(4).setMaxWidth(130);
		getColumnModel().getColumn(4).setResizable(false);
		getColumnModel().getSelectionModel().addListSelectionListener(this);

		tableHeader.setReorderingAllowed(false);
	}

	/*
	 * ABSTRACT METHODS
	 */
	public abstract String nameFor(Object arg1);

	
	
	
	/**
	 * Set the {@link List} of {@link T} to be contained in the
	 * {@link TermsTable} and resize table if necessary.
	 * 
	 * @param results
	 */
	public void setTerms(Collection<T> results)
	{
		getModel().setTerms(results);
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Remove all instances of {@link T} from the {@link TermsTable}
	 * and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeTerms(Collection<T> terms)
	{
		getModel().removeAll(terms);
		JTableHelper.recalculateScrollableViewportSize(this, minScrollableViewPortHeight, maxScrollableViewPortHeight);
	}

	/**
	 * Remove all instances of {@link T} from the {@link TermsTable}
	 * and resize table if necessary.
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

	/**
	 * Update displayed candidate terms. Filter by regex provided.
	 * 
	 * @param regex
	 */
	public void findTerm(String regex)
	{
		if (regex != null && !lastRegex.equals(regex)) {
			lastRegex = regex;
			Pattern p = null;

			try {
				p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			}
			catch (PatternSyntaxException exception) {
				return;
			}
			Iterator<T> it = getModel().getAllTerms().iterator();
			int index = 0;
			while (it.hasNext()) {
				T term = it.next();
				String name = getModel().getTermName(term);
				if (p.matcher(name).find()) {
					getSelectionModel().setSelectionInterval(index, index);
					JTableHelper.scrollToCenter(this, index, 2);
					return;
				}
				index++;
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public AbstractOntologyTermsTableModel<T> getModel()
	{
		return (AbstractOntologyTermsTableModel<T>) super.getModel();
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
			component.setRenderer(new DefaultListCellRenderer()
			{
				private static final long serialVersionUID = 640070988606722883L;

				@Override
				public Component getListCellRendererComponent(JList arg0, Object arg1, int arg2, boolean arg3, boolean arg4)
				{

					super.getListCellRendererComponent(arg0, arg1, arg2, arg3, arg4);
					this.setText(nameFor(arg1));
					return this;
				}
			});
			
			getModel().addTableModelListener(this);
		}

		@Override
		public Component getTableCellEditorComponent(JTable arg0, Object value, boolean arg2, int arg3, int arg4)
		{
			JComboBox component = (JComboBox) this.getComponent();
			component.setSelectedItem(value);
			return component;
		}

		public void tableChanged(TableModelEvent e)
		{
			if(e.getSource()==getModel()) {
			
				int column = e.getColumn();
				if(column==2) {
					JComboBox component = (JComboBox) this.getComponent();
					component.removeAllItems();
					Object[] relationTypes = getModel().getRelationTypes();
					for (Object relationType : relationTypes) {
						component.addItem(relationType);
					}
				}
			}else{
				throw new RuntimeException("Not my event");
			}
		}

	}
}
