package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.MouseEvent;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface;

/**
 * {@link JTable} to display {@link List} of {@link CandidateTerm}.
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class TermsTable extends JTable {
	private static final long serialVersionUID = 1184929335454420009L;
	private String lastRegex = new String();
	private int lastVisibleRow = -1;
	private int firstVisibleRow = -1;

	private TermsTableCellRenderer termsTableCellRenderer;
	private TableColumn colToHide;

	/**
	 * Constructs a {@link TermsTable}
	 * 
	 * @param numberOfColumnsToShow
	 * @param clipboard
	 */
	public TermsTable(OntologyModelAdapterInterface<?, ?> adapter, CandidateTermCache clipboard, int numberOfColumnsToShow, boolean isMainTermsTable) {
		super(new TermsTableModel(adapter, clipboard, numberOfColumnsToShow, isMainTermsTable));
		setGridColor(Color.LIGHT_GRAY);
		setRowHeight(getRowHeight() + 4);
		tableHeader.setReorderingAllowed(false);
		termsTableCellRenderer = new TermsTableCellRenderer();
		if (this.getColumnCount() > 3) {
			colToHide = this.getColumnModel().getColumn(3);
		}
	}

	/**
	 * Set the {@link List} of {@link CandidateTerm} to be contained in the
	 * {@link TermsTable} and resize table if necessary.
	 * 
	 * @param results
	 */
	public void setTerms(List<CandidateTerm> results) {
		getModel().setTerms(results);
		setCurrentFirstVisibleRow(-1);
		setCurrentLastVisibleRow(-1);
	}

	/**
	 * Set whether only existing loaded terms should be displayed
	 * 
	 * @param onlyExistingTerms
	 */
	public void setOnlyShowExistingTerms(boolean onlyExistingTerms) {
		getModel().setOnlyShowExistingTerms(onlyExistingTerms);
	}

	/**
	 * Remove all instances of {@link CandidateTerm} from the {@link TermsTable}
	 * and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeTerms(List<CandidateTerm> terms) {
		getModel().removeAll(terms);
		setCurrentFirstVisibleRow(-1);
		setCurrentLastVisibleRow(-1);
	}

	/**
	 * Remove all instances of {@link CandidateTerm} from the {@link TermsTable}
	 * and resize table if necessary.
	 * 
	 * @param terms
	 */
	public void removeAllTerms() {
		getModel().removeAll();
		setCurrentFirstVisibleRow(-1);
		setCurrentLastVisibleRow(-1);
	}

	/*
	 * OVERRIDDEN METHODS
	 */

	@Override
	public TermsTableModel getModel() {
		return (TermsTableModel) super.getModel();
	}

	@Override
	public void setValueAt(Object value, int row, int column) {
		super.setValueAt(value, row, column);
		if (column == 0) {
			CandidateTerm term = getModel().getTermAt(row);
			term.setTicked((Boolean) value);
			if (term.isTicked()) {
				getModel().addTermToClipboard(term);
			} else {
				getModel().removeTermFromClipboard(term);
			}
			getModel().fireTableCellUpdated(row, column);
		}
	}

	@Override
	public String getToolTipText(MouseEvent e) {
		String tip = null;
		java.awt.Point p = e.getPoint();
		int rowIndex = rowAtPoint(p);
		int colIndex = columnAtPoint(p);
		int realColumnIndex = convertColumnIndexToModel(colIndex);

		if (realColumnIndex == 3) {
			setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			tip = Messages.getString("TermsTable.OpenGoPubMedResultsButton"); //$NON-NLS-1$

		} else if (realColumnIndex == 1) {
			tip = getModel().getValueAt(rowIndex, colIndex).toString();
		} else if (realColumnIndex == 2) {
			setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			tip = Messages.getString("OntologyGenerationComponent.GenerateDefinitionsButton"); //$NON-NLS-1$
		} else {
			/*
			 * You can omit this part if you know you don't have any renderer
			 * that supply their own tool tips.
			 */
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			tip = super.getToolTipText(e);
		}

		return tip;
	}

	/**
	 * Update displayed candidate terms. Filter by regex provided.
	 * 
	 * @param regex
	 */
	public void findTerm(String regex) {

		if (regex != null && !lastRegex.equals(regex)) {
			lastRegex = regex;
			Pattern p = null;

			try {
				p = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			} catch (PatternSyntaxException exception) {
				return;
			}
			Iterator<CandidateTerm> it = getModel().getAllTerms().iterator();
			int index = 0;
			while (it.hasNext()) {
				CandidateTerm term = it.next();
				String name = term.getGeneratedLabel();
				if (p.matcher(name).find()) {
					getSelectionModel().setSelectionInterval(index, index);
					JTableHelper.scrollToCenter(this, index, 2);
					return;
				}
				index++;
			}
		}
	}

	/**
	 * @return
	 */
	public int getCurrentLastVisibleRow() {
		return lastVisibleRow;
	}

	/**
	 * @return
	 */
	public int getCurrentFirstVisibleRow() {
		return firstVisibleRow;
	}

	/**
	 * @param firstVisibleRow
	 */
	public void setCurrentFirstVisibleRow(int firstVisibleRow) {
		this.firstVisibleRow = firstVisibleRow;
	}

	/**
	 * @param lastVisibleRow
	 */
	public void setCurrentLastVisibleRow(int lastVisibleRow) {
		this.lastVisibleRow = lastVisibleRow;

	}

	/**
	 * @see javax.swing.JTable#getCellRenderer(int, int)
	 */
	@Override
	public TableCellRenderer getCellRenderer(int row, int column) {
		if (column == 1)
			return termsTableCellRenderer;
		else
			return super.getCellRenderer(row, column);
	}

	private class TermsTableCellRenderer extends DefaultTableCellRenderer {
		private static final long serialVersionUID = 5435653832511917987L;

		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
				boolean hasFocus, int row, int column) {
			JLabel comp = (JLabel) super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
			comp.setText((String) getModel().getValueAt(row, column));
			if (getModel().getTermAt(row).isInLoadedOntology()) {
				comp.setFont(table.getFont().deriveFont(Font.BOLD));
			}
			return comp;
		}

	}

	void updateTermsTableStructure() {
		if (this.getColumnCount() > 0) {
			this.getColumnModel().getColumn(0).setMinWidth(50);
			this.getColumnModel().getColumn(0).setMaxWidth(50);
			this.getColumnModel().getColumn(0).setPreferredWidth(50);
			this.getColumnModel().getColumn(0).setResizable(false);
		}

		if (this.getColumnCount() > 2) {
			this.getColumnModel().getColumn(2).setMinWidth(18);
			this.getColumnModel().getColumn(2).setMaxWidth(18);
			this.getColumnModel().getColumn(2).setPreferredWidth(18);
			this.getColumnModel().getColumn(3).setResizable(false);
			TableCellImageRenderer definitionGenerationImageRenderer = new TableCellImageRenderer(
					"resources/iconDefinitionGeneration.png");
			this.getColumnModel().getColumn(2).setCellRenderer(definitionGenerationImageRenderer);
		}

		if (this.getColumnCount() > 3) {
			this.getColumnModel().getColumn(3).setMinWidth(30);
			this.getColumnModel().getColumn(3).setMaxWidth(30);
			this.getColumnModel().getColumn(3).setPreferredWidth(18);
			this.getColumnModel().getColumn(3).setResizable(false);
			TableCellImageRenderer termInformationIconRenderer = new TableCellImageRenderer("resources/aboutIcon.png");
			this.getColumnModel().getColumn(3).setCellRenderer(termInformationIconRenderer);
		}
	}

	private Stack<TableColumn> colHidden = new Stack<TableColumn>();

	public void setShowInformationIcon(boolean b) {
		if (!b && colHidden.isEmpty()) {
			this.removeColumn(colToHide);
			this.validate();
			if (colHidden.isEmpty()) {
				colHidden.push(colToHide);
			}
		} else if (b && !colHidden.isEmpty()) {
			this.addColumn(colHidden.pop());
			this.validate();
		}
	}
}
