package org.oboedit.gui.components;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;


import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;

import org.apache.log4j.Logger;

/**
 * Displays redundant links in table format to view, assess redundancy and delete.
 * */
public class RemoveRedundantLinksComponent extends AbstractGUIComponent implements ListSelectionListener {
	private static final long serialVersionUID = 1L;

	//	initialize logger
	protected final static Logger logger = Logger.getLogger(RemoveRedundantLinksComponent.class);

	protected JPanel panel = new JPanel();
	protected Box northPanel = Box.createHorizontalBox();
	protected Box southPanel = Box.createHorizontalBox();
	protected static ImageIcon expIcon = (ImageIcon) Preferences.loadLibraryIcon("info_icon.gif");
	protected JButton removeLinksButton = new JButton("Delete Links");
	protected JScrollPane sp = null;

	protected Collection<Link> redundantLinks = null;
	protected Collection<Link> allLinks = null;
	protected List<Link> links;

	protected JTable table;
	private RemoveRedundantLinksModel model;
	protected CheckBoxHeader selectAll;
	protected MyItemListener it;
	protected boolean selectFlags[];
	protected List<Integer> selectedIx = new ArrayList<Integer>();
	private final int COLUMN_COUNT = 4;
	/** Alternate row highlighting - Light blue color. */
	public static final Color LIGHT_BLUE = new Color(210,220,240);


	public RemoveRedundantLinksComponent(final String id) {
		super(id);
		setLayout(new BorderLayout());
		northPanel.add(Box.createVerticalGlue());
		southPanel.add(Box.createGlue());

		redundantLinks = getRedundantLinks();
		//notify when there are no redundant links
		if(redundantLinks.size()==0){
			JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
			"There are no redundant links in the current ontology.");	
		}
		links = new ArrayList<Link>(redundantLinks);
		displayResults();
		removeLinksButton.setToolTipText("Remove selected links");	
		removeLinksButton.addActionListener(new ActionListener(){
			public void actionPerformed(final ActionEvent e){
				removeLinks();
			}
		});

		southPanel.add(removeLinksButton);
		add(northPanel,"Center");
		add(southPanel, "South");
		revalidate();
	}

	protected void removeLinks(){
		final Collection<Link> removeLinks = new ArrayList<Link>();
		//		logger.debug("redundantLinks size before removing: " + redundantLinks.size());

		logger.debug("Removing " + selectedIx.size() + " links...");
		for(int i=0; i<selectedIx.size(); i++){
			logger.debug("Remove selectedIx[" + i + "]: " + selectedIx.get(i));
			logger.debug("which is link: " + links.get(selectedIx.get(i)));
			removeLinks.add(links.get(selectedIx.get(i)));
			redundantLinks.remove(links.get(selectedIx.get(i)));

		}
		for (final Link link : removeLinks) {
			final TermMacroHistoryItem item = new TermMacroHistoryItem(
					"Delete "+ removeLinks.size() +" redundant links");
			item.addItem(new DeleteLinkHistoryItem(link));
			if (item != null)
				SessionManager.getManager().apply(item);
		}
		//logger.debug("redundantLinks size after deleting: " + redundantLinks.size());
		//update table
		displayResults();
	}

	protected Collection<Link> getRedundantLinks(){
		final ReasonedLinkDatabase reasoner = SessionManager.getManager().getReasoner();
		allLinks = new LinkedHashSet<Link>();
		redundantLinks = new LinkedHashSet<Link>();

		final Iterator<Link> it = TermUtil.getAllLinks(reasoner);
		while (it.hasNext()) {
			final Link link = it.next();
			// TODO: make configurable, allow repair mode, in which links implied via xps are not considered redundant
			Explanation explanation = ReasonerUtil.getRedundancyExplanation(reasoner, link, true);
			if (explanation == null)
				continue;
			redundantLinks.add(link);
		}
		return redundantLinks;		
	}

	class RemoveRedundantLinksModel extends AbstractTableModel {
		private static final long serialVersionUID = 1L;
		private final String[] columnNames = {"Select","Child Name","Parent Name", "Explanation"};
		private Object[][] data;
		protected int sortCol = 1;
		protected boolean ascending = true;

		public RemoveRedundantLinksModel(Object[][] data) {
			this.data = data;
		}

		public int getColumnCount() {
			return columnNames.length;
		}

		public int getRowCount() {
			return data == null ? 0 : data.length;
		}

		public String getColumnName(int col) {
			return columnNames[col];
		}

		/**
		 * set value at [rowIndex][columnIndex]
		 * @param Object value
		 * @param int rowIndex
		 * @param int columnIndex
		 * */
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			data[rowIndex][columnIndex] = value;
			super.fireTableCellUpdated(rowIndex, columnIndex);
		}

		/**
		 * getValueAt
		 * @param int rowIndex
		 * @param int columnIndex
		 * @return value at [rowIndex][columnIndex]
		 * */
		public Object getValueAt(final int rowIndex, final int columnIndex) {
			if (rowIndex < 0 || rowIndex >= getRowCount())
				return "";
			if(columnIndex > 3){
				return "";
			}
			return data[rowIndex][columnIndex];
		}

		public boolean isCellEditable(final int rowIndex, final int columnIndex) {
			if(columnIndex ==0)
				return true;
			else
				return false;
		}

		public Class<?> getColumnClass(final int columnIndex) {
			if (data == null || data.length == 0) {
				return Object.class;
			}
			final Object o = getValueAt(0, columnIndex);
			return o == null ? Object.class : o.getClass();
		}

		class ColumnListener extends MouseAdapter {
			protected JTable table;
			public ColumnListener(JTable t) {
				table = t;
			}
			public void mouseClicked(MouseEvent e) {
				TableColumnModel columnModel = table.getColumnModel();
				int viewColumn = columnModel.getColumnIndexAtX(e.getX());
				int column = columnModel.getColumn(viewColumn).getModelIndex();

				if (column < 0)
					return;
				if (sortCol == column)
					ascending = !ascending;
				else
					sortCol = column;
				Arrays.sort(data, new ColumnSorter(sortCol, ascending));
				table.tableChanged(new TableModelEvent(RemoveRedundantLinksModel.this));
				table.repaint();
			}
		}

	}

	public void valueChanged(final ListSelectionEvent e) {
		if(table.getSelectedColumn() == 1 || table.getSelectedColumn() ==2 || table.getSelectedColumn() ==3){
			//			logger.debug("\nSelected [row, column]: [" + table.getSelectedRow() + ", " + table.getSelectedColumn() + "]  in Remove Redundant Links Table");
			//Select action for Child name and Parent name columns  
			//selects term in all components corresponding to the column in focus
			if(table.getSelectedColumn() == 1 || table.getSelectedColumn() == 2){
				//				logger.debug("value in selected column:  " + table.getValueAt(table.getSelectedRow(), table.getSelectedColumn()));
				final Object colobj = table.getValueAt(table.getSelectedRow(), table.getSelectedColumn());					
				SelectionManager.selectTerm(table, (LinkedObject) colobj);
			}
			//Select link and show explanation in explanation component
			if(table.getSelectedColumn() ==3){
				Link rowobj = links.get(table.getSelectedRow());
				SelectionManager.selectLink(table, rowobj, false);
			}
		}
	}

	public void displayResults(){
		Object[][] data = new Object[redundantLinks.size()][COLUMN_COUNT];		
		int i=0;
		for(Link link : redundantLinks){
			data[i][0]= Boolean.FALSE;
			data[i][1]= link.getChild();
			data[i][2]= link.getParent();
			data[i][3] = expIcon;
			i++;
		}
		final String[] columnToolTips = {
				"Select Links to Delete",
				"Sort table alphabetically on Child Name",
				"Sort table alphabetically on Parent Name", 
				"Highlight link and display additional information in the Explanation Component",
		};

		model = new RemoveRedundantLinksModel(data);

		if(table == null){
			table = new JTable(model){
				//alternate row coloring 
				public Component prepareRenderer
				(TableCellRenderer renderer,int Index_row, int Index_col) {
					Component comp = super.prepareRenderer(renderer, Index_row, Index_col);

					if(System.getProperty("os.name").startsWith("Win")){

						comp.setBackground(Color.white);
						comp.setForeground(Color.black);

					} else {

						if(isCellSelected(Index_row, Index_col)){
							comp.setBackground(Color.blue);
							comp.setForeground(Color.white);

						}
						//even index, selected or not selected
						else if (Index_row % 2 == 0) {
							comp.setBackground(LIGHT_BLUE);
							comp.setForeground(Color.black);
						} 
						else {
							comp.setBackground(Color.white);
							comp.setForeground(Color.black);
						}
					}
					return comp;
				}
				//table header tool tips
				protected JTableHeader createDefaultTableHeader() {
					return new JTableHeader(columnModel) {
						public String getToolTipText(MouseEvent e) {
							java.awt.Point p = e.getPoint();
							int index = columnModel.getColumnIndexAtX(p.x);
							int realIndex = columnModel.getColumn(index).getModelIndex();
							return columnToolTips[realIndex];
						}
					};
				}
			};			
		} 
		else {
			table.setModel(model);
		}

		table.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.getSelectionModel().addListSelectionListener (this);
		table.setDragEnabled(false);
		table.setCellSelectionEnabled(true);

		//sort columns by clicking on header
		JTableHeader header = table.getTableHeader();
		header.setUpdateTableInRealTime(true);
		header.addMouseListener(model.new ColumnListener(table));
		header.setReorderingAllowed(true);

		//clicking on header for Select column does a SelectAll 
		final TableColumn tc0 = table.getColumnModel().getColumn(0);  
		tc0.setCellEditor(table.getDefaultEditor(Boolean.class));  
		tc0.setCellRenderer(table.getDefaultRenderer(Boolean.class));  
		tc0.setHeaderRenderer(new CheckBoxHeader(new MyItemListener()));

		table.addMouseListener(new MyMouseListener());
		if(sp == null){
			sp = new JScrollPane();
			sp.getViewport().add(table);
			northPanel.add(sp, BorderLayout.CENTER);
		}
		else
			table.repaint();
	}

	//	private void sortAllRowsBy(Object[][] tableData, AssertedLinksModel model, int colIndex, boolean ascending) {
	//		Arrays.sort(tableData, new ColumnSorter(colIndex, ascending));
	//		model.fireTableStructureChanged();
	//	}

	// object comparator
	class ColumnSorter implements Comparator {
		int colIndex;
		boolean ascending;
		ColumnSorter(int colIndex, boolean ascending) {
			this.colIndex = colIndex;
			this.ascending = ascending;
		}
		public int compare(Object o1, Object o2) {
			Object[] v1 = (Object[]) o1;
			Object[] v2 = (Object[]) o2;

			Object compVal1 = v1[colIndex];
			Object compVal2 = v2[colIndex];

			// Treat empty strains like nulls
			if (compVal1 instanceof String && ((String)compVal1).length() == 0) {
				compVal1 = null;
			}
			if (compVal2 instanceof String && ((String)compVal2).length() == 0) {
				compVal2 = null;
			}

			// Sort nulls so they appear last, regardless
			// of sort order
			if (compVal1 == null && compVal2 == null) {
				return 0;
			} else if (compVal1 == null) {
				return 1;
			} else if (compVal2 == null) {
				return -1;
			} else if (compVal1 instanceof Comparable) {
				if (ascending) {
					return ((Comparable)compVal1).compareTo(compVal2);
				} else {
					return ((Comparable)compVal2).compareTo(compVal1);
				}
			} else {
				if (ascending) {
					return compVal1.toString().compareTo(compVal2.toString());
				} else {
					return compVal2.toString().compareTo(compVal1.toString());
				}
			}
		}
	}

	class MyItemListener implements ItemListener{   
		public void itemStateChanged(final ItemEvent e){  
			final boolean checked = e.getStateChange() == ItemEvent.SELECTED;   
			for(int x = 0, y = table.getRowCount(); x < y; x++){   
				table.setValueAt(new Boolean(checked),x,0);  
			}
		}   
	}

	class MyMouseListener extends MouseAdapter{  
		public void mouseClicked(final MouseEvent mouseEvent) {
			int checkedCount = 0;  
			selectAll.removeItemListener(it);  
			if (selectAll instanceof JCheckBox) {  
				selectFlags = new boolean[table.getRowCount()];  
				for (int i = 0; i < table.getRowCount(); i++) {  
					selectFlags[i] = ((Boolean) table.getValueAt(i, 0)).booleanValue();
					//logger.debug("Flag[" + i + "]: " + selectFlags[i]);
					if(selectFlags[i]){  
						checkedCount++;  
					}  
				}
				for(int i=0; i<selectFlags.length;i++){
					if(!selectedIx.contains(i) && selectFlags[i] == true){
						selectedIx.add(i);
					}
				}
				if(checkedCount== table.getRowCount()){  
					selectAll.setSelected(true);                 
				}  
				if(checkedCount!= table.getRowCount()){  
					selectAll.setSelected(false);      
				}  
			}  
			selectAll.addItemListener(it);  
			table.getTableHeader().repaint();  
		}  
	}  

	class CheckBoxHeader extends JCheckBox implements TableCellRenderer, MouseListener {   
		private static final long serialVersionUID = 1L;
		protected int column;   
		protected boolean mousePressed = false;  
		ItemListener it1; 
		public CheckBoxHeader(final ItemListener itemListener) {   
			setSelectAllComponent(this);
			this.it1 = itemListener;
			selectAll.addItemListener(it1);
		}	  
		public Component getTableCellRendererComponent(final JTable table, final Object value,final boolean isSelected, final boolean hasFocus, final int row, final int column) {
			if (table != null) {   
				final JTableHeader header = table.getTableHeader();  
				if (header != null) {  
					selectAll.setForeground(header.getForeground());   
					selectAll.setBackground(header.getBackground());   
					selectAll.setFont(header.getFont());
					header.addMouseListener(selectAll);  
				}   
			}
			setColumn(column);
			setBorder(UIManager.getBorder("TableHeader.cellBorder"));   
			return selectAll;   
		}   
		protected void setColumn(final int column) {   
			this.column = column;   
		}   
		public int getColumn() {   
			return column;   
		}   
		protected void handleClickEvent(final MouseEvent e) {   
			if (mousePressed) {   
				mousePressed=false; 	      
				final JTableHeader header = (JTableHeader)(e.getSource());   
				final JTable tableView = header.getTable();   
				final TableColumnModel columnModel = tableView.getColumnModel();
				final int viewColumn = columnModel.getColumnIndexAtX(e.getX());   
				final int column = tableView.convertColumnIndexToModel(viewColumn);
				if (viewColumn == this.column && e.getClickCount() == 1 && column != -1) {  
					doClick();
				} 
			}   
		}   
		public void mouseClicked(final MouseEvent e) {
			handleClickEvent(e);   
			((JTableHeader)e.getSource()).repaint();   
		}   
		public void mousePressed(final MouseEvent e) {   
			mousePressed = true;	
		}   
		public void mouseReleased(final MouseEvent e) {   
		}   
		public void mouseEntered(final MouseEvent e) {   
		}   
		public void mouseExited(final MouseEvent e) {   
		} 
	}


	public void setSelectAllComponent(final CheckBoxHeader selectAll) {
		selectAll.setText("Select All");
		this.selectAll = selectAll;
	}

	public String getName() {
		return "Remove Redundant Links Panel";
	}

}


