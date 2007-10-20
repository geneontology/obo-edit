package org.oboedit.gui.components;

import org.obo.annotation.datamodel.Annotation;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.obo.util.AnnotationUtil;
import org.obo.util.IDUtil;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.util.TinySet;


import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Set;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.Dimension;
import java.net.URL;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableModel;



public class AnnotationSummaryComponent extends AbstractGUIComponent {

	private static final long serialVersionUID = -7919246476674947971L;



	protected JTable asTable;
	protected JComboBox relationChooser = new JComboBox();
	protected JCheckBox checkBoxForIsA = null;
	protected JCheckBox checkBoxForAll = null;

	protected JEditorPane AnnotationSummaryPane = new JEditorPane("text/html",
			"<html></html>");

	protected JEditorPane referencePane = new JEditorPane("text/html",
			"<html></html>");

	protected HyperlinkListener linkListener = new HyperlinkListener() {
		public void hyperlinkUpdate(HyperlinkEvent e) {
			if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
				selectTerm(e.getURL(), SessionManager.getManager().getSession());
			}
		}
	};

	protected SelectionManager selectionManager = SelectionManager.getManager();

	protected SelectionListener termSelectListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			update();
		}
	};

	public void selectTerm(URL url, OBOSession history) {
		String id = url.getPath();
		OBOClass t = (OBOClass) history.getObject(id);
		List<LinkedObject> terms = new LinkedList<LinkedObject>();
		terms.add(t);
		SelectionManager.setGlobalSelection(SelectionManager
				.createSelectionFromTerms(null, terms, null, true));
	}

	public AnnotationSummaryComponent(String id) {
		super(id);
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

		referencePane.addHyperlinkListener(linkListener);
		AnnotationSummaryPane.addHyperlinkListener(linkListener);
	}

	@Override
	public void init() {
		selectionManager.addSelectionListener(termSelectListener);

		update();
	}

	protected void update() {
		removeAll();
		Selection gs = SelectionManager.getGlobalSelection();
		OBOSession session = SessionManager.getManager().getSession();
		

		AnnotationSummaryTableModel asTableModel = new AnnotationSummaryTableModel(session);
		
				
		System.out.println("making asTable");
		// create a JTable with per-cell tooltips
		asTable = new JTable(asTableModel) {
			public String getToolTipText(MouseEvent e) {
				java.awt.Point p = e.getPoint();

				int rowIndex = rowAtPoint(p);
				int colIndex = columnAtPoint(p);

				int realColumnIndex = convertColumnIndexToModel(colIndex);

				TableModel model = getModel();
				LinkedObject lo = (LinkedObject)model.getValueAt(rowIndex,colIndex);

				// TODO: provide more info in  tooltip
				if (lo == null) {
					return "";
				}
				else {
					StringBuffer out = new StringBuffer(); 
					IdentifiedObject xpGenus = ReasonerUtil.getGenus((OBOClass)lo);
					Collection<Link> xpDiffs = ReasonerUtil.getDifferentia((OBOClass)lo);
					out.append("<html>");
					out.append("Term: [" + lo.getID() + "] " + lo + "<br>");
					out.append("Genus: " + xpGenus + "<br>");
					out.append("Discriminating Properties:<br><ul>");
		
					for (Link xpDiff : xpDiffs) {
						LinkedObject io = xpDiff.getParent();

						out.append("<li>" + xpDiff.getType().getName()+" "+io+"</li>");
					}
					//return lo.getName() + " "+lo.getID();
					out.append("</ul></html>");
					return out.toString();
				}
			}
		};
	    
		asTable.setShowGrid(true);
		int numCols = asTable.getColumnCount();
		for (int i = 0; i < numCols; i++) {
		    TableColumn column = asTable.getColumnModel().getColumn(i);
		       column.setPreferredWidth(150); 
		}
		asTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		asTable.setCellSelectionEnabled(true);
		
		// mouse clicks
		//asTable.getSelectionModel().addListSelectionListener(new asTableListener());

		// The Renderer highlights subsumption paths
		asTable.setDefaultRenderer(Object.class, new AnnotationSetCellRenderer());

		// Show genus terms in header
		JTableHeader asTableHeader = asTable.getTableHeader();

		JScrollPane asTableScrollPane = new JScrollPane(asTable,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		
		add(asTableScrollPane,"CENTER");

		validate();
		repaint();
	}
	
	
	
	public String objectHref(IdentifiedObject io) {
		return "<a href='file:" + io.getID()
				+ "'>" + io + " (" + io.getID()
				+ ")</a>";
	}

	@Override
	public void cleanup() {
		selectionManager.removeSelectionListener(termSelectListener);
	}

	@Override
	public String getName() {
		return "Cross Product Matrix Editor Plugin";
	}
	
	private JButton addButton(String name,ActionListener al,JPanel parent) {
		JButton button = new JButton(name);
		button.setActionCommand(name);
		button.addActionListener(al);
		parent.add(button);
		return button;
	}
	
	

	public class AnnotationSummary {
		protected Collection<Annotation> directAnnotations;
		protected Collection<Annotation> transitiveAnnotations;

		public Collection<Annotation> getDirectAnnotations() {
			return directAnnotations;
		}
		public void setDirectAnnotations(Collection<Annotation> directAnnotations) {
			this.directAnnotations = directAnnotations;
		}
		public Collection<Annotation> getTransitiveAnnotations() {
			return transitiveAnnotations;
		}
		public void setTransitiveAnnotations(
				Collection<Annotation> transitiveAnnotations) {
			this.transitiveAnnotations = transitiveAnnotations;
		}
		AnnotationSummary() {
			directAnnotations = new HashSet<Annotation>();
			transitiveAnnotations = new HashSet<Annotation>();
		}
	}
	
	/**
	 * @author cjm
	 *
	 */
	// TODO: DRY
	private class AnnotationSummaryTableModel extends AbstractTableModel {

		protected int rowCount;
		protected int columnCount; // number of column objects (excluding first rowname column)
		protected LinkedObject[] columnObjs;
		protected LinkedObject[] rowObjs;
		protected AnnotationSummary[][] matrix;
		//protected CellStatus[][] cellStatus;
		
		protected Map<LinkedObject, Object> obj2row = 
			new HashMap<LinkedObject, Object>();
		protected Map<LinkedObject, Integer> obj2col = 
			new HashMap<LinkedObject, Integer>();
		TinySet<LinkedObject> objSet = new TinySet<LinkedObject>();
		
		
		/**
		 * @param rowObjsIn
		 * genus terms
		 * 
		 * @param columnObjsIn
		 * differentium terms
		 */
		AnnotationSummaryTableModel(OBOSession session) {
			
			boolean isTransitiveIncluded = true;
			boolean isFilteredToMinimalSet = true; //TODO
			
			Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
			Collection<LinkedObject> subjs =  AnnotationUtil.getAnnotationSubjects(session);
			Collection<LinkedObject> objs =  AnnotationUtil.getAnnotationObjects(session);

			if (isTransitiveIncluded) {
				Collection<LinkedObject> ancs = new HashSet<LinkedObject>();
				for (LinkedObject obj : objs) {
					System.out.println("getting ancestors of "+obj);
					ancs.addAll(TermUtil.getAncestors(obj,true));
				}
				objs = ancs;
			}
			
			System.out.println("constructor called");
			rowObjs = objs.toArray(new LinkedObject[0]);
			//rowObjs = (LinkedObject[])rowObjsIn.toArray();
			//columnObjs = (LinkedObject[])columnObjsIn.toArray();
			columnObjs = subjs.toArray(new LinkedObject[0]);
					
			rowCount = objs.size();
			columnCount = subjs.size();
			System.out.println(rowCount + "/" + columnCount);
			
			HashMap<LinkedObject, Integer> hdr2rownum =
				new HashMap<LinkedObject, Integer>();
			HashMap<LinkedObject, Integer> hdr2colnum =
				new HashMap<LinkedObject, Integer>();

			for (int i=0; i< objs.size(); i++) {
				System.out.println("row: "+i+" ="+objs.toArray()[i]);
				hdr2rownum.put((LinkedObject) objs.toArray()[i], i);
			}
			for (int i=0; i< subjs.size(); i++) {
				System.out.println("col: "+i+" ="+subjs.toArray()[i]);
				hdr2colnum.put((LinkedObject) subjs.toArray()[i], i);
			}
			
			// initialize arrays. 
			// cols = genus cols+1 -- left col shows diff term
			// (we are using a JTable to show a matrix)
			matrix = new AnnotationSummary[rowCount][columnCount];
			//cellStatus = new CellStatus[rowCount][columnCount];
			
			for (Annotation annot : annots) {
				LinkedObject subj = annot.getSubject();
				LinkedObject obj = annot.getObject();
				System.out.println(subj+" -> "+obj);
				int row = hdr2rownum.get(obj);
				int col = hdr2colnum.get(subj);
				setCell(annot,row,col);
				
				for (LinkedObject objAnc : TermUtil.getAncestors(obj, true)) {
					System.out.println(obj + " < "+objAnc+" : "+row);
					row = hdr2rownum.get(objAnc);
					System.out.println(obj + " < "+objAnc+" : "+row);
					setCellTransitive(annot,row,col);
				}
			}
		}
		
		public int getRowCount() {
			System.out.println("rc= "+rowCount);
			return rowCount;
		}
		
		// total columns, including diff column
		public int getColumnCount() {
			return columnCount+1;
		}
		
		
		// table coords
		// row: commencing from 0
		// col: column 0 is the differentium
		//      col 1+ is the xp term
		public Object getValueAt(int row, int column) {
			System.out.println("getting "+row+" "+column);
			if (column == 0) {
				return rowObjs[row];
			}
			else {
				AnnotationSummary as = 
				 matrix[row][column-1];
				if (as == null) {
					return "";
				}
				else {
					return as.getTransitiveAnnotations().size()
					 + " ("+as.getDirectAnnotations().size()+")";
		
				}
			}
		}
		
		
		
		/**
		 * @param lo -- xp term
		 * @param prop -- relation
		 * @param row -- row
		 * @param column -- 0 is first xp term
		 */
		public void setCell(Annotation lo,
							int row, int column) {
			//System.out.println("adding "+row+" "+column+" = "+lo);
			if (matrix[row][column] == null) {
				matrix[row][column] = new AnnotationSummary();
			}
			matrix[row][column].getDirectAnnotations().add(lo);
			//cellStatus[row][column] = CellStatus.NORMAL;
			objSet.add(lo);
			obj2row.put(lo, row);
			obj2col.put(lo, column);
		}
		
		public void setCellTransitive(Annotation lo,
				int row, int column) {
//			System.out.println("adding "+row+" "+column+" = "+lo);
			if (matrix[row][column] == null) {
				matrix[row][column] = new AnnotationSummary();
			}
			matrix[row][column].getTransitiveAnnotations().add(lo);
//			cellStatus[row][column] = CellStatus.NORMAL;
			objSet.add(lo);
		}


		
		// xp coords
		public int getObjectRow(LinkedObject lo) {
			return (Integer)obj2row.get(lo);
		}
		
		// xp coords
		public int getObjectColumn(LinkedObject lo) {
			return (Integer)obj2col.get(lo);
		}
		
		public TinySet<LinkedObject> getAllObjects() {
			return objSet;
		}
		
		public LinkedObject getColumnObj(int col) {
			return columnObjs[col-1];
		}
		public LinkedObject getRowObj(int row) {
			return rowObjs[row];
		}
		
		public String getColumnName(int col) {
			if (col==0) {
				return "-";
			}
			else {
				//System.out.println("cn for "+col+" = "+columnObjs[col-1].getName());
				return columnObjs[col-1].getName();
			}
		}
		
		
	}

	private class AnnotationSetCellRenderer extends DefaultTableCellRenderer {

		// TODO: something interesting here
		public Component getTableCellRendererComponent(
				JTable table, Object value,
				boolean isSelected, boolean hasFocus,
				int row, int column) {
			
			// Differentium column
			if (column==0) {
				setBackground(Color.lightGray);
			}
			else {
				AnnotationSummaryTableModel tm = (AnnotationSummaryTableModel)table.getModel();
				//System.out.println("status="+tm.getStatusAt(row, column-1));
			
						setBackground(Color.white);
			}
			return 
				super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
			
		}
	}


}
