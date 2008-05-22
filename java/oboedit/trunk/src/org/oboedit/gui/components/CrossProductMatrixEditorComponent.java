package org.oboedit.gui.components;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;


import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.TitledBorder;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.util.TinySet;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PathCapable;
import org.obo.history.CompletesHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.postcomp.PostcompUtil;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Selection;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;



import org.apache.log4j.*;

public class CrossProductMatrixEditorComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CrossProductMatrixEditorComponent.class);

	private static final long serialVersionUID = -7919246476674947971L;

	public enum CellStatus {
		SUBSUMED,
		SUBSUMER,
		NORMAL
	}


	protected JTable xpTable;
	protected JComboBox relationChooser = new JComboBox();
	protected JCheckBox checkBoxForIsA = null;
	protected JCheckBox checkBoxForAll = null;

	protected JEditorPane crossProductPane = new JEditorPane("text/html",
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

	public CrossProductMatrixEditorComponent(String id) {
		super(id);
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		TitledBorder rborder = new TitledBorder("Referenced by");

		TitledBorder cborder = new TitledBorder("Cross product defs");
		crossProductPane.setBorder(cborder);
		crossProductPane.setEditable(false);
		crossProductPane.setOpaque(false);

		crossProductPane.addHyperlinkListener(linkListener);
	}

	@Override
	public void init() {
		selectionManager.addSelectionListener(termSelectListener);

		update();
	}

	protected void update() {
		removeAll();
		Selection gs = SelectionManager.getGlobalSelection();
		
		// objs that have xp definitions
		TinySet<LinkedObject> xpObjs = new TinySet<LinkedObject>();
		
		// objs that form the genus of other xp definitions
		TinySet<LinkedObject> xpGenusObjs = new TinySet<LinkedObject>();
		HashMap<LinkedObject, LinkedObject> objByGenus = new HashMap<LinkedObject, LinkedObject>();
		
		// objs that form the differentium obj of other xp definitions
		// we also include *potential* differentia
		// (for making new xps)
		// any selected obj (that is not itself an xp?) goes here
		TinySet<LinkedObject> xpDiffObjs = new TinySet<LinkedObject>();
		HashMap<LinkedObject, LinkedObject> objByDiff = new HashMap<LinkedObject, LinkedObject>();

		// Top part of panel has relation chooser and maker button
		relationChooser = new JComboBox();
		relationChooser.addItem("DEFAULT");
		for (OBOProperty p : 
			TermUtil.getRelationshipTypes(SessionManager.getManager().getSession())) {
			if (!p.isBuiltIn()) {
				relationChooser.addItem(p);
			}
		}


		// build sets
		for (PathCapable io : gs.getAllSelectedObjects()) {			
			if (io instanceof LinkedObject &&
			    io instanceof OBOClass) {
				OBOClass lo = (OBOClass)io;
				if (TermUtil.isIntersection(lo)) {
					xpObjs.add(lo);
					xpGenusObjs.add(ReasonerUtil.getGenus(lo));
					for (Link linkUp : lo.getParents()) {
						LinkedObject parent = linkUp.getParent();
						if (TermUtil.isIntersection(linkUp)) {
							if (linkUp.getType().equals(OBOProperty.IS_A)) {
								xpGenusObjs.add(parent);
								objByGenus.put(parent,lo);
							}
							else {
								xpDiffObjs.add(parent);
								objByDiff.put(parent,lo);
							}
						}
					}
				}
				else {
					// treat as *potential* differentium
					// TODO: decide, do this for all?
					// we can have recursive differentia
					if (!xpGenusObjs.contains(lo)) {
						xpDiffObjs.add(lo);
					}
					
					for (Link linkDn : lo.getChildren()) {
						LinkedObject child = linkDn.getChild();
						if (TermUtil.isIntersection(linkDn)) {
							if (linkDn.getType().equals(OBOProperty.IS_A)) {
							}
							else {
								OBOClass g = ReasonerUtil.getGenus((OBOClass)child);
								xpGenusObjs.add(g);
								objByGenus.put(g,child);
								xpDiffObjs.add(lo);
								objByDiff.put(lo,child);
							}
						}
					}

				}
			}
		}

	
		// Initialize xp table model
		CrossProductTableModel xpTableModel = new CrossProductTableModel(xpDiffObjs,xpGenusObjs);
		Vector<LinkedObject> xpGenusVec = new Vector<LinkedObject>(xpGenusObjs);
		Vector<LinkedObject> xpDiffVec = new Vector<LinkedObject>(xpDiffObjs);
		
		// Populate the table model. Each cell is a cross-product
		for (int colNum=0; colNum<xpGenusVec.size(); colNum++) {
			
			LinkedObject g = xpGenusVec.elementAt(colNum);

			HashSet<LinkedObject> xpSet = new HashSet<LinkedObject>();
			
			// potential xps to fill in matrix. Must have matching genus
			for (Link link : g.getChildren()) {
				if (link.getType().equals(OBOProperty.IS_A) &&
							TermUtil.isIntersection(link)) {
					xpSet.add(link.getChild());
				}
			}
			for (int rowNum=0; rowNum<xpDiffVec.size(); rowNum++) {
				LinkedObject d = xpDiffVec.elementAt(rowNum);
				
				// find xps, starting with those matching genus
				for (LinkedObject xp : xpSet) {
					
					// all terms that use this differentium
					for (Link dlink : d.getChildren()) {
						if (dlink.getChild().getID().equals(xp.getID()) &&
								!dlink.getType().equals(OBOProperty.IS_A)) {
							OBOProperty prop = dlink.getType();
							xpTableModel.setCell(xp,prop,rowNum,colNum);
						}
					}
				}
				
			}
		}
		JPanel buttonPanel = new JPanel();
		
		
		ActionListener al = new MakeButtonActionListener();
		addButton("Make",al,buttonPanel);
		buttonPanel.add(new JLabel("xp relation "));
		buttonPanel.add(relationChooser);
		buttonPanel.add(new JLabel("assert:"));
		checkBoxForIsA = new JCheckBox("is_a");
		checkBoxForAll = new JCheckBox("all links");
		buttonPanel.add(checkBoxForIsA);
		buttonPanel.add(checkBoxForAll);
		
		logger.info("Creating xpTable");
		// create a JTable with per-cell tooltips
		xpTable = new JTable(xpTableModel) {
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
	    
		xpTable.setShowGrid(true);
		int numCols = xpTable.getColumnCount();
		for (int i = 0; i < numCols; i++) {
		    TableColumn column = xpTable.getColumnModel().getColumn(i);
		       column.setPreferredWidth(150); 
		}
		xpTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		xpTable.setCellSelectionEnabled(true);
		
		// mouse clicks
		xpTable.getSelectionModel().addListSelectionListener(new XPTableListener());

		// The Renderer highlights subsumption paths
		xpTable.setDefaultRenderer(Object.class, new XPCellRenderer());

		// Show genus terms in header
		JTableHeader xpTableHeader = xpTable.getTableHeader();

		JScrollPane xpTableScrollPane = new JScrollPane(xpTable,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		add(buttonPanel,"WEST");
		
		add(xpTableScrollPane,"CENTER");

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
	
	
	/**
	 * @author cjm
	 *
	 */
	private class CrossProductTableModel extends AbstractTableModel {

		protected int rowCount;
		protected int columnCount; // number of column objects (excluding first rowname column)
		protected LinkedObject[] columnObjs;
		protected LinkedObject[] rowObjs;
		protected LinkedObject[][] matrix;
		protected CellStatus[][] cellStatus;
		
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
		CrossProductTableModel(Collection<LinkedObject> rowObjsIn, Collection<LinkedObject> columnObjsIn) {
			logger.info("creating xpTableModel");
			rowObjs = rowObjsIn.toArray(new LinkedObject[0]);
			//rowObjs = (LinkedObject[])rowObjsIn.toArray();
			//columnObjs = (LinkedObject[])columnObjsIn.toArray();
			columnObjs = columnObjsIn.toArray(new LinkedObject[0]);
					
			rowCount = rowObjsIn.size();
			columnCount = columnObjsIn.size();
			
			logger.info(rowCount + "/" + columnCount);
			
			// initialize arrays. 
			// cols = genus cols+1 -- left col shows diff term
			// (we are using a JTable to show a matrix)
			matrix = new LinkedObject[rowCount][columnCount];
			cellStatus = new CellStatus[rowCount][columnCount];
		}
		
		public int getRowCount() {
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
			if (column == 0) {
				return rowObjs[row];
			}
			else {
				return matrix[row][column-1];
			}
		}
		
		// xp coords
		public void setStatusAt(int row, int column, CellStatus status) {
			cellStatus[row][column] = status;
		}
		
		// xp coords
		public CellStatus getStatusAt(int row, int column) {
			return cellStatus[row][column];
		}
		
		
		/**
		 * @param lo -- xp term
		 * @param prop -- relation
		 * @param row -- row
		 * @param column -- 0 is first xp term
		 */
		public void setCell(LinkedObject lo, OBOProperty prop, 
							int row, int column) {
			//logger.info("adding "+row+" "+column+" = "+lo);
			matrix[row][column] = lo;
			//cellStatus[row][column] = CellStatus.NORMAL;
			objSet.add(lo);
			obj2row.put(lo, row);
			obj2col.put(lo, column);
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
				//logger.info("cn for "+col+" = "+columnObjs[col-1].getName());
				return columnObjs[col-1].getName();
			}
		}
		
		
	}

	private class XPCellRenderer extends DefaultTableCellRenderer {

		public Component getTableCellRendererComponent(
				JTable table, Object value,
				boolean isSelected, boolean hasFocus,
				int row, int column) {
			
			// Differentium column
			if (column==0) {
				setBackground(Color.lightGray);
			}
			else {
				CrossProductTableModel tm = (CrossProductTableModel)table.getModel();
				//logger.info("status="+tm.getStatusAt(row, column-1));
			
				if (tm.getStatusAt(row, column-1) == CellStatus.SUBSUMED) {
					setBackground(Color.pink);
				}
				else if (tm.getStatusAt(row, column-1) == CellStatus.SUBSUMER) {
					setBackground(Color.yellow);
				}
				else {
					setBackground(Color.white);
				}
			}
			return 
				super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
			
		}
	}

	/** Listens to New, Copy & Delete buttons */
	private class MakeButtonActionListener implements ActionListener {

		// bug/issue - if last row is deleted should create new blank one...
		public void actionPerformed(ActionEvent e) {
			int[] selectedCols = xpTable.getSelectedColumns();
			int[] selectedRows = xpTable.getSelectedRows();
			//logger.info("pressed: cols="+selectedCols+" rows="+selectedRows);
			Object selectedRel = relationChooser.getSelectedItem();
			//logger.info("selected relation="+selectedRel);
			CrossProductTableModel tm = (CrossProductTableModel)xpTable.getModel();
			
			for (int colNum: selectedCols) {
				for (int rowNum: selectedRows) {
				
					LinkedObject genus = tm.getColumnObj(colNum);
					LinkedObject diffClass = tm.getRowObj(rowNum);
					
					String id = GUIUtil.fetchID(null);
					logger.info("new term with id="+id);
					TermMacroHistoryItem item = new TermMacroHistoryItem("Created new xp term");
							
					String typeID =  OBOClass.OBO_CLASS.getID();
					String relID = null;
					if (selectedRel instanceof OBOProperty) {
						relID = ((OBOProperty)selectedRel).getID();
					}
					else {
						for (int rowNumInner=0; rowNumInner <tm.rowCount; rowNumInner++) {
							OBOClass proto = (OBOClass)tm.getValueAt(rowNumInner, colNum);
							if (proto != null) {
								for (Link link : ReasonerUtil.getDifferentia(proto)) {
									relID = link.getType().getID();
									logger.info("default rel="+relID);
								}
							}
						}									
					}
					if (relID == null) {
						
					}
					else {
						logger.info("relID="+relID);
						item.addItem(new CreateObjectHistoryItem(id, typeID));
						item.addItem(new CreateLinkHistoryItem(id, relID, diffClass.getID()));
						item.addItem(new CreateLinkHistoryItem(id, "OBO_REL:is_a", genus.getID()));
//						item.addItem(new NameChangeHistoryItem("<new term>", id, id));
						SessionManager.getManager().apply(item);
						logger.info("applied "+item);


						item = new TermMacroHistoryItem("Setting to completes");	
						item.addItem(new CompletesHistoryItem(id, relID, diffClass.getID(), false));
						item.addItem(new CompletesHistoryItem(id, "OBO_REL:is_a", genus.getID(), false));
						SessionManager.getManager().apply(item);

						// give it a name
						LinkedObject newObj = 
							(LinkedObject)SessionManager.getManager().getSession().getObject(id);
						logger.info("newObj= "+newObj);
						String newName = PostcompUtil.getPostcompName(newObj, null, true);
						logger.info("newName= "+newName);

						item = new TermMacroHistoryItem("Named new xp term");
						item.addItem(new NameChangeHistoryItem(newName, id, id));
						SessionManager.getManager().apply(item);
						
						if (checkBoxForIsA.isSelected() ||
								checkBoxForAll.isSelected()) {
							item = new TermMacroHistoryItem("Auto-created links");
							ReasonedLinkDatabase reasoner = SessionManager.getManager().getReasoner();
							for (Link link : reasoner.getParents(newObj)) {
								if (TermUtil.isImplied(link) &&
										!ReasonerUtil.shouldBeTrimmed(reasoner, link)) {
									if (checkBoxForAll.isSelected() ||
											link.getType().equals(OBOProperty.IS_A)) {
										item.addItem(new CreateLinkHistoryItem(link));
									}
								}
							}
							SessionManager.getManager().apply(item);
						}
						
					}
				}
			}
			update();
			repaint();
		}
	} 
	
	private class XPTableListener implements ListSelectionListener {
		public void valueChanged(ListSelectionEvent e) {
			int[] selectedCols = xpTable.getSelectedColumns();
			int[] selectedRows = xpTable.getSelectedRows();
			CrossProductTableModel tm = (CrossProductTableModel)xpTable.getModel();
			TinySet<LinkedObject> objSet = tm.getAllObjects();
			ReasonedLinkDatabase reasoner = SessionManager.getManager().getReasoner();
			if (reasoner == null) {
				return;
			}
			//logger.info("pressed:" + e+" cols="+selectedCols+" rows="+selectedRows);
			for (int i: selectedCols) {
				for (int j: selectedRows) {
					
					
					/*
					 * TODO
					 * find all subsumed classes in xpTable
					 * highlight them
					 */
					LinkedObject lo = (LinkedObject)tm.getValueAt(j, i);
					for (LinkedObject subsumedObj : objSet) {
						int k = tm.getObjectColumn(subsumedObj);
						int l = tm.getObjectRow(subsumedObj);
						if (lo == null) {
							tm.setStatusAt(l,k,CellStatus.NORMAL);
						}
						else if (reasoner.isSubclassOf((OBOClass)subsumedObj,(OBOClass)lo)) {
							tm.setStatusAt(l,k,CellStatus.SUBSUMED);
						}
						else if (reasoner.isSubclassOf((OBOClass)lo,(OBOClass)subsumedObj)) {
							tm.setStatusAt(l,k,CellStatus.SUBSUMER);
						}
						else {
							tm.setStatusAt(l,k,CellStatus.NORMAL);
						}
					}
					
				}
			}
			repaint();
			
		}
	}


}
