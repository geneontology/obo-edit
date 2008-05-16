package org.oboedit.gui.components;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.bbop.util.TinySet;

import org.obo.annotation.datamodel.Annotation;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOSession;
import org.obo.util.AnnotationUtil;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Selection;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;



import org.apache.log4j.*;

public class AnnotationSummaryComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AnnotationSummaryComponent.class);

	private static final long serialVersionUID = -7919246476674947971L;


	protected JCheckBox checkBoxUseTransitive = new JCheckBox("Include ancestors");
	protected JCheckBox checkBoxConcise = new JCheckBox("use concise");

	protected JTable asTable;
	protected JComboBox relationChooser = new JComboBox();

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
		
				
		logger.info("making asTable");
		// create a JTable with per-cell tooltips
		asTable = new JTable(asTableModel) {
			public String getToolTipText(MouseEvent e) {
				java.awt.Point p = e.getPoint();

				int rowIndex = rowAtPoint(p);
				int colIndex = columnAtPoint(p);

				int realColumnIndex = convertColumnIndexToModel(colIndex);

				TableModel model = getModel();
				Object obj = model.getValueAt(rowIndex,colIndex);
				if (obj == null)
					return "--";
				else if (obj instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject)obj;

					StringBuffer out = new StringBuffer(); 
					out.append("<html>");
					out.append("Term: [" + lo.getID() + "] " + lo.getName() + "<br>");
					out.append("</html>");
					return out.toString();
				}
				else {
					return obj.toString();
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

		JPanel buttonPanel = new JPanel();
		
		checkBoxUseTransitive.addItemListener(
				new ItemListener() {
					public void itemStateChanged(ItemEvent e) {
						update();
					}
				}
				);
		checkBoxConcise.addItemListener(
				new ItemListener() {
					public void itemStateChanged(ItemEvent e) {
						update();
					}
				}
				);

		
		buttonPanel.add(checkBoxUseTransitive);
		buttonPanel.add(checkBoxConcise);

		add(buttonPanel,"NORTH");

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
		
		
		private class AnnotationSorter implements Comparator<Annotation> {
			AnnotationSorter() {
			}
			
			public int compare(Annotation a1, Annotation a2) {
				return a1.getNamespace().getID().compareTo(a2.getNamespace().getID());
			}
		}	
		
		private class ClassSorter implements Comparator<LinkedObject> {
			ClassSorter() {
			}
			
			public int compare(LinkedObject a1, LinkedObject a2) {
				if (a1 == null || a2 == null) {
					// TODO: this should never happen. Check in Phenote
					if (a1 == null && a2 == null)
						return 0;
					if (a1 == null)
						return -1;
					return 1;
				}
				String a1s = a1.getNamespace() == null ? "" : a1.getNamespace().getID();
				String a2s = a2.getNamespace() == null ? "" : a2.getNamespace().getID();
				
				return a1s.compareTo(a2s);
			}
		}	
		/**
		 * @param rowObjsIn
		 * genus terms
		 * 
		 * @param columnObjsIn
		 * differentium terms
		 */
		AnnotationSummaryTableModel(OBOSession session) {
			
			boolean isTransitiveIncluded = true;
			
			Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
			// Won't compile.  Temporarily changing it so we can compile.  Note that this is NOT
			// a real fix, as it will break the behavior.
//			Collection<LinkedObject> subjs =  AnnotationUtil.getAnnotationSubjects(session);
//			Collection<LinkedObject> objSet =  AnnotationUtil.getAnnotationObjects(session);
			Collection<LinkedObject> subjs =  new LinkedList<LinkedObject>();
			Collection<LinkedObject> objSet =  new LinkedList<LinkedObject>();

			logger.info("n_objs: "+objSet.size());
			logger.info("n_subjs: "+subjs.size());
			if (checkBoxUseTransitive.isSelected()) {
				Set<LinkedObject> objsWithAncs = new HashSet<LinkedObject>();
				for (LinkedObject obj : objSet) {
					//logger.info("  direct annotation to:"+obj.getID()+" :: "+obj);
					if (obj == null)
						continue;
					Collection<LinkedObject> ancs = TermUtil.getAncestors(obj,true);
						objsWithAncs.addAll(ancs);
				}
				objSet = objsWithAncs;
			}
			logger.info("n_objs (after including transitive): "+objSet.size());
			
			if (checkBoxConcise.isSelected()) {
				//MultiMap<LinkedObject, LinkedObject> obj2subj = 
				//	new MultiHashMap<LinkedObject, LinkedObject>();
				Map<LinkedObject, HashSet<LinkedObject>> obj2subj = 
					new HashMap<LinkedObject, HashSet<LinkedObject>>();
				
				// build annotation mapping; eg Phenotype to Genotype
				for (Annotation annot : annots)
					for (LinkedObject obj : TermUtil.getAncestors(annot.getObject()))
						if (obj2subj.containsKey(obj))
							obj2subj.get(obj).add(annot.getSubject());
						else
							obj2subj.put(obj, 
									new HashSet(Collections.singleton(annot.getSubject())));
				
				Collection<LinkedObject> filteredObjSet =  objSet;
				
				for (LinkedObject obj : objSet) {
					Set<LinkedObject> parentsAndChildren = new HashSet<LinkedObject>();
					for (Link link : obj.getParents())
						if (objSet.contains(link.getParent()))
							parentsAndChildren.add(link.getParent());
					for (Link link : obj.getChildren())
						if (objSet.contains(link.getChild()))
							parentsAndChildren.add(link.getChild());
					//logger.info(parentsAndChildren.size()+" ::: "+obj);
					Collection<LinkedObject> profile =
						obj2subj.get(obj);
						//AnnotationUtil.getSubjectsAnnotatedWithObject(session, obj);
					boolean isInformative = false; // only informative if a neighbour is different
					//logger.info("  profile="+profile);
					for (LinkedObject neighbor : parentsAndChildren) {
						Set<LinkedObject> neighborProfile =
							obj2subj.get(neighbor);
							//AnnotationUtil.getSubjectsAnnotatedWithObject(session, neighbor);
							//logger.info("      Nprofile="+neighborProfile+" "+neighbor);
						if (neighborProfile == null ||
								profile == null ||
								!neighborProfile.equals(profile))
							isInformative = true;
						//logger.info("      INF:"+isInformative);
					}
					if (parentsAndChildren.size() == 0)
						isInformative = true; // graph singletons are informative
					if (isInformative)
						filteredObjSet.add(obj);
				}
				objSet = filteredObjSet;
				logger.info("n_objs (uniformative nodes trimmed): "+objSet.size());

			}
			
			LinkedList<LinkedObject> objs = new LinkedList<LinkedObject>(objSet);
			Collections.sort(objs, new ClassSorter());
			//logger.info("sorted objs="+objs);
			rowObjs = objs.toArray(new LinkedObject[0]);
			//rowObjs = (LinkedObject[])rowObjsIn.toArray();
			//columnObjs = (LinkedObject[])columnObjsIn.toArray();
			columnObjs = subjs.toArray(new LinkedObject[0]);
					
			rowCount = objs.size();
			columnCount = subjs.size();
			logger.info(rowCount + "/" + columnCount);
			
			HashMap<LinkedObject, Integer> hdr2rownum =
				new HashMap<LinkedObject, Integer>();
			HashMap<LinkedObject, Integer> hdr2colnum =
				new HashMap<LinkedObject, Integer>();

			// fill edges of grid
			for (int i=0; i< objs.size(); i++) {
				//logger.info("row: "+i+" ="+((IdentifiedObject)objs.toArray()[i]).getID()+" :: "+objs.toArray()[i]);
				hdr2rownum.put((LinkedObject) objs.toArray()[i], i);
			}
			for (int i=0; i< subjs.size(); i++) {
				//logger.info("col: "+i+" ="+subjs.toArray()[i]);
				hdr2colnum.put((LinkedObject) subjs.toArray()[i], i);
			}
			
			// initialize arrays. 
			// cols = genus cols+1 -- left col shows diff term
			// (we are using a JTable to show a matrix)
			matrix = new AnnotationSummary[rowCount][columnCount];
			//cellStatus = new CellStatus[rowCount][columnCount];
			
			for (Annotation annot : annots) {
				//logger.info(annot);
				LinkedObject annotatedEntity = annot.getSubject();
				LinkedObject annotatedWithObject = annot.getObject();
				
				int row = hdr2rownum.get(annotatedWithObject); // classes
				int col = hdr2colnum.get(annotatedEntity); // annotated entities
				setCell(annot,row,col);
				//logger.info("  setting cell " + row + ","+col);
				
				if (checkBoxUseTransitive.isSelected()) {
					for (LinkedObject objAnc : TermUtil.getAncestors(annotatedWithObject, true)) {
						//logger.info("  T:"+row+" "+objAnc.getID()+" :: "+objAnc);
						if (hdr2rownum.containsKey(objAnc)) {
							row = hdr2rownum.get(objAnc);
							setCellTransitive(annot,row,col);
						}
						else {
							if (!checkBoxConcise.isSelected())
								logger.error("THIS SHOULD NOT HAPPEN!! "+objAnc);
						}
					}
				}
			}
			logger.info("made table model");
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
				AnnotationSummary as = 
				 matrix[row][column-1];
				if (as == null) {
					return "";
				}
				else {
					if (checkBoxUseTransitive.isSelected())
						return as.getTransitiveAnnotations().size()
						+ " ("+as.getDirectAnnotations().size()+")";
					else
						return as.getDirectAnnotations().size();
		
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
			//logger.info("adding "+row+" "+column+" = "+lo);
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
//			logger.info("adding "+row+" "+column+" = "+lo);
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
				//logger.info("cn for "+col+" = "+columnObjs[col-1].getName());
				LinkedObject co = columnObjs[col-1];
				if (co == null) {
					logger.error("nothing in col: "+col);
					return "";
				}
				else
					return co.getName();
			}
		}
		
		
	}

	private class AnnotationSetCellRenderer extends DefaultTableCellRenderer {

		// TODO: something interesting here
		public Component getTableCellRendererComponent(
				JTable table, Object value,
				boolean isSelected, boolean hasFocus,
				int row, int column) {
			
			// AnnotatedWith column
			if (column==0) {
				setBackground(Color.lightGray);
			}
			else {
				AnnotationSummaryTableModel tm = 
					(AnnotationSummaryTableModel)table.getModel();
				setBackground(Color.white);
			}
			return 
				super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
			
		}
	}


}
