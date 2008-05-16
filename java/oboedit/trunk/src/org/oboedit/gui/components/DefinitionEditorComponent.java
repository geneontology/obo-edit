package org.oboedit.gui.components;

import java.awt.*;
import java.util.*;

import javax.swing.*;
import javax.swing.border.*;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.filters.DefinitionDbxrefSearchCriterion;
import org.obo.filters.DefinitionSearchCriterion;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.RootTextEditComponent;

import org.apache.log4j.*;

public class DefinitionEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefinitionEditorComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected JTextPane defField = new JTextPane();

	protected JScrollPane defScroller = new JScrollPane(defField,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected DefinitionDbxrefEditorComponent defDbxrefList = new DefinitionDbxrefEditorComponent();

	protected Vector dbxrefs = new Vector();

	protected Border lineBorder = new LineBorder(Color.black);

	/*
	 * DropListener dropListener = new DropAdapter() { @Override public boolean
	 * allowDrop(DragEvent e) { if (e.getData() instanceof Vector) { Vector v =
	 * (Vector) e.getData(); for (int i = 0; i < v.size(); i++) { if
	 * (!(v.elementAt(i) instanceof Dbxref)) return false; } return true; } else
	 * if (e.getData() instanceof Object[]) { Object[] v = (Object[])
	 * e.getData(); for (int i = 0; i < v.length; i++) { if (!(v[i] instanceof
	 * Dbxref)) return false; } return true; } return false; }
	 * 
	 * @Override public void dragEnter(DragEvent e) {
	 * defDbxrefList.setBorder(lineBorder); }
	 * 
	 * @Override public void dragExit(DragEvent e) {
	 * defDbxrefList.setBorder(null); }
	 * 
	 * @Override public void drop(DragEvent e) { defDbxrefList.setBorder(null);
	 * if (e.getData() instanceof Collection) { Iterator it = ((Collection)
	 * e.getData()).iterator(); while (it.hasNext()) { Dbxref s = (Dbxref)
	 * ((Dbxref) it.next()).clone(); s.setType(Dbxref.DEFINITION); if
	 * (!dbxrefs.contains(s)) { dbxrefs.add(s);
	 * defDbxrefList.setListData(dbxrefs); } } } else if (e.getData() instanceof
	 * Object[]) { Object[] v = (Object[]) e.getData(); for (int i = 0; i <
	 * v.length; i++) { Dbxref s = (Dbxref) ((Dbxref) v[i]).clone();
	 * s.setType(Dbxref.DEFINITION); if (!dbxrefs.contains(s)) { dbxrefs.add(s);
	 * defDbxrefList.setListData(dbxrefs); } } } } }; protected DropTarget
	 * dropTarget = new DropTarget(defDbxrefList, dropListener);
	 */

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("field"))
			return defScroller;
		else if (id.equals("ref_list"))
			return defDbxrefList;
		else
			return super.resolveName(id, props, xml);
	}

	public DefinitionEditorComponent() {
		// setCheck(new DefinitionCheck());
		// setComponent(defField);
	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<panel>\n"
				+ "   <center>\n"
				+ "      <grid cols=\"2\">\n"
				+ "         <component id=\"field\" bordertitle=\"Definition\"/>\n"
				+

				"               <component id=\"ref_list\"/>\n" +

				"      </grid>\n" + "   </center>\n" + "</panel>";
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null && currentObject instanceof DefinedObject) {

			DefinedObject defined = (DefinedObject) currentObject;
			defField.setText(defined.getDefinition());
			dbxrefs.clear();
			defDbxrefList.setObject(defined);
			/*
			 * Iterator it = defined.getDefDbxrefs().iterator(); while
			 * (it.hasNext()) { Dbxref dbxref = (Dbxref) it.next();
			 * dbxrefs.add((dbxref).clone()); } //
			 * dbxrefs.addAll(defined.getDefDbxrefs());
			 * Collections.sort(dbxrefs, Dbxref.COMPARATOR); defDbxrefList.set
			 */
			boolean enable = !TermUtil.isObsolete(currentObject);
			defField.setEnabled(enable);
			if (enable)
				defField.setForeground(Color.black);
			else
				defField.setForeground(Color.gray);
			defDbxrefList.setEnabled(enable);
		} else {
			defField.setText("<no selection>");
			defDbxrefList.setObject(null);
			defDbxrefList.setEnabled(false);
			defField.setEnabled(false);
		}
	}

	@Override
	public void init() {
		super.init();
		defDbxrefList.init();
	}

	@Override
	public void cleanup() {
		defDbxrefList.cleanup();
		super.cleanup();
	}

	@Override
	public void setRoot(RootTextEditComponent root) {
		super.setRoot(root);
		defDbxrefList.setRoot(root);
	}

	@Override
	protected void initializeGUI() {
		TitledBorder border = new TitledBorder("Dbxrefs");
		defDbxrefList.setBorder(border);
	}

	public String getText() {
		return defField.getText().trim();
	}

	public String getID() {
		return "DEFINITION_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		DefinedObject defined = (DefinedObject) io;
		defined.setDefinition(getText());
		defDbxrefList.populateFields(io);
	}

	@Override
	protected void installListeners() {
		super.installListeners();
		getRoot().addMapping(
				new FieldPathSpec(DefinitionSearchCriterion.CRITERION), this,
				defField);

	}

	@Override
	protected void uninstallListeners() {
		super.uninstallListeners();
		getRoot().removeMapping(
				new FieldPathSpec(DefinitionSearchCriterion.CRITERION),
				defField);
	}

	public java.util.List<HistoryItem> getChanges() {
		if (currentObject != null && currentObject instanceof DefinedObject) {
			DefinedObject defined = (DefinedObject) currentObject;
			LinkedList<HistoryItem> out = new LinkedList<HistoryItem>();
			if (!ObjectUtil.equals(defined.getDefinition(), getText())) {
				DefinitionChangeHistoryItem ditem = new DefinitionChangeHistoryItem(
						defined, getText());
				out.add(ditem);
			}
			out.addAll(defDbxrefList.getChanges());
			return out;
		} else
			return Collections.emptyList();
	}

}
