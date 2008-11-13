package org.oboedit.gui.components;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;

import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.bbop.swing.widget.TableList;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.filters.SynonymSearchCriterion;
import org.obo.filters.SynonymTextSearchCriterion;
import org.obo.history.*;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.RootTextEditComponent;
import org.oboedit.gui.SynonymTableCellEditor;

import org.apache.log4j.*;

public class SynonymEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymEditorComponent.class);

	protected static final Color reallyLightGray = new Color(230, 230, 230);

	public static final String[] SYNSCOPES = { "Related Synonym", "Exact Synonym",
			"Narrow Synonym", "Broad Synonym" };

	public final static FieldPathSpec spec = new FieldPathSpec(
			SynonymSearchCriterion.CRITERION);

	protected class SynonymTableRenderer extends DefaultTableCellRenderer {
		private Synonym previousSynonym;
		private Component previousTableCellRendererComponent;

		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			if (value instanceof Synonym) {
				Synonym synonym = (Synonym) value;
				// Avoid updating the label if it doesn't actually need to change
				if (synonym.equals(previousSynonym)) {
					return previousTableCellRendererComponent;
				}
			}
			JLabel out = (JLabel) super.getTableCellRendererComponent(table,
					value, isSelected, hasFocus, row, column);
			if (value instanceof Synonym) {
				configureLabel(table, out, (Synonym) value, row, isSelected);
				previousSynonym = (Synonym) value;
				previousTableCellRendererComponent = out;
			}
			// Need?
// 			if (out.getPreferredSize().height != table.getRowHeight(row)) {
// 				table.setRowHeight(row, out.getPreferredSize().height);
// 			}
			return out;
		}
	}

	protected void configureLabel(JTable table, JLabel out, Synonym synonym,
				      int index, boolean isSelected) {
		out.setOpaque(true);
		out.setBorder(new EmptyBorder(10, 10, 10, 10));
		out.setMinimumSize(new Dimension(table.getWidth(), 0));
		if (!isSelected) {
			if (index % 2 == 0)
				out.setBackground(reallyLightGray);
			else
				out.setBackground(Color.white);
		}
		
		StringBuffer s = new StringBuffer();
		s.append("<html><table border=0 cellpadding=0 cellspacing=0 width='"
				+ table.getWidth() + "'>");

		s.append("<tr valign=top><td>");
		s.append("<table>");
		s.append("<tr valign=top><td>" + synonym.getText() + "</td>");
		s.append("<tr valign=top><td>");
		s.append("Scope: <i>" + SYNSCOPES[synonym.getScope()] + "</i>");
		s.append("</td>");
		if (synonym.getSynonymType() != null) {
			s.append("<tr valign=top><td>");
			s.append("Type: <i>" + synonym.getSynonymType() + "</i>");
			s.append("</td>");
		} 		
		s.append("</table>");
		s.append("</td>");

		if (synonym.getXrefs().size() > 0) {
			s.append("<td width='33%'>");
			s.append("<center><i>Xrefs</i></center>");
			s.append("<hr>");
			s.append("<ul>");
			for (Dbxref d : synonym.getXrefs()) {
				s.append("<li> " + d.toString() + "\n");
			}
			s.append("</ul>");
			s.append("</td>");
		}
		s.append("</table></html>");
		out.setText(s.toString());
	}

	protected JButton addButton = new JButton("+");

	protected JButton removeButton = new JButton("-");

	protected TableList<Synonym> synonymList;

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		return null;
	}

	public SynonymEditorComponent() {
		synonymList = new TableList<Synonym>();
		synonymList.setEditor(new SynonymTableCellEditor());
		synonymList.setRenderer(new SynonymTableRenderer());
		synonymList.addSelectionListener(new ListSelectionListener() {

			public void valueChanged(ListSelectionEvent e) {
				removeButton.setEnabled(synonymList.getSelectedRowCount() > 0);
			}

		});

		setLayout(new BorderLayout());
		JScrollPane pane = new JScrollPane(synonymList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setOpaque(false);
		buttonPanel.setLayout(new GridLayout(1, 2));
		buttonPanel.add(addButton);
		buttonPanel.add(removeButton);
		addButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				addSynonym();
			}
		});
		removeButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				delSynonym();
			}
		});
		addButton.setToolTipText("Add new synonym");
		removeButton.setToolTipText("Remove selected synonym");
		add(pane, "Center");
		add(buttonPanel, "South");
	}

	protected void addSynonym() {
		synonymList.add();
	}

	protected void delSynonym() {
		synonymList.deleteSelectedRows();
	}

	@Override
	protected boolean useSubLayout() {
		return false;
	}

	@Override
	protected String getDefaultLayout() {
		return null;
	}

	@Override
	protected void loadGUI() {
		if (currentObject instanceof SynonymedObject) {
			synonymList
					.setData(((SynonymedObject) currentObject).getSynonyms());
			
		}
	}

	@Override
	protected void initializeGUI() {
	}

	public String getID() {
		return "SYNONYM_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		if (io instanceof SynonymedObject) {
			((SynonymedObject) io).getSynonyms().clear();
			((SynonymedObject) io).getSynonyms().addAll(getEditedSynonyms());
		}
	}

	protected Collection<Synonym> getEditedSynonyms() {
		return synonymList.getData();
	}

	public java.util.List getChanges() {
		if (currentObject != null && currentObject instanceof SynonymedObject) {
			SynonymedObject synonymed = (SynonymedObject) currentObject;
			java.util.List out = new LinkedList();
			Iterator it;

			it = synonymed.getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym ref = (Synonym) it.next();
				boolean found = false;
				Iterator it2 = getEditedSynonyms().iterator();
				while (it2.hasNext()) {
					Synonym eref = (Synonym) it2.next();
					if (ref.equals(eref)) {
						found = true;
						break;
					}
				}

				if (!found) {
					if (ref.getScope() != Synonym.RELATED_SYNONYM) {
						ChangeSynScopeHistoryItem sitem = new ChangeSynScopeHistoryItem(
								synonymed, ref, Synonym.RELATED_SYNONYM);
						out.add(sitem);
					}

					if (ref.getSynonymType() != null) {
						ChangeSynTypeHistoryItem titem = new ChangeSynTypeHistoryItem(
								synonymed, ref, null);
						out.add(titem);
					}

					Iterator it3 = ref.getXrefs().iterator();
					while (it3.hasNext()) {
						Dbxref xref = (Dbxref) it3.next();
						DelDbxrefHistoryItem ritem = new DelDbxrefHistoryItem(
								synonymed.getID(), xref, false, ref.getText());
						out.add(ritem);
					}

					DelSynonymHistoryItem item = new DelSynonymHistoryItem(
							synonymed.getID(), ref.getText());
					out.add(item);
				}
			}

			it = getEditedSynonyms().iterator();
			while (it.hasNext()) {
				Synonym ref = (Synonym) it.next();
				boolean found = false;
				Iterator it2 = synonymed.getSynonyms().iterator();
				while (it2.hasNext()) {
					Synonym eref = (Synonym) it2.next();
					if (ref.equals(eref)) {
						found = true;
						break;
					}
				}
				if (!found) {
					AddSynonymHistoryItem item = new AddSynonymHistoryItem(
							synonymed.getID(), ref.getText());
					out.add(item);

					Iterator it3 = ref.getXrefs().iterator();
					while (it3.hasNext()) {
						Dbxref xref = (Dbxref) it3.next();
						AddDbxrefHistoryItem ritem = new AddDbxrefHistoryItem(
								synonymed.getID(), xref, false, ref.getText());
						out.add(ritem);
					}

					if (ref.getSynonymType() != null) {
						ChangeSynTypeHistoryItem titem = new ChangeSynTypeHistoryItem(
								synonymed, ref, ref.getSynonymType());
						out.add(titem);
					}

					if (ref.getScope() != Synonym.RELATED_SYNONYM) {
						ChangeSynScopeHistoryItem sitem = new ChangeSynScopeHistoryItem(
								synonymed, ref, ref.getScope());
						out.add(sitem);
					}
				}
			}
			return out;
		} else
			return Collections.EMPTY_LIST;
	}

	@Override
	protected void installListeners() {
		super.installListeners();
		getRoot().addMapping(spec, this, synonymList);
		((SynonymTableCellEditor) synonymList.getEditor()).installMappings(this);

	}
	
	@Override
	protected void uninstallListeners() {
		super.uninstallListeners();
		getRoot().removeMapping(spec, synonymList);
		((SynonymTableCellEditor) synonymList.getEditor()).uninstallMappings(this);
	}
}
