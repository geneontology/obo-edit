package org.oboedit.gui;

import java.awt.Color;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.Collections;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.ListCellRenderer;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.JListErrorDecorator.ListRendererWrapper;
import org.oboedit.util.TextEditUtil;
import org.oboedit.verify.CheckWarning;

import org.apache.log4j.*;

public class JTableErrorDecorator implements ErrorDecorator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(JTableErrorDecorator.class);

	protected FieldPathSpec spec;

	protected OBOTextEditComponent parent;

	protected MultiMap<FieldPath, CheckWarning> warnings = new MultiHashMap<FieldPath, CheckWarning>();

	protected JTable list;

	protected class TableRendererWrapper implements TableCellRenderer {
		protected TableCellRenderer r;

		public TableRendererWrapper(TableCellRenderer r) {
			this.r = r;
		}

		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			boolean error = false;
			StringBuffer tooltip = new StringBuffer("<html><ul>");
			if (warnings != null) {
				for (CheckWarning w : warnings.singleValues()) {
					Object warningVal = w.getPath().getValueAt(spec);
					if (warningVal != null && warningVal.equals(value)) {
						error = true;
						tooltip.append("<li>" + w.getMessage());
					}
				}
			}
			tooltip.append("</ul></html>");
			if (error && r instanceof DefaultTableCellRenderer) {
				((DefaultTableCellRenderer) r).setForeground(Color.red);
				table.setSelectionForeground(Color.red);
			} else {
				table.setSelectionForeground(Color.black);
				((DefaultTableCellRenderer) r).setForeground(Color.black);
			}
			Component out = r.getTableCellRendererComponent(table, value,
					isSelected, hasFocus, row, column);
			if (error && out instanceof JComponent)
				((JComponent) out).setToolTipText(tooltip.toString());
			else
				((JComponent) out).setToolTipText(null);

			return out;
		}

	}

	protected TableModelListener listener = new TableModelListener() {

		public void tableChanged(TableModelEvent e) {
			TextEditUtil.addDirtyPaths(parent, getPaths());
		}

	};

	public Collection<FieldPath> getPaths() {
		if (parent.getObject() == null)
			return Collections.emptySet();
		IdentifiedObject object = (IdentifiedObject) parent.getObject().clone();
		parent.populateFields(object);
		FieldPath queryPath = FieldPathSpec.createQueryPath(spec, object);
		Collection<FieldPath> out = queryPath.resolve();
		return out;
	}

	public JTableErrorDecorator(FieldPathSpec spec,
			OBOTextEditComponent parent, JTable list) {
		this.spec = spec;
		this.parent = parent;
		this.list = list;
		list.getModel().addTableModelListener(listener);
		list.setDefaultRenderer(Object.class, new TableRendererWrapper(list
				.getDefaultRenderer(Object.class)));
	}

	public void cleanup() {
		list.getModel().removeTableModelListener(listener);
		list.setDefaultRenderer(Object.class, ((TableRendererWrapper) list
				.getDefaultRenderer(Object.class)).r);
	}
	
	public void clearWarnings() {
		warnings.clear();
	}

	public void setWarnings(FieldPath path, Collection<CheckWarning> warnings) {
		this.warnings.put(path, warnings);
		list.repaint();
	}
}
