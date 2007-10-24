package org.oboedit.gui;

import java.awt.Color;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.Collections;

import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.VerificationManager;
import org.oboedit.util.TextEditUtil;
import org.oboedit.verify.CheckWarning;

public class JListErrorDecorator implements ErrorDecorator {

	protected PropertyChangeListener listener = new PropertyChangeListener() {

		public void propertyChange(PropertyChangeEvent evt) {
			TextEditUtil.addDirtyPaths(parent, getPaths());
		}

	};

	protected FieldPathSpec spec;

	protected OBOTextEditComponent parent;

	protected MultiMap<FieldPath, CheckWarning> warnings = new MultiHashMap<FieldPath, CheckWarning>();

	protected JList list;

	protected class ListRendererWrapper implements ListCellRenderer {
		protected ListCellRenderer r;

		public ListRendererWrapper(ListCellRenderer r) {
			this.r = r;
		}

		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			boolean error = false;
			if (warnings != null) {
				for (CheckWarning w : warnings.singleValues()) {
					Object warningVal = w.getPath().getValueAt(spec);
					if (warningVal != null && warningVal.equals(value)) {
						error = true;
						break;
					}
				}
			}
			Component out = r.getListCellRendererComponent(list, value, index,
					isSelected, cellHasFocus);
			if (error)
				out.setForeground(Color.red);
			else
				out.setForeground(Color.black);
			return out;
		}

	}

	public void clearWarnings() {
		warnings.clear();
	}

	public Collection<FieldPath> getPaths() {
		if (parent.getObject() == null)
			return Collections.emptySet();
		IdentifiedObject object = (IdentifiedObject) parent.getObject().clone();
		parent.populateFields(object);
		FieldPath queryPath = FieldPathSpec.createQueryPath(spec, object);
		return queryPath.resolve();
	}

	public JListErrorDecorator(FieldPathSpec spec, OBOTextEditComponent parent,
			JList list) {
		this.spec = spec;
		this.parent = parent;
		this.list = list;
		list.addPropertyChangeListener(listener);
		list.setCellRenderer(new ListRendererWrapper(list.getCellRenderer()));
	}

	public void cleanup() {
		list.removePropertyChangeListener(listener);
		list.setCellRenderer(((ListRendererWrapper) list.getCellRenderer()).r);
	}

	public void setWarnings(FieldPath path, Collection<CheckWarning> warnings) {
		this.warnings.put(path, warnings);
		list.repaint();
	}

}
