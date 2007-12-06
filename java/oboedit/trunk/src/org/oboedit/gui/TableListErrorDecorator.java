package org.oboedit.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

import org.bbop.swing.widget.TableList;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.history.HistoryItem;
import org.obo.history.SingleTermOperationModel;
import org.oboedit.controller.SessionManager;
import org.oboedit.util.TextEditUtil;
import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.HistoryQuickFix;
import org.oboedit.verify.ImmediateQuickFix;
import org.oboedit.verify.QuickFix;
import org.oboedit.verify.TextReplaceQuickFix;

import sun.reflect.ReflectionFactory.GetReflectionFactoryAction;

public class TableListErrorDecorator extends
		AbstractErrorDecorator<TableList<?>> {

	protected TableModelListener listener;

	protected class TableRendererWrapper implements TableCellRenderer {
		protected TableCellRenderer r;
		protected Icon oldIcon;

		public TableRendererWrapper(TableCellRenderer r) {
			this.r = r;
		}

		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			boolean error = false;
			StringBuffer tooltip = new StringBuffer("<html><ul>");
			Collection<QuickFix> fixes = new ArrayList<QuickFix>();
			if (warnings != null) {
				for (CheckWarning w : warnings.singleValues()) {
					Object warningVal = w.getPath().getValueAt(spec);
					if (warningVal != null && warningVal.equals(value)) {
						error = true;
						tooltip.append("<li>" + w.getMessage());
						fixes.addAll(w.getFixes());
					}
				}
			}
			tooltip.append("</ul></html>");
			if (error && r instanceof DefaultTableCellRenderer) {
				((DefaultTableCellRenderer) r).setForeground(Color.red);
				if (fixes.size() > 0)
					((DefaultTableCellRenderer) r).setIcon(Preferences
							.loadLibraryIcon("quickfix.gif"));
				table.setSelectionForeground(Color.red);
			} else {
				table.setSelectionForeground(Color.black);
				((DefaultTableCellRenderer) r).setForeground(Color.black);
				((DefaultTableCellRenderer) r).setFont(Preferences
						.getPreferences().getFont());
				((DefaultTableCellRenderer) r).setIcon(null);
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

	protected MouseListener mouseListener;

	public TableListErrorDecorator(FieldPathSpec spec,
			OBOTextEditComponent parent, TableList<?> c) {
		super(spec, parent, c);
	}

	protected void applyFix(QuickFix action) {
		if (action instanceof ImmediateQuickFix) {
			((ImmediateQuickFix) action).run();
		} else if (action instanceof HistoryQuickFix) {
			IdentifiedObject io = (IdentifiedObject) parent.getObject().clone();
			parent.populateFields(io);
			SingleTermOperationModel model = new SingleTermOperationModel(
					SessionManager.getManager().getSession(), io);
			HistoryItem item = ((HistoryQuickFix) action).getItem();
			model.apply(item);
			parent.setObject(io);
			component.requestFocus();
		}
		TextEditUtil.addDirtyPaths(parent, getPaths());
	}

	@Override
	public void init() {
		mouseListener = new MouseAdapter() {

			@Override
			public void mousePressed(MouseEvent e) {
				if (SwingUtilities.isRightMouseButton(e)) {
					Object obj = component.getItemAt(e.getPoint());
					for (FieldPath path : warnings.keySet()) {
						if (path.containsValue(obj)) {
							Collection<CheckWarning> warningList = warnings
									.get(path);
							Collection<QuickFix> fixes = new ArrayList<QuickFix>();
							for (CheckWarning warning : warningList) {
								fixes.addAll(warning.getFixes());
							}
							if (fixes.size() > 0) {
								JPopupMenu menu = new JPopupMenu();
								for (final QuickFix f : fixes) {
									JMenuItem item = new JMenuItem(f.getDesc());
									item
											.addActionListener(new ActionListener() {
												public void actionPerformed(
														ActionEvent e) {
													applyFix(f);
												};
											});
									menu.add(item);
								}
								menu.show(component, e.getX(), e.getY());
							}
						}
					}
				}
			}
		};

		listener = new TableModelListener() {

			public void tableChanged(TableModelEvent e) {
				component.removeTableModelListener(listener);
				Collection<FieldPath> paths = getPaths();
				TextEditUtil.addDirtyPaths(parent, paths);
				component.addTableModelListener(listener);
			}

		};
		component.addTableModelListener(listener);
		component
				.setRenderer(new TableRendererWrapper(component.getRenderer()));
		component.addMouseListener(mouseListener);
	}

	public void cleanup() {
		component.removeTableModelListener(listener);
		component
				.setRenderer(((TableRendererWrapper) component.getRenderer()).r);
		component.removeMouseListener(mouseListener);
	}
}
