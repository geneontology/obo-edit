package org.oboedit.gui.actions;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.tree.TreePath;

import org.bbop.swing.KeyRecorder;
import org.obo.filters.*;
import org.obo.util.FilterUtil;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

public class ApplyFilterAction implements InputHandlerI {

	protected JPopupMenu dropMenu = new JPopupMenu();

	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof FilterPair)
			return ACCEPT_DROP;
		else
			return REJECT_DROP;
	}

	public boolean drop(final JComponent c, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker) {
		if (o instanceof FilterPair && c instanceof Filterable) {
			final FilterPair filter = (FilterPair) o;
			dropMenu.removeAll();

			if (c instanceof Filterable) {
				final Filterable dropPanel = (Filterable) c;
				JMenuItem filterItem = new JMenuItem("Set term filter");
				filterItem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						dropPanel.setFilter(filter);
					}
				});
				dropMenu.add(filterItem);
			}

			if (c instanceof FilteredRenderable) {
				final FilteredRenderable dropPanel = (FilteredRenderable) c;
				JMenuItem renderItem = new JMenuItem("Add renderer");
				renderItem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						GUIUtil.addRendererPair(dropPanel, filter);
					}
				});

				dropMenu.add(renderItem);
			}

			dropMenu.show(c, (int) p.getX(), (int) p.getY());
			return true;
		} else
			return false;
	}

	public boolean click(JComponent panel, GestureTarget dest, MouseEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		return false;
	}

	public boolean press(JComponent panel, KeyEvent e,
			KeyRecorder.KeyChecker keyChecker) {
		return false;
	}

	public String getDragDesc() {
		return "Applying Filter";
	}
}
