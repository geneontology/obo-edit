package org.bbop.framework;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.ButtonGroup;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;

import org.bbop.framework.dock.Perspective;
import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.ObjectUtil;

public class LayoutMenu extends AbstractDynamicMenuItem {

	public LayoutMenu() {
		super("Layout");
	}

	public List<Component> getItems() {
	    List<Component> out = new ArrayList<Component>();
	            ButtonGroup group = new ButtonGroup();
	            for (final Perspective perspective : ComponentManager
	                    .getManager().getPerspectives()) {
	                JRadioButtonMenuItem item = new JRadioButtonMenuItem(
	                        new AbstractAction(perspective.getName()) {
	                            public void actionPerformed(ActionEvent e) {
	                                ComponentManager.getManager().setPerspective(perspective);
	                            }
	                        });
	                group.add(item);
	                if (ObjectUtil.equals(ComponentManager.getManager()
	                        .getCurrentPerspective(), perspective))
	                    item.setSelected(true);
	                out.add(item);
		    }
	            out.add(new JSeparator());

	            final Perspective current = ComponentManager.getManager()
	            .getDriver().getCurrentPerspective();
	            JMenuItem savePerspectiveAsItem = new JMenuItem(
	                    new AbstractAction("Save current perspective as...") {
	                        public void actionPerformed(ActionEvent e) {
	                            String name = JOptionPane
	                            .showInputDialog("New perspective name");
	                            if (name != null) {
	                                ComponentManager.getManager()
	                                .savePerspectiveAs(current, name);
	                            }
	                        }
	                    });
	            JMenuItem deleteItem = new JMenuItem(new AbstractAction(
	            "Delete current perspective") {
	                public void actionPerformed(ActionEvent e) {
	                    ComponentManager.getManager()
	                    .deletePerspective(current);
	                }
	            });
	            boolean builtin = current == null ? false : current
	                    .getBuiltIn();
	            deleteItem.setEnabled(!builtin);
	            out.add(savePerspectiveAsItem);
	            out.add(deleteItem);
	            return out;
	}


}
