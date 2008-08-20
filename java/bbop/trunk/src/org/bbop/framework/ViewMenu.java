package org.bbop.framework;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
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
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;

import org.bbop.framework.dock.Perspective;
import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.ObjectUtil;

import org.apache.log4j.*;

/** No longer used by OBO-Edit, but still used by Phenote. */

public class ViewMenu extends AbstractDynamicMenuItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ViewMenu.class);

	public ViewMenu() {
		super("View");
	}

	public List<Component> getItems() {
	    List<Component> out = new ArrayList<Component>();
	    DynamicMenu perspectivesMenu = new DynamicMenu("Layouts") {
	        public List<Component> getItems() {
	            ButtonGroup group = new ButtonGroup();
	            List<Component> out = new LinkedList<Component>();
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

		    // Also added to LayoutMenu.java for OBO-Edit, which doesn't use ViewMenu anymore.
	            JMenuItem importPerspectiveItem = new JMenuItem(
	                    new AbstractAction("Import perspective...") {
	                        public void actionPerformed(ActionEvent e) {
					JFileChooser chooser = new JFileChooser();
					if (chooser.showOpenDialog(GUIManager.getManager().getFrame()) == JFileChooser.APPROVE_OPTION) {
						File file = chooser.getSelectedFile();
						if (!file.getName().endsWith(".idw")) {
							JOptionPane.showMessageDialog(null, "File " + file + " doesn't end with .idw--can't import as perspective.");
							return;
						}
						ComponentManager.getManager().importPerspective(file);
					}
	                        }
	                    });

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
	            out.add(importPerspectiveItem);
	            boolean builtin = current == null ? false : current
	                    .getBuiltIn();
	            deleteItem.setEnabled(!builtin);
	            out.add(savePerspectiveAsItem);
	            out.add(deleteItem);
	            return out;
	        }
	    };

	    DynamicMenu showMenu = new DynamicMenu("Show") {
	        public List<Component> getItems() {
	            List<Component> out = new LinkedList<Component>();
	            Map<GUIComponentFactory.FactoryCategory, List<GUIComponentFactory>> factoryMap = new HashMap<GUIComponentFactory.FactoryCategory, List<GUIComponentFactory>>();
	            Collection<GUIComponentFactory<?>> f = ComponentManager
	            .getManager().getFactories();
	            for (final GUIComponentFactory factory : f) {
	                if (!factory.showInMenus())
	                    continue;
	                List<GUIComponentFactory> factories = factoryMap
	                .get(factory.getCategory());
	                if (factories == null) {
	                    factories = new ArrayList<GUIComponentFactory>();
	                    factoryMap.put(factory.getCategory(), factories);
	                }
	                factories.add(factory);
	            }
	            Comparator<GUIComponentFactory> factoryComparator = new Comparator<GUIComponentFactory>() {

	                public int compare(GUIComponentFactory o1,
	                        GUIComponentFactory o2) {
	                    return o1.toString().compareTo(o2.toString());
	                }

	            };
	            for (GUIComponentFactory.FactoryCategory category : GUIComponentFactory.FactoryCategory
	                    .values()) {
	                List<GUIComponentFactory> factories = factoryMap.get(category);
	                if (factories != null && factories.size() > 0) {
	                    Collections.sort(factories, factoryComparator);
	                    DynamicMenu subMenu = new DynamicMenu(category
	                            .toString());
	                    for (final GUIComponentFactory factory : factories) {
	                        JMenuItem item = new JMenuItem(new AbstractAction(
	                                factory.getName()) {
	                            public void actionPerformed(ActionEvent e) {
	                                ComponentManager.getManager()
	                                .showComponent(factory, null);
	                            }
	                        });
	                        subMenu.add(item);
	                    }
	                    out.add(subMenu);
	                }
	            }
	            return out;
	        }
	    };

	    out.add(perspectivesMenu);
	    out.add(showMenu);
	    return out;
	}
}
