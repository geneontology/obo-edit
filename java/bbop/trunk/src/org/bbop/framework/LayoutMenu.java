package org.bbop.framework;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;

import org.bbop.framework.dock.Perspective;
import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.util.ObjectUtil;
import org.apache.log4j.*;

public class LayoutMenu extends AbstractDynamicMenuItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ComponentManager.class);

	public LayoutMenu() {
		super("Layout");
	}

	public List<Component> getItems() {
		final List<Component> out = new ArrayList<Component>();
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

		ImageIcon lockIcon = new ImageIcon(System.getProperty("user.dir") + "/src/org/oboedit/gui/resources/icons/lock.png");
		ImageIcon unlockIcon = new ImageIcon(System.getProperty("user.dir") + "/src/org/oboedit/gui/resources/icons/unlock.png");	            
		JMenuItem lockLayout = new JMenuItem(new AbstractAction("Lock layout", lockIcon){
			public void actionPerformed(ActionEvent e) {
				//if layout is locked already - unlock
				if(GUIManager.getManager().getDocLockStatus())
					GUIManager.getManager().setDocLockStatus(false);
				else //lock layout
					GUIManager.getManager().setDocLockStatus(true);
			}
		});
		//if locked show unlock interface and vice-versa
		if(GUIManager.getManager().getDocLockStatus()){
			lockLayout.setIcon(unlockIcon);
			lockLayout.setText("Unlock Layout");
			lockLayout.setToolTipText("enable layout modifications");
			GUIManager.getManager().getFrame().validate();
		} else { 
			lockLayout.setIcon(lockIcon);
			lockLayout.setText("Lock Layout");
			lockLayout.setToolTipText("disable layout modifications");
			GUIManager.getManager().getFrame().validate();
		}

		out.add(importPerspectiveItem);
		out.add(savePerspectiveAsItem); 
		boolean builtin = current == null ? false : current.getBuiltIn();
		deleteItem.setEnabled(!builtin);
		out.add(deleteItem);	            
		out.add(lockLayout);
		return out;
	}

}
