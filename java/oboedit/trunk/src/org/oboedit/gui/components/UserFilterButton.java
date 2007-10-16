package org.oboedit.gui.components;

import java.awt.event.*;
import java.io.*;
import java.net.*;
import javax.swing.*;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;
import org.obo.filters.*;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.widget.FilterPairEditor;

public class UserFilterButton implements GUIComponent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public static final int MODIFY = 1;

	public static final int FILTER = 2;

	public static final int RENDER = 3;

	public static final int SEARCH = 4;

	protected int method = SEARCH;

	protected FilterPair pair;

	protected String label;

	protected String iconPath;

	protected String tooltipText;

	protected boolean selected;

	protected AbstractButton component;

	protected ItemListener modifyActionListener = new ItemListener() {
		public void itemStateChanged(ItemEvent e) {
			selected = ((JToggleButton) component).isSelected();
			if (selected) {
				FilterManager.getManager().addModifyFilter(pair);
				System.err.println("adding modify filter: " + pair);
			} else {
				FilterManager.getManager().removeModifyFilter(pair);
				System.err.println("removing modify filter: " + pair);
			}
		}
	};

	protected ActionListener searchActionListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			System.err.println("DISABLED!");
//			Controller.getController().getFindPanel().setFilterPair(pair);
//			Controller.getController().getFindPanel().search();
		}
	};

	protected ActionListener filterActionListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			System.err.println("DISABLED!");
//			Controller.getController().getFindPanel().setFilterPair(pair);
//			Controller.getController().getFindPanel().filter();
		}
	};

	protected static String resolveFilterPath(String filterPath) {
		/*
		if (filterPath != null && filterPath.startsWith("resource:"))
			return Controller.getController().getResourceURL(
					"org/oboedit/gui/resources/filters/"
							+ filterPath.substring(9, filterPath.length()))
					.toString();
		else
			return filterPath;
			*/
		throw new UnsupportedOperationException("Not supported!");
	}

	public String getTitle() {
		return label;
	}

	public void setTitle(String name) {
		label = name;
	}

	public ConfigurationPanel getConfigurationPanel() {
		return null;
	}

	public UserFilterButton(int method, String filterPath, String label,
			boolean selected, String iconPath, String tooltipText,
			ButtonGroup group) {
		try {
			filterPath = resolveFilterPath(filterPath);
			this.method = method;
			if (filterPath != null)
				pair = FilterPairEditor.loadFilterPair(filterPath);
			else
				pair = FilterPair.ALWAYS_TRUE;
			this.label = label;
			this.iconPath = iconPath;
			this.tooltipText = tooltipText;
			this.selected = selected;

			if (method == MODIFY) {
				if (group == null)
					component = new JCheckBox();
				else
					component = new JRadioButton();
				component.addItemListener(modifyActionListener);
				if (group != null)
					group.add(component);
				component.setOpaque(false);
				((JToggleButton) component).setSelected(selected);
			} else {
				component = new JButton();
				component.addActionListener(searchActionListener);
			}

			if (tooltipText != null)
				component.setToolTipText(tooltipText);

			Icon icon = null;
			if (iconPath != null) {
				try {
					URL url = new URL(iconPath);
					icon = new ImageIcon(url);
				} catch (Exception ex) {
					File file = new File(iconPath);
					if (file.exists())
						icon = new ImageIcon(iconPath);
					else
						icon = Preferences.loadLibraryIcon(iconPath);
				}
			}
			if (icon != null)
				component.setIcon(icon);

			if (label != null)
				component.setText(label);

		} catch (IOException ex) {
			ex.printStackTrace();
			component.setText("Could not load filter " + filterPath);
		}
	}

	public void init() {

		if (selected)
			FilterManager.getManager().addModifyFilter(pair);
	}

	public void cleanup() {
		if (selected)
			FilterManager.getManager().removeModifyFilter(pair);
	}

	public boolean isSingleton() {
		return false;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}
	
	public boolean teardownWhenHidden() {
		return true;
	}

	public void setConfiguration(ComponentConfiguration c) {
	}

	public String getID() {
		return "SEARCH_BUTTON";
	}

	public JComponent getComponent() {
		return component;
	}

	public void setXML(String xml) {
	}

	public boolean isXMLSettable() {
		return false;
	}
}
