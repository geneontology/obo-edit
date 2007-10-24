package org.oboedit.gui.widget;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import org.obo.datamodel.*;
import org.obo.filters.*;

public class LinkFilterPanel extends AbstractFilterEditor {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected ObjectFilterPanel filterPanel = new ObjectFilterPanel();
	protected JComboBox aspectComboBox = new JComboBox();
	protected LinkFilter filter;
	protected ActionListener aspectListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if (getSelectedAspect() == LinkFilter.SELF) {
				filterPanel.setObjectClass(Link.class);
			} else {
				filterPanel.setObjectClass(IdentifiedObject.class);
			}
		}
	};
	
	public void removeListeners() {
		aspectComboBox.removeActionListener(aspectListener);
	}


	public void attachListeners() {
		aspectComboBox.addActionListener(aspectListener);
	}
	
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		aspectComboBox.setEnabled(enabled);
		filterPanel.setEnabled(enabled);
	}

	public LinkFilterPanel() {
		setOpaque(false);
		setLayout(new BorderLayout());

		filter = new LinkFilterImpl();

		aspectComboBox.addItem("Child");
		aspectComboBox.addItem("Type");
		aspectComboBox.addItem("Parent");
		aspectComboBox.addItem("Self");

		add(aspectComboBox, "North");
		add(filterPanel, "Center");

		attachListeners();

		filterPanel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				acceptEdits();
			}
		});
	}

	@Override
	public void setFont(Font font) {
		if (filterPanel != null)
			filterPanel.setFont(font);
		if (aspectComboBox != null)
			aspectComboBox.setFont(font);
	}

	public void setButtonColor(Color color) {
		aspectComboBox.setBackground(color);
		filterPanel.setButtonColor(color);

	}

	public void acceptEdits() {
		filterPanel.acceptEdits();
		filter.setAspect(getSelectedAspect());
		filter.setFilter((ObjectFilter) filterPanel.getFilter());
	}

	protected void updateGUI() {
		removeListeners();
		setSelectedAspect(filter.getAspect());
		filterPanel.setFilter(filter.getFilter());
		attachListeners();
	}

	public void setSelectedAspect(int aspect) {
		aspectComboBox.setSelectedIndex(aspect - 1);
	}

	public void addActionListener(ActionListener listener) {
		filterPanel.addActionListener(listener);
	}

	public void removeActionListener(ActionListener listener) {
		filterPanel.removeActionListener(listener);
	}

	public void setFilter(Filter filter) {
		this.filter = (LinkFilter) filter;
		updateGUI();
	}

	public Filter getFilter() {
		acceptEdits();
		return filter;
	}

	protected int getSelectedAspect() {
		return aspectComboBox.getSelectedIndex() + 1;
	}
}
