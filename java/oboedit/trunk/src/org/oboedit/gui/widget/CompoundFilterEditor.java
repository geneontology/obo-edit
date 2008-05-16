package org.oboedit.gui.widget;

import java.awt.*;

import javax.swing.*;
import java.awt.event.*;

import org.obo.filters.*;

import org.apache.log4j.*;

public class CompoundFilterEditor extends AbstractFilterEditor {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CompoundFilterEditor.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1885178355886161229L;
	protected CompoundFilter filter;
	protected JComboBox operationsComboBox = new JComboBox();

	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		operationsComboBox.setEnabled(enabled);
	}

	public CompoundFilterEditor() {
		setOpaque(false);
		operationsComboBox.addItem("AND");
		operationsComboBox.addItem("OR");
		add(operationsComboBox);

		operationsComboBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				acceptEdits();
			}
		});
	}

	public void setButtonColor(Color buttonColor) {
		operationsComboBox.setBackground(buttonColor);
	}

	public void addActionListener(ActionListener listener) {
	}

	public void removeActionListener(ActionListener listener) {
	}

	public Filter getFilter() {
		return filter;
	}

	public void acceptEdits() {
		filter.setBooleanOperation(operationsComboBox.getSelectedIndex());
	}

	protected void updateGUI() {
		operationsComboBox.setSelectedIndex(filter.getBooleanOperation());
	}

	public void setFilter(Filter filter) {
		this.filter = (CompoundFilter) filter;
		updateGUI();
	}

}
