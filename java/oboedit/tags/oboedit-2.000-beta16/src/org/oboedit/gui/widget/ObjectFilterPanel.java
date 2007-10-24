package org.oboedit.gui.widget;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import org.obo.datamodel.*;
import org.obo.filters.*;
import org.obo.util.FilterUtil;
import org.oboedit.controller.FilterManager;

public class ObjectFilterPanel extends AbstractFilterEditor {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ObjectFilter filter;

	protected JCheckBox notCheckBox = new JCheckBox("NOT");

	protected JComboBox aspectComboBox = new JComboBox();

	protected JComboBox criterionComboBox = new JComboBox();

	protected JComboBox comparisonComboBox = new JComboBox();

	protected JTextField valueField = new JTextField();

	protected Box topLine = new Box(BoxLayout.X_AXIS);

	protected JPanel bottomPanel = new JPanel();

	protected static int idgen = 0;

	protected int id = idgen++;

	protected Class objectClass = Object.class;

	ActionListener criterionListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			SearchCriterion sc = (SearchCriterion) criterionComboBox
					.getSelectedItem();
			setComparisonClass(getReturnType(sc));
			fireFilterEditUpdate();
		}
	};

	KeyListener keyListener = new KeyAdapter() {
		@Override
		public void keyPressed(KeyEvent e) {
			fireFilterEditUpdate();
		}
	};

	ActionListener updatingActionListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fireFilterEditUpdate();
		}
	};

	ActionListener acceptListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			// acceptEdits();
		}
	};

	/*
	 * ActionListener valueAcceptListener = new ActionListener() { public void
	 * actionPerformed(ActionEvent e) { // acceptEdits();
	 * fireActionPerformed(e); } };
	 */
	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		notCheckBox.setEnabled(enabled);
		criterionComboBox.setEnabled(enabled);
		aspectComboBox.setEnabled(enabled);
		comparisonComboBox.setEnabled(enabled);
		valueField.setEnabled(enabled);
	}

	@Override
	public String toString() {
		return "ObjectFilterPanel:" + id;
	}

	@Override
	public void setFont(Font font) {
		if (notCheckBox != null)
			notCheckBox.setFont(font);
		if (criterionComboBox != null)
			criterionComboBox.setFont(font);
		if (aspectComboBox != null)
			aspectComboBox.setFont(font);
		if (comparisonComboBox != null)
			comparisonComboBox.setFont(font);
		if (valueField != null)
			valueField.setFont(font);
	}

	public void setButtonColor(Color buttonColor) {
		notCheckBox.setBackground(buttonColor);
		criterionComboBox.setBackground(buttonColor);
		comparisonComboBox.setBackground(buttonColor);
		aspectComboBox.setBackground(buttonColor);
		// valueField.setBackground(buttonColor);
	}

	public void setObjectClass(Class c) {
		if (!c.equals(objectClass)) {
			criterionComboBox.removeActionListener(criterionListener);

			Object oldSelection = criterionComboBox.getSelectedItem();
			criterionComboBox.removeAllItems();
			Collection col = FilterUtil.filterCriteriaByInput(getAllCriteria(),
					c);

			Iterator it = col.iterator();
			boolean matched = false;
			while (it.hasNext()) {
				Object o = it.next();
				if (oldSelection != null && o.equals(oldSelection))
					matched = true;
				criterionComboBox.addItem(o);
			}

			if (matched) {
				criterionComboBox.setSelectedItem(oldSelection);
				criterionComboBox.addActionListener(criterionListener);
			} else {
				criterionComboBox.addActionListener(criterionListener);
				criterionComboBox.setSelectedIndex(0);
			}
			objectClass = c;
		}
	}

	public void setComparisonClass(Class c) {
		topLine.removeAll();
		remove(bottomPanel);

		topLine.add(notCheckBox);
		topLine.add(Box.createHorizontalStrut(10));
		topLine.add(aspectComboBox);
		topLine.add(Box.createHorizontalStrut(10));
		topLine.add(criterionComboBox);

		if (!Boolean.class.isAssignableFrom(c)) {
			topLine.add(Box.createHorizontalStrut(10));
			topLine.add(comparisonComboBox);

			add(bottomPanel, "Center");

			Object oldSelection = comparisonComboBox.getSelectedItem();
			boolean stillThere = false;
			comparisonComboBox.removeAllItems();
			Collection col = FilterUtil.filterComparisonByInput(
					getAllComparisons(), c);

			Iterator it = col.iterator();
			while (it.hasNext()) {
				Object o = it.next();
				if (oldSelection != null && o.equals(oldSelection))
					stillThere = true;
				comparisonComboBox.addItem(o);
			}
			if (stillThere && oldSelection != null)
				comparisonComboBox.setSelectedItem(oldSelection);
		}
		validate();
		repaint();
	}

	public ObjectFilterPanel() {
		// valueField.setEditable(true);
		bottomPanel.setLayout(new BorderLayout());

		filter = new ObjectFilterImpl();

		attachListeners();

		setObjectClass(IdentifiedObject.class);

		topLine.add(notCheckBox);
		topLine.add(Box.createHorizontalStrut(10));
		topLine.add(aspectComboBox);
		topLine.add(Box.createHorizontalStrut(10));
		topLine.add(criterionComboBox);
		topLine.add(Box.createHorizontalStrut(10));
		topLine.add(comparisonComboBox);

		bottomPanel.add(valueField, "North");

		setLayout(new BorderLayout());
		add(topLine, "North");
		add(bottomPanel, "Center");

		setOpaque(false);
		bottomPanel.setOpaque(false);

		aspectComboBox.addItem(new SelfSearchAspect());
		aspectComboBox.addItem(new AncestorSearchAspect());
		aspectComboBox.addItem(new DescendantSearchAspect());
		aspectComboBox.addItem(new RootSearchAspect());

		notCheckBox.setOpaque(false);
		notCheckBox.addActionListener(acceptListener);

		comparisonComboBox.addActionListener(acceptListener);

		// valueField.addActionListener(valueAcceptListener);
		/*
		 * valueField.getEditor().getEditorComponent(). addKeyListener(new
		 * KeyAdapter() { public void keyTyped(KeyEvent e) { acceptEdits(); }
		 * });
		 */
		criterionComboBox.setSelectedIndex(0);
	}

	public void acceptEdits() {
		removeListeners();
		filter.setNegate(notCheckBox.isSelected());
		filter.setAspect((SearchAspect) aspectComboBox.getSelectedItem());
		filter.setCriterion((SearchCriterion) criterionComboBox
				.getSelectedItem());
		filter.setComparison((SearchComparison) comparisonComboBox
				.getSelectedItem());
		/*
		 * JTextField field = (JTextField) valueField.getEditor().
		 * getEditorComponent(); filter.setValue(field.getText());
		 */
		filter.setValue(valueField.getText());
		attachListeners();
	}

	protected void attachListeners() {
		criterionComboBox.addActionListener(criterionListener);
		notCheckBox.addActionListener(updatingActionListener);
		aspectComboBox.addActionListener(updatingActionListener);
		comparisonComboBox.addActionListener(updatingActionListener);
		valueField.addKeyListener(keyListener);
	}

	protected void removeListeners() {
		criterionComboBox.removeActionListener(criterionListener);
		notCheckBox.removeActionListener(updatingActionListener);
		aspectComboBox.removeActionListener(updatingActionListener);
		comparisonComboBox.removeActionListener(updatingActionListener);
		valueField.removeKeyListener(keyListener);
	}

	protected void updateGUI() {
		removeListeners();
		notCheckBox.setSelected(filter.getNegate());
		aspectComboBox.setSelectedItem(filter.getAspect());
		criterionComboBox.setSelectedItem(filter.getCriterion());
		comparisonComboBox.setSelectedItem(filter.getComparison());
		valueField.setText(filter.getValue());

		if (filter.getComparison() != null) {
			Class returnType = getReturnType(filter.getCriterion());
			setComparisonClass(returnType);
		}

		attachListeners();
	}

	protected Class getReturnType(SearchCriterion criterion) {
		Class returnType = criterion.getReturnType();
		if (!Number.class.isAssignableFrom(returnType) &&
				!Boolean.class.isAssignableFrom(returnType))
			return String.class;
		else
			return returnType;
	}

	public void addActionListener(ActionListener listener) {
		valueField.addActionListener(listener);
	}

	public void removeActionListener(ActionListener listener) {
		valueField.removeActionListener(listener);
	}

	/*
	 * protected void fireActionPerformed(ActionEvent e) { Iterator it =
	 * actionListeners.iterator(); while(it.hasNext()) { ActionListener listener =
	 * (ActionListener) it.next(); listener.actionPerformed(e); } }
	 */
	public Collection getAllCriteria() {
		return FilterManager.getManager().getCriteria();
	}

	public Collection getAllComparisons() {
		return FilterManager.getManager().getComparisons();
		/*
		 * ArrayList out = new ArrayList(); out.add(new EqualsComparison());
		 * out.add(new ContainsComparison()); out.add(new
		 * StartsWithComparison()); out.add(new EndsWithComparison());
		 * out.add(new LessThanComparison()); return out;
		 */
	}

	public Filter getFilter() {
		return filter;
	}

	public void setFilter(Filter filter) {
		this.filter = (ObjectFilter) filter;
		updateGUI();
	}
}
