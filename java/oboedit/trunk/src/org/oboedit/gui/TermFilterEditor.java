package org.oboedit.gui;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import info.clearthought.layout.TableLayout;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;

import org.bbop.swing.MultiIcon;
import org.bbop.swing.widget.AutocompleteBox;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.filters.Filter;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterImpl;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterImpl;
import org.obo.filters.SearchAspect;
import org.obo.filters.SearchComparison;
import org.obo.filters.SearchCriterion;
import org.obo.filters.SelfSearchAspect;
import org.obo.util.FilterUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.GUIUpdateEvent;
import org.oboedit.gui.event.GUIUpdateListener;

public class TermFilterEditor extends JPanel {

	protected static final String[] values = { "have", "don't have" };

	protected JComboBox aspectBox = new JComboBox();

	protected JComboBox comparisonBox = new JComboBox();

	protected JComboBox criterionBox = new JComboBox();

	protected JComboBox notBox = new JComboBox(values);

	protected JComboBox typeBox = new JComboBox();

	protected Box aspectLineBox = new Box(BoxLayout.X_AXIS);

	protected Box valueBox = new Box(BoxLayout.X_AXIS);

	protected StringFieldAutocompleteModel model = new StringFieldAutocompleteModel();

	protected AutocompleteBox<String> valueField;

	protected JButton advancedButton = new JButton();

	protected MultiIcon rightIcon;

	protected MultiIcon leftIcon;

	protected GUIUpdateEvent updateEvent = new GUIUpdateEvent(this);

	protected Collection<GUIUpdateListener> updateListeners = new LinkedList<GUIUpdateListener>();

	protected JPanel mainPanel = new JPanel();

	protected JLabel selectTermsLabel = new JLabel("Select terms that ");

	protected Box comparisonPanel = new Box(BoxLayout.X_AXIS);

	protected Box valuePanel = new Box(BoxLayout.X_AXIS);

	protected JLabel reachedViaLabel = new JLabel("that can be reached via ");

	protected class BasicActionListener implements ActionListener {

		public void actionPerformed(ActionEvent e) {
			updateFields();
		}

	}

	protected ActionListener aspectBoxListener = new BasicActionListener();

	protected ActionListener comparisonBoxListener = new BasicActionListener();

	protected ActionListener criterionBoxListener = new BasicActionListener();

	protected ActionListener notBoxListener = new BasicActionListener();

	protected ActionListener typeBoxListener = new BasicActionListener();

	protected ActionListener valueFieldListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fireUpdateEvent();
		}
	};

	protected Timer timer = new Timer(500, new ActionListener() {

		public void actionPerformed(ActionEvent e) {
			if (updateEventPending) {
				fireUpdateEventImmediately();
				updateEventPending = false;
			}
		}

	});

	protected Class getInputClass() {
		return IdentifiedObject.class;
	}

	protected Collection<SearchCriterion<?, ?>> getCriteria() {
		Collection<SearchCriterion<?, ?>> out = new LinkedList<SearchCriterion<?, ?>>();
		for (SearchCriterion sc : FilterManager.getManager()
				.getDisplayableCriteria()) {
			if (getInputClass().isAssignableFrom(sc.getInputType())) {
				out.add(sc);
			}
		}
		return out;
	}

	protected boolean aspectVisible = false;

	protected void updateFields() {
		aspectBox.removeActionListener(aspectBoxListener);
		comparisonBox.removeActionListener(comparisonBoxListener);
		criterionBox.removeActionListener(criterionBoxListener);
		notBox.removeActionListener(notBoxListener);
		typeBox.removeActionListener(typeBoxListener);

		SearchCriterion<?, ?> criterion = (SearchCriterion<?, ?>) criterionBox
				.getSelectedItem();
		SearchComparison comparison = (SearchComparison) comparisonBox
				.getSelectedItem();

		OBOProperty type = null;
		if (typeBox.getSelectedIndex() > 0)
			type = (OBOProperty) typeBox.getSelectedItem();

		criterionBox.removeAllItems();
		comparisonBox.removeAllItems();

		boolean containsCurrentSelection = false;
		for (SearchCriterion c : getCriteria()) {
			criterionBox.addItem(c);
			if (ObjectUtil.equals(c, criterion))
				containsCurrentSelection = true;
		}

		if (!containsCurrentSelection)
			criterion = (SearchCriterion) criterionBox.getItemAt(0);

		criterionBox.setSelectedItem(criterion);

		containsCurrentSelection = false;
		typeBox.removeAllItems();
		typeBox.addItem("<any type>");
		for (IdentifiedObject prop : SessionManager.getManager().getSession()
				.getObjects()) {
			if (prop instanceof OBOProperty) {
				typeBox.addItem(prop);
				if (ObjectUtil.equals(prop, type))
					containsCurrentSelection = true;
			}
		}
		if (containsCurrentSelection) {
			typeBox.setSelectedItem(type);
		} else
			typeBox.setSelectedIndex(0);
		boolean showTypeBox = !(aspectBox.getSelectedItem() instanceof SelfSearchAspect);
		reachedViaLabel.setVisible(showTypeBox);
		typeBox.setVisible(showTypeBox);

		containsCurrentSelection = false;
		Collection<SearchComparison> comparisons = FilterManager.getManager()
				.getComparisons();
		for (SearchComparison c : comparisons) {
			Class[] acceptedTypes = c.getAcceptedTypes();
			boolean match = false;
			for (Class atype : acceptedTypes) {
				boolean isNumeric = Number.class.isAssignableFrom(criterion
						.getReturnType());
				boolean isString = String.class.isAssignableFrom(atype);
				if (criterion != null
						&& (atype.isAssignableFrom(criterion.getReturnType()) || (!isNumeric && isString))) {
					match = true;
					break;
				}
			}
			if (match) {
				if (ObjectUtil.equals(c, comparison))
					containsCurrentSelection = true;
				comparisonBox.addItem(c);
			}
		}
		if (!containsCurrentSelection)
			comparison = (SearchComparison) comparisonBox.getItemAt(0);
		comparisonBox.setSelectedItem(comparison);
		boolean isBoolean = Boolean.class.isAssignableFrom(criterion
				.getReturnType());
		comparisonPanel.setVisible(!isBoolean);
		valuePanel.setVisible(!isBoolean);
		validate();

		aspectBox.addActionListener(aspectBoxListener);
		comparisonBox.addActionListener(comparisonBoxListener);
		criterionBox.addActionListener(criterionBoxListener);
		notBox.addActionListener(notBoxListener);
		typeBox.addActionListener(typeBoxListener);
		fireUpdateEvent();
	}

	public void addActionListener(ActionListener actionListener) {
		valueField.addActionListener(actionListener);
	}

	public void removeActionListener(ActionListener actionListener) {
		valueField.removeActionListener(actionListener);
	}

	public TermFilterEditor() {
		setOpaque(false);
		mainPanel.setOpaque(false);
		rightIcon = new MultiIcon();
		rightIcon.addIcon(Preferences.loadLibraryIcon("customize_simple.gif"));
		rightIcon.addIcon(Preferences
				.loadLibraryIcon("tiny_right_indent_icon.gif"));
		leftIcon = new MultiIcon();
		leftIcon.addIcon(Preferences
				.loadLibraryIcon("tiny_left_indent_icon.gif"));
		leftIcon.addIcon(Preferences.loadLibraryIcon("customize_simple.gif"));

		advancedButton.setIcon(rightIcon);
		advancedButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				setAspectControlsVisible(!aspectVisible);
			}

		});
		double[][] sizes = {
				{ TableLayout.FILL },
				{ TableLayout.PREFERRED, TableLayout.PREFERRED,
						TableLayout.PREFERRED } };
		mainPanel.setLayout(new TableLayout(sizes));
		// setLayout(new GridLayout(3, 1));
		valueField = new AutocompleteBox<String>(model);
		model.clear();
		valueField.setAllowNonModelValues(true);
		valueField.setMinLength(1);

		criterionBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				model.clear();
				model.addCriterion((SearchCriterion) criterionBox
						.getSelectedItem());
			}
		});

		for (SearchAspect aspect : FilterManager.getManager().getAspects()) {
			aspectBox.addItem(aspect);
		}

		comparisonPanel.add(new JLabel("that "));
		comparisonPanel.add(comparisonBox);

		Box topBox = new Box(BoxLayout.X_AXIS);
		topBox.add(selectTermsLabel);
		topBox.add(notBox);
		topBox.add(new JLabel("a "));
		topBox.add(criterionBox);
		topBox.add(comparisonPanel);
		topBox.add(Box.createHorizontalGlue());

		valuePanel.add(new JLabel("the value "));
		valuePanel.add(valueField);
		valuePanel.add(Box.createHorizontalStrut(5));

		valueBox.add(valuePanel);
		valueBox.add(advancedButton);

		aspectLineBox.add(new JLabel("in "));
		aspectLineBox.add(aspectBox);
		aspectLineBox.add(reachedViaLabel);
		aspectLineBox.add(typeBox);

		mainPanel.add(topBox, "0,0");
		mainPanel.add(valueBox, "0,1");
		mainPanel.add(aspectLineBox, "0,2");
		setAspectControlsVisible(false);

		aspectBox.setOpaque(false);
		comparisonBox.setOpaque(false);
		criterionBox.setOpaque(false);
		notBox.setOpaque(false);
		typeBox.setOpaque(false);
		advancedButton.setOpaque(false);
		valueField.setOpaque(false);

		notBox.addActionListener(notBoxListener);
		aspectBox.addActionListener(aspectBoxListener);
		comparisonBox.addActionListener(comparisonBoxListener);
		criterionBox.addActionListener(criterionBoxListener);
		typeBox.addActionListener(typeBoxListener);
		valueField.addUpdateListener(valueFieldListener);
		timer.start();
		layoutGUI();
		updateFields();
	}

	protected void layoutGUI() {
		setLayout(new GridLayout(1, 1));
		add(mainPanel);
	}

	protected void setAspectControlsVisible(boolean visible) {
		this.aspectVisible = visible;
		if (visible) {
			mainPanel.add(aspectLineBox, "0,2");
			valueBox.remove(advancedButton);
			aspectLineBox.add(advancedButton);
			advancedButton.setIcon(leftIcon);
		} else {
			mainPanel.remove(aspectLineBox);
			aspectLineBox.remove(advancedButton);
			valueBox.add(advancedButton);
			advancedButton.setIcon(rightIcon);
		}
		SearchPanel p = getSearchPanel();
		if (p != null)
			p.topmostValidate();
	}

	public Filter<IdentifiedObject> getFilter() {
		ObjectFilter out = new ObjectFilterImpl();
		out.setCriterion((SearchCriterion) criterionBox.getSelectedItem());
		// out.setValue(valueField.getValue());
		// Trim whitespace at beginning/end of search string before doing search,
		// so that "term " returns the same results as "term".
		out.setValue(valueField.getEditorText().trim());
		out.setNegate(notBox.getSelectedIndex() == 1);
		out.setComparison((SearchComparison) comparisonBox.getSelectedItem());
		if (aspectVisible) {
			out.setAspect((SearchAspect) aspectBox.getSelectedItem());
			if (typeBox.getSelectedIndex() > 0) {
				OBOProperty p = (OBOProperty) typeBox.getSelectedItem();
				LinkFilter linkFilter = new LinkFilterImpl(p);
				out.setTraversalFilter(linkFilter);
			}
		}
		return out;
	}

	public SearchPanel getSearchPanel() {
		return (SearchPanel) SwingUtilities.getAncestorOfClass(
				SearchPanel.class, this);
	}

	protected boolean updateEventPending = false;

	protected void fireUpdateEvent() {
		updateEventPending = true;
	}

	protected void fireUpdateEventImmediately() {
		for (GUIUpdateListener listener : updateListeners) {
			listener.guiupdated(updateEvent);
		}
	}

	public void addUpdateListener(GUIUpdateListener listener) {
		updateListeners.add(listener);
	}

	public void removeUpdateListener(GUIUpdateListener listener) {
		updateListeners.remove(listener);
	}

	public void setFilter(Filter filter) {
		if (filter instanceof ObjectFilter) {
			ObjectFilter of = (ObjectFilter) filter;
			criterionBox.setSelectedItem(of.getCriterion());
			if (of.getNegate())
				notBox.setSelectedIndex(1);
			else
				notBox.setSelectedIndex(0);
			comparisonBox.setSelectedItem(of.getComparison());
			if (!(of.getAspect() instanceof SelfSearchAspect)) {
				setAspectControlsVisible(true);
				aspectBox.setSelectedItem(of.getAspect());
				if (of.getTraversalFilter() != null) {
					if (FilterUtil
							.isTypeOnlyLinkFilter(of.getTraversalFilter())) {
						String propID = FilterUtil.getTypeOnlyPropertyID(of
								.getTraversalFilter());
						if (propID != null) {
							OBOProperty prop = (OBOProperty) SessionManager
									.getManager().getSession()
									.getObject(propID);
							typeBox.setSelectedItem(prop);
						}
					}
				}
			}
		} else
			throw new IllegalArgumentException("Cannot load non-object filter");
	}

}
