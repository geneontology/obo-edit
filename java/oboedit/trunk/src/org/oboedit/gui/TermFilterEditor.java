package org.oboedit.gui;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import javax.swing.ToolTipManager;

import org.bbop.swing.MultiIcon;
import org.bbop.swing.widget.AutocompleteBox;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.IdentifiedObject;
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
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.util.GUIUtil;
import org.apache.log4j.*;

public class TermFilterEditor extends JPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermFilterEditor.class);

	protected static final String[] values = { "have", "don't have" };

        protected JComboBox aspectBox = new JComboBox();

	protected JComboBox comparisonBox = new JComboBox();

	protected JComboBox criterionBox = new JComboBox();

	protected JComboBox notBox = new JComboBox(values);

	protected JComboBox typeBox = new JComboBox();

	protected Box aspectLineBox = new Box(BoxLayout.X_AXIS);
        protected Box advancedButtonAndAspectLineBox = new Box(BoxLayout.X_AXIS);

	protected Box valueBox = new Box(BoxLayout.X_AXIS);

	protected StringFieldAutocompleteModel model = new StringFieldAutocompleteModel();

	protected AutocompleteBox<String> valueField;

	protected JButton advancedButton = new JButton();

	protected MultiIcon rightIcon;

	protected MultiIcon leftIcon;

	protected GUIUpdateEvent updateEvent = new GUIUpdateEvent(this);

	protected Collection<GUIUpdateListener> updateListeners = new LinkedList<GUIUpdateListener>();

	protected JPanel mainPanel = new JPanel();

	protected JLabel selectTermsLabel = new JLabel("Find terms that ");

	protected Box comparisonPanel = new Box(BoxLayout.X_AXIS);

	protected Box valuePanel = new Box(BoxLayout.X_AXIS);

	protected JLabel reachedViaLabel = new JLabel(" that can be reached via ");

	protected class BasicActionListener implements ActionListener {

		public void actionPerformed(ActionEvent e) {
                  // updateFields();
                  // <3/2011: This call to updateFields is needed by the Link Search when the user
                  // selects a different item from the "Find links where" menu.
                  // 3/31/2011: Is it?  It doesn't seem to be, and having it there breaks
                  // the aspect field (in [self, child, etc.]) in the Search Panel (it
                  // always selects the first thing, "self", rather than what you actually
                  // tried to select).
                  // Oh, I see...if you don't updateFields, the status label
                  // (the text that has a descriptin of the search, e.g., "all_text_fields contains 'blue'")
                  // (it's in FilterComponent.java) doesn't update when you change the search.
                    // logger.debug("actionPerformed: " + e); // DEL
                    fireUpdateEvent(); // This seems to do the trick--statusLabel updates as it should.
                    // 2/2012: Except that if you type something into
                    // valueField and then select one of the autocomplete terms,
                    // the statusLabel doesn't update right away to show the selected term.
                    // (and changing back to updateFields didn't help).
		}
	}

    // Fix for bug (reported in two redundant bug reports):
    // [ geneontology-Bugs-3296528 ] Search aspect is missing the "can be reached via" selector
    // [ geneontology-Bugs-3297446 ] Can't restrict ancestor search by relation type in 2.1b12

	protected ActionListener aspectBoxListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
                    boolean showTypeBox = !(aspectBox.getSelectedItem() instanceof SelfSearchAspect);
                    // logger.debug("aspectBoxListener: aspectBox.getSelectedItem = " + aspectBox.getSelectedItem() + ", showTypeBox = " + showTypeBox); // DEL
                    reachedViaLabel.setVisible(showTypeBox);
                    typeBox.setVisible(showTypeBox);
                    updateFields(); // Need?
		}
	};

	protected ActionListener comparisonBoxListener = new BasicActionListener();

	protected ActionListener criterionBoxListener = new BasicActionListener();

	protected ActionListener notBoxListener = new BasicActionListener();

	protected ActionListener typeBoxListener = new BasicActionListener();

	protected ActionListener valueFieldListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
                    // logger.debug("TFE.valueFieldListener.actionPerformed: " + e); // DEL
                    // Calling updateFields doesn't solve the problem that if you type something into
                    // valueField and then select one of the autocomplete terms,
                    // the statusLabel doesn't update to show the selected term.
                    //                    updateFields();
                    // fireUpdateEvent(); // Need?
		}
	};

    // The timer causes the statusLabel (the string at the bottom of the search panel
    // that describes the search) to get updated. (Not sure why it's done this way.)
	protected Timer timer = new Timer(500, new ActionListener() {

		public void actionPerformed(ActionEvent e) {
			if (updateEventPending) {
				fireUpdateEventImmediately();
				updateEventPending = false;
			}
		}

	});

	protected Class<?> getInputClass() {
		return IdentifiedObject.class;
	}

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
                    //                    logger.debug("TFE: ReloadEvent: " + e); // DEL
                    //fireUpdateEvent(); // this wasn't good enough--aspect relation pulldown didn't show ontology-specific relations
                    updateFields();
            }
	};

        // This method gets called 33 times when OBO-Edit launches!
	protected Collection<SearchCriterion<?, ?>> getCriteria() {
		Collection<SearchCriterion<?, ?>> out = new LinkedList<SearchCriterion<?, ?>>();
                
		for (SearchCriterion sc : FilterManager.getManager()
				.getDisplayableCriteria()) {
                    // Could call getInputClass once outside loop...
			if (getInputClass().isAssignableFrom(sc.getInputType())) {
                            out.add(sc);
                        }
		}
		return out;
	}

	protected boolean aspectVisible = false;

        // This method gets called excessively
	protected void updateFields() {
            // logger.debug("TFE.updateFields"); // DEL
		aspectBox.removeActionListener(aspectBoxListener);
		comparisonBox.removeActionListener(comparisonBoxListener);
                criterionBox.removeActionListener(criterionBoxListener); // Need?
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
                //                logger.debug("TFE.updateFields: adding " + getCriteria().size() + " criteria"); // DEL
		for (SearchCriterion c : getCriteria()) {
			criterionBox.addItem(c);
			if (ObjectUtil.equals(c, criterion))
				containsCurrentSelection = true;
		}

                if (!containsCurrentSelection) {
			criterion = (SearchCriterion) criterionBox.getItemAt(0);
                }
                // logger.debug("criterionBox.setSelectedItem(" + criterion + ")");
		criterionBox.setSelectedItem(criterion);

                // 5/9/11: No longer doing this
                // // Update aspects too--some might have become valid or invalid due to
                // // change in reasoner state.
                // //                logger.debug("updateFields: updating aspects."); // DEL
		// aspectBox.removeAllItems();
		// for (SearchAspect aspect : FilterManager.getManager().getAspects()) {
                //   if (isValid(aspect))
		// 	aspectBox.addItem(aspect);
		// }

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
		//		logger.debug("TermFilterEditor: aspectBox.getSelectedItem = " + aspectBox.getSelectedItem() + ", showTypeBox = " + showTypeBox); // DEL
		reachedViaLabel.setVisible(showTypeBox);
		typeBox.setVisible(showTypeBox);

		containsCurrentSelection = false;
		Collection<SearchComparison> comparisons = FilterManager.getManager()
				.getComparisons();
		for (SearchComparison c : comparisons) {
			Class<?>[] acceptedTypes = c.getAcceptedTypes();
			boolean match = false;
			for (Class<?> atype : acceptedTypes) {
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
                criterionBox.addActionListener(criterionBoxListener); // Need?
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
                            //                            logger.debug("advancedButton.actionPerformed: " + e); // DEL
				setAspectControlsVisible(!aspectVisible);
                                if (!aspectVisible)
                                    updateFields();
			}

		});
		double[][] sizes = {
				{ TableLayout.FILL },
				{ TableLayout.PREFERRED, TableLayout.PREFERRED,
						TableLayout.PREFERRED } };
		mainPanel.setLayout(new TableLayout(sizes));
		// setLayout(new GridLayout(3, 1));
		valueField = new AutocompleteBox<String>(model);
//		model.clear();  // Need?
		valueField.setAllowNonModelValues(true);
		valueField.setMinLength(1);
                // The default for ComboBox is to show 8 items.  Something is limiting
                // this pulldown to shown 10 items, so might as well have it show all 10
                // rather than 8 with a scrollbar.
		valueField.setMaximumRowCount(10); // Default is 8
                // Color used to highlight selected item in dropdown autocomplete list
                // will be the same as the one used to highlight selected search result
                // (and selected term in OTE).
                valueField.setSelectionBackground(Preferences.getPreferences().getSelectionColor());

		criterionBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				model.clear();
                                // logger.info("criterionBox.actionPerformed: " + e); // DEL
				model.addCriterion((SearchCriterion) criterionBox
						.getSelectedItem());
                                // Is this really needed? (It doesn't seem to solve the issue of updating the statusLabel.)
                                // updateFields(); // Need?
			}
		});

		for (SearchAspect aspect : FilterManager.getManager().getAspects()) {
                    // 5/9/11: No longer calling isValid(aspect)--instead, we leave all the aspects in the aspect menu,
                    // and if user selects Ancestor or Descendent when reasoner is not on, they get a
                    // pop-up error message.
                    //                  if (isValid(aspect))
			aspectBox.addItem(aspect);
		}

		comparisonPanel.add(new JLabel(" that"));
		comparisonPanel.add(comparisonBox);

		Box topBox = new Box(BoxLayout.X_AXIS);
		topBox.add(selectTermsLabel);
		topBox.add(notBox);
		topBox.add(new JLabel(" a "));
		topBox.add(criterionBox);
		topBox.add(comparisonPanel);
		topBox.add(Box.createHorizontalGlue());

		valuePanel.add(new JLabel(" the value "));
		valuePanel.add(valueField);
		valuePanel.add(Box.createHorizontalStrut(20));

		valueBox.add(valuePanel);

		advancedButtonAndAspectLineBox.add(advancedButton);
                // Tried adding this tooltip to the aspectBox but that didn't seem to work.
                JLabel inLabel = new JLabel("  in ") {
                  @Override
                  public String getToolTipText() {
                    return "Note: Ancestor and Descendent are available only if the reasoner is on.";
                  }
                };
                ToolTipManager.sharedInstance().registerComponent(inLabel);
		aspectLineBox.add(inLabel);
                ToolTipManager.sharedInstance().registerComponent(aspectBox);
		aspectLineBox.add(aspectBox);
		aspectLineBox.add(reachedViaLabel);
		aspectLineBox.add(typeBox);
		aspectLineBox.add(Box.createHorizontalStrut(20));
		advancedButtonAndAspectLineBox.add(aspectLineBox);

		mainPanel.add(topBox, "0,0");
		mainPanel.add(valueBox, "0,1");
		mainPanel.add(advancedButtonAndAspectLineBox, "0,2");
		setAspectControlsVisible(false);

		aspectBox.setOpaque(false);
                // This ComboBox was excessively wide.
                // Tried setting preferred size but that didn't do anything.
                aspectBox.setMaximumSize(new Dimension(120, aspectBox.getPreferredSize().height));
		comparisonBox.setOpaque(false);
		criterionBox.setOpaque(false);
		criterionBox.setMaximumRowCount(25); // Show all the options
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

                // 5/9/11: No longer doing this
//                 // If reasoner is turned on or off, may need to update the choices (for aspect or criterion)
// 		SessionManager.getManager().addReasonerStatusListener(
//                   new ReasonerStatusListener() {
//                     // For some reason, this gets triggered like 24 times when the reasoner is turned on or off...
//                     public void statusChanged(ReasonerStatusEvent e) {
// //                      logger.debug("TermFilterEditor: reasoning statusChanged"); // DEL
//                       updateFields();
//                     }
//                   });

		GUIUtil.addReloadListener(reloadListener);
		timer.start();
		layoutGUI();
		updateFields(); // Apparently this is needed, although it seems to get called excessively.
	}

	protected void layoutGUI() {
		setLayout(new GridLayout(1, 1));
		add(mainPanel);
	}

	protected void setAspectControlsVisible(boolean visible) {
		this.aspectVisible = visible;
		if (visible) {
		    advancedButtonAndAspectLineBox.add(aspectLineBox);
		    advancedButton.setIcon(leftIcon);
		} else {
		    advancedButtonAndAspectLineBox.remove(aspectLineBox);
		    advancedButton.setIcon(rightIcon);
		}
		SearchPanel p = getSearchPanel();
		if (p != null)
			p.topmostValidate();
	}

        // This method gets called excessively. Could we maybe cache the filter and
        // only update it when there's some event that warrants it?
	public Filter<IdentifiedObject> getFilter() {
            //            logger.debug("TermFilterEditor.getFilter");
          ObjectFilter out = new ObjectFilterImpl(Preferences.getPreferences().getExcludeObsoletesFromSearches());
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
                        // This call to updateFields results in excessive calls to getFilter when there are TWO aspect boxes visible
                        // (which makes the relation pick lists behave badly)
                        // but just commenting it out prevented the aspect relation lists from updating to
                        // show ontology-specific relations when a new ontology was reloaded.
                        // Solved that by adding a reload listener.
                        //                        updateFields();
                        fireUpdateEvent(); // Need?
                        // fireUpdateEventImmediately(); // stack overflow!
		}
		if (SessionManager.getManager().getUseReasoner())
			out.setReasoner(SessionManager.getManager().getReasoner());
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
            //            logger.debug("fireUpdateEventImmediately");
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
                        // logger.debug("setFilter:  set selected item to " + of.getCriterion()); // DEL
			criterionBox.setSelectedItem(of.getCriterion());
			valueField.setSelectedItem(of.getValue());
			if (of.getNegate())
				notBox.setSelectedIndex(1);
			else
				notBox.setSelectedIndex(0);
			comparisonBox.setSelectedItem(of.getComparison());

				if (!(of.getAspect() instanceof SelfSearchAspect)) {
					setAspectControlsVisible(true);
					// logger.debug("setFilter: aspectBox.setSelectedItem(" + of.getAspect()); // DEL
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
