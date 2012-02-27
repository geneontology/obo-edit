package org.oboedit.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.filters.Filter;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterImpl;
import org.obo.filters.ObjectFilter;
import org.apache.log4j.*;

public class LinkFilterEditor extends TermFilterEditor {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkFilterEditor.class);

	protected static String[] values = { "Self", "Type", "Child", "Parent" };
	protected JComboBox aspectBox;
	protected JLabel selectLinkLabel;

	protected ActionListener linkAspectBoxListener = new BasicActionListener();

	public LinkFilterEditor() {
		super();
		aspectBox.addActionListener(linkAspectBoxListener);
	}
	
	@Override
	public void setFilter(Filter filter) {
		if (filter instanceof LinkFilter) {
			aspectBox.setSelectedItem(getNameForValue(((LinkFilter) filter)
					.getAspect()));
			ObjectFilter ofilter = ((LinkFilter) filter).getFilter();
			super.setFilter(ofilter);
		} else
			throw new IllegalArgumentException("Cannot load non-link filter");
	}
	
	@Override
	protected Class<?> getInputClass() {
		if (aspectBox.getSelectedIndex() == 0)
			return Link.class;
		else
			return IdentifiedObject.class;
	}
	
	@Override
	protected void updateFields() {
		aspectBox.removeActionListener(linkAspectBoxListener);
		super.updateFields();
		aspectBox.addActionListener(linkAspectBoxListener);
	}

	@Override
	protected void layoutGUI() {
		if (aspectBox == null)
			aspectBox = new JComboBox(values);
		if (selectLinkLabel == null)
			selectLinkLabel = new JLabel("Find links where ");
		Box northPanel = Box.createHorizontalBox();
		northPanel.add(selectLinkLabel);
		northPanel.add(aspectBox);
		northPanel.add(Box.createHorizontalGlue());
		notBox.setRenderer(new DefaultListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				if (index == 0)
					value = "has";
				return super.getListCellRendererComponent(list, value, index,
						isSelected, cellHasFocus);
			}
		});
//		selectTermsLabel.setText("that");
		selectTermsLabel.setText("");
		setLayout(new BorderLayout());
		add(northPanel, "North");
		add(mainPanel, "Center");
	}

	protected static String getNameForValue(int val) {
		if (val == LinkFilter.TYPE)
			return "Type";
		else if (val == LinkFilter.SELF)
			return "Self";
		else if (val == LinkFilter.CHILD)
			return "Child";
		else if (val == LinkFilter.PARENT)
			return "Parent";
		else
			return null;
	}

	protected static int getValueForName(String name) {
		if (name.equals("Type"))
			return LinkFilter.TYPE;
		else if (name.equals("Self"))
			return LinkFilter.SELF;
		else if (name.equals("Child"))
			return LinkFilter.CHILD;
		else if (name.equals("Parent"))
			return LinkFilter.PARENT;
		else
			return -1;
	}

	public Filter getFilter() {
		ObjectFilter filter = (ObjectFilter) super.getFilter();
		LinkFilter linkFilter = new LinkFilterImpl();
		linkFilter.setFilter(filter);
		linkFilter.setAspect(getValueForName((String) aspectBox
				.getSelectedItem()));
		//		logger.debug("linkFilter.getFilter " + linkFilter);
		//		logger.debug("linkFilter.getFilter: calling updateFields"); // DEL
                // This call to updateFields results in excessive calls to getFilter when there are TWO aspect boxes visible
                // (which makes the relation pulldown lists behave badly).
                // Doesn't seem to be needed anymore to update relation lists, probably
                // because of other changes I've made.
                //		updateFields(); // This is needed in order to properly update pulldown menus, although getFilter gets called like 30 times, so it seems a bit wasteful.
		return linkFilter;
	}
}
