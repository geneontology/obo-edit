package org.obo.filters;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import org.apache.log4j.*;

public class KeywordFilterBuilder extends AbstractFilterEditor implements CompoundEditable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(KeywordFilterBuilder.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected JTextField textField = new JTextField();
	protected Filter filter;

	public KeywordFilterBuilder() {
		setOpaque(false);

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(Box.createVerticalGlue());
		add(textField);
		add(Box.createVerticalGlue());
	}

	public void setShowCompoundFilter(boolean showCompoundFilter) {
		// ignore this
	}

	public void setFilterFactory(FilterFactory filterFactory) {
		// ignore this too
	}

	public void addActionListener(ActionListener listener) {
		textField.addActionListener(listener);
	}

	public void removeActionListener(ActionListener listener) {
		textField.addActionListener(listener);
	}

	public Filter getFilter() {
		return filter;
	}

	protected void populateList(Collection c, Filter filter) {
		if (filter instanceof CompoundFilter)
			populateList(c, (CompoundFilter) filter);
		else if (filter instanceof LinkFilter)
			populateList(c, (LinkFilter) filter);
		else if (filter instanceof ObjectFilter)
			populateList(c, (ObjectFilter) filter);
		else
			logger.info("what do I do with " + filter.getClass());
	}

	protected void populateList(Collection c, CompoundFilter filter) {
		Iterator it = filter.getFilters().iterator();
		while (it.hasNext()) {
			Filter f = (Filter) it.next();
			populateList(c, f);
		}
	}

	protected void populateList(Collection c, LinkFilter filter) {
		populateList(c, filter.getFilter());
	}

	protected void populateList(Collection c, ObjectFilter filter) {
		c.add(filter.getValue());
	}

	public void setFilter(Filter filter) {
		this.filter = filter;
		if (filter == null) {
			textField.setText("");
			return;
		}
		Collection vals = new LinkedList();
		populateList(vals, filter);
		StringBuffer out = new StringBuffer();
		boolean first = true;

		Iterator it = vals.iterator();
		while (it.hasNext()) {
			String s = (String) it.next();
			if (first)
				out.append(s);
			else {
				out.append(" ");
				out.append(s);
			}
			first = false;
		}
		textField.setText(out.toString());
		// load the gui
	}

	protected Collection unescapeValues(String string) {
		Collection out = new LinkedList();
		KeywordSearchCriterion.extractKeywords(out, string);
		/*
		 * StringTokenizer st = new StringTokenizer(string);
		 * while(st.hasMoreTokens()) { String s = st.nextToken(); out.add(s); }
		 */
		return out;
	}

	public void acceptEdits() {
		CompoundFilter cf = new CompoundFilterImpl();
		cf.setBooleanOperation(CompoundFilter.AND);

		Collection c = unescapeValues(textField.getText());
		if (c.size() > 0) {
			Iterator it = c.iterator();
			while (it.hasNext()) {
				String s = (String) it.next();

				ObjectFilter f = new ObjectFilterImpl();
				f.setCriterion(new KeywordSearchCriterion());
				f.setComparison(new ContainsComparison());
				f.setValue(s);

				cf.addFilter(f);
			}
		} else {
			ObjectFilter f = new ObjectFilterImpl();
			f.setCriterion(new AllTextFieldsCriterion());
			f.setComparison(new ContainsComparison());
			f.setValue("");
			cf.addFilter(f);
		}

		filter = cf;
		// this requires a little work
	}

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (textField != null) {
			textField.setFont(font);
			textField.setMaximumSize(new Dimension(Integer.MAX_VALUE, font
					.getSize() + 20));
		}
	}

	public void setButtonColor(Color buttonColor) {
		// no buttons, don't do anything
	}
}
