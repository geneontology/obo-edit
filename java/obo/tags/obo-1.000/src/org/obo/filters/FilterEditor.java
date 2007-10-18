package org.obo.filters;

import java.awt.event.ActionListener;
import java.awt.Color;

public interface FilterEditor {
	public void addFilterEditUpdateListener(FilterEditUpdateListener listener);
	public void removeFilterEditUpdateListener(FilterEditUpdateListener listener);
	
	public void addActionListener(ActionListener listener);

	public void removeActionListener(ActionListener listener);

	public Filter getFilter();

	public void setFilter(Filter filter);

	public void setButtonColor(Color buttonColor);

	public void acceptEdits();
}
