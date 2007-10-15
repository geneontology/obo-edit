package org.bbop.swing.autocomplete;

import java.util.Collection;
import java.util.List;

import org.bbop.util.TaskDelegate;

public interface AutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE> {
	
	public Collection<DISPLAY_TYPE> getAllValues();
	public TaskDelegate<List<MatchPair<DISPLAY_TYPE>>> getObjects(List<String> tokens);
	public boolean isLegal(DISPLAY_TYPE val);
	public OUTPUT_TYPE getOutputValue(DISPLAY_TYPE val);
	public List<DISPLAY_TYPE> getDisplayValues(OUTPUT_TYPE val);
	public DISPLAY_TYPE createValue(String val);
	public String toString(DISPLAY_TYPE val);
	
	public double getWeight(DISPLAY_TYPE val);
	
	public Class<DISPLAY_TYPE> getDisplayType();
	public Class<OUTPUT_TYPE> getOutputType();
}
