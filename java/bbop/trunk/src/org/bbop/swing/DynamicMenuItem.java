package org.bbop.swing;

import java.awt.Component;
import java.util.List;


public interface DynamicMenuItem {

	public List<? extends Component> getItems();
	
	public boolean getMerge();
	
	public boolean bracketTop();
	
	public boolean bracketBottom();
}
