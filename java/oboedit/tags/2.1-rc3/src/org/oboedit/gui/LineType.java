package org.oboedit.gui;

import org.bbop.util.EnumPersistenceDelegate;

public enum LineType {
	// pain in the ass thing we have to do to make this serialize properly
	// using XMLEncoder/Decoder
	
	
	SOLID_LINE, DASHED_LINE, ZIGZAG_LINE;
	
	static { EnumPersistenceDelegate.installFor(values()[0].getClass()); }
}
