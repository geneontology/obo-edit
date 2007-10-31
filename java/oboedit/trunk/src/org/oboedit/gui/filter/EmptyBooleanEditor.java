package org.oboedit.gui.filter;

import java.awt.Dimension;

import javax.swing.JPanel;

public class EmptyBooleanEditor extends JPanel implements
		GeneralRendererSpecFieldEditor<Boolean> {

	public EmptyBooleanEditor() {
		setMaximumSize(new Dimension(1,1));
	}
	
	public Boolean getValue() {
		return Boolean.TRUE;
	}

	public void setValue(Boolean o) {
	}

}
