package org.oboedit.gui.filter;

import java.awt.Dimension;

import javax.swing.JPanel;

import org.apache.log4j.*;

public class EmptyBooleanEditor extends JPanel implements
	GeneralRendererSpecFieldEditor<Boolean> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EmptyBooleanEditor.class);

	public EmptyBooleanEditor() {
		setMaximumSize(new Dimension(1,1));
	}
	
	public Boolean getValue() {
		return Boolean.TRUE;
	}

	public void setValue(Boolean o) {
	}

}
