package org.oboedit.gui.filter;

import java.awt.GridLayout;

import javax.swing.JPanel;
import javax.swing.JSpinner;

import org.apache.log4j.*;

public class IntegerSpecEditor extends JPanel implements
	GeneralRendererSpecFieldEditor<Integer> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntegerSpecEditor.class);

	protected JSpinner spinner = new JSpinner();

	public IntegerSpecEditor() {
		this(10);
	}

	public IntegerSpecEditor(int defaultValue) {
		spinner.setValue(defaultValue);
		setLayout(new GridLayout(1, 1));
		add(spinner);
	}

	public Integer getValue() {
		return ((Number) spinner.getValue()).intValue();
	}

	public void setValue(Integer o) {
		if (o == null)
			spinner.setValue(10);
		else
			spinner.setValue(o);
	}

}
