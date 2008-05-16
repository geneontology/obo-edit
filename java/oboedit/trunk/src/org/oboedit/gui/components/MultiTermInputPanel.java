package org.oboedit.gui.components;

import java.awt.GridBagLayout;
import javax.swing.JPanel;

import org.apache.log4j.*;

public class MultiTermInputPanel extends JPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiTermInputPanel.class);

	private static final long serialVersionUID = 1L;

	/**
	 * This is the default constructor
	 */
	public MultiTermInputPanel() {
		super();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(300, 200);
		this.setLayout(new GridBagLayout());
	}

}
