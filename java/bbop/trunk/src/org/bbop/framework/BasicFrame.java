package org.bbop.framework;

import javax.swing.JFrame;
import javax.swing.JPanel;

import org.bbop.swing.EnhancedMenuBar;

import org.apache.log4j.*;

public class BasicFrame extends JFrame {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BasicFrame.class);
	
	protected JPanel mainPanel;


	public BasicFrame(String title) {
		super(title);
		initGUI();
	}
	
	protected void initGUI() {
		setJMenuBar(new EnhancedMenuBar());
	}

}
