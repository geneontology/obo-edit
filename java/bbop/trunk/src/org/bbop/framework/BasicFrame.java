package org.bbop.framework;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;

import javax.swing.JFrame;
import javax.swing.JPanel;

import org.bbop.swing.EnhancedMenuBar;

public class BasicFrame extends JFrame {
	
	protected JPanel mainPanel;


	public BasicFrame(String title) {
		super(title);
		initGUI();
	}
	
	protected void initGUI() {
		setJMenuBar(new EnhancedMenuBar());
	}

}
