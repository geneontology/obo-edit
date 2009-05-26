package org.oboedit.gui.components.ontologyGeneration;

import java.awt.BorderLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class FilterTextField extends JPanel
{
	private static final long serialVersionUID = 8878237220844157476L;
	
	private JTextField textField;
	private JLabel label;
	
	public FilterTextField(boolean needsFilterIcon) {
		BorderLayout borderLayout = new BorderLayout();
		this.setLayout(borderLayout);
    	textField = new JTextField(5);
    	if (needsFilterIcon)
    		label = new JLabel(new ImageIcon(getClass().getResource("resources/filterIcon.png")));
    	else
    		label = new JLabel(new ImageIcon(getClass().getResource("resources/searchIcon.png")));
	    label.setOpaque(true);  
	    label.setBackground(textField.getBackground());
	    this.setBorder(textField.getBorder());
	    textField.setBorder(null);
	    this.setBackground(textField.getBackground());
	    this.add(label, BorderLayout.WEST);
	    this.add(textField, BorderLayout.CENTER);
	}
	
	public JTextField getTextField() {
		return textField;
	}
}
