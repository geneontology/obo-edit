package org.oboedit.gui.filter;

import java.awt.BorderLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.apache.log4j.*;

public class HTMLSpecEditor extends JPanel implements
	GeneralRendererSpecFieldEditor<String> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HTMLSpecEditor.class);

	JTextArea textArea = new JTextArea();

	public HTMLSpecEditor() {
		setLayout(new BorderLayout());
		add(new JLabel("<html>Enter HTML code for the renderer below."
				+ " Use the variable " + HTMLSpecField.REPLACE_SEQ
				+ " to represent the term name.<br>"
				+ "Do not use the &lt;html&gt; tag in you code -"
				+ " it will be added automatically.</html>"),
				BorderLayout.NORTH);
		add(new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), BorderLayout.CENTER);
	}

	public String getValue() {
		return textArea.getText();
	}

	public void setValue(String o) {
		textArea.setText(o);
	}
}
