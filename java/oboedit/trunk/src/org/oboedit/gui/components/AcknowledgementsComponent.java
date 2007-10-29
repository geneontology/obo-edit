package org.oboedit.gui.components;

import java.awt.GridLayout;

import javax.swing.JLabel;

import org.bbop.framework.AbstractGUIComponent;

public class AcknowledgementsComponent extends AbstractGUIComponent {
	
	protected JLabel label = new JLabel();
	public AcknowledgementsComponent(String id) {
		super(id);
		label.setText("<html>" +
				"<font size='+3'>Acknowledgements</font><br>" +
				"<font size='+2'>" +
				"<ul>"+
				"<li>The OBO-Edit Working Group"+
				"<li>The Gene Ontology Consortium"+
				"<li>Berkeley Bioinformatics and Ontologies Project"+
				"</ul>"+
				"</font></html>");
		setLayout(new GridLayout(1,1));
		add(label);
	}
}
