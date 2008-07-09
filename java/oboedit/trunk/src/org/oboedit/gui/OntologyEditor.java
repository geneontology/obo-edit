package org.oboedit.gui;

import javax.swing.JToggleButton;

public interface OntologyEditor extends ObjectSelector {
	public void setLiveButton(JToggleButton button);
	public JToggleButton getLiveButton();
}
