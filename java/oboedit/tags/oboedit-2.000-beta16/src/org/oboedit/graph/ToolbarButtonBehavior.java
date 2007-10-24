package org.oboedit.graph;

import java.awt.Cursor;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.oboedit.gui.components.LinkDatabaseCanvas;

public interface ToolbarButtonBehavior {
	
	public void activate(LinkDatabaseCanvas canvas);
	public void deactivate(LinkDatabaseCanvas canvas);
	public Icon getButtonIcon();
	public String getButtonLabel();
	public String getTooltip();
	public JComponent getConfigurationPanel();
}
