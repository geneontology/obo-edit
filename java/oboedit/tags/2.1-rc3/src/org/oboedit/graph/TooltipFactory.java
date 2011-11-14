package org.oboedit.graph;

import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.PNode;

public interface TooltipFactory {
	public static final Object KEY = "tooltipFactory";
	public static final Object TEXT_KEY = "tooltipText";
	
	public PNode getTooltip(LinkDatabaseCanvas canvas, PNode node);
	public void destroyTooltip(PNode tooltip);
	public void addTooltipChangeListener(TooltipChangeListener listener);
	public void removeTooltipChangeListener(TooltipChangeListener listener);
	public long getDelay();
}
