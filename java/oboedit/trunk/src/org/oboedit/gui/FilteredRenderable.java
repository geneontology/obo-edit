package org.oboedit.gui;

import java.util.List;

import org.obo.filters.*;
import org.oboedit.gui.filter.RenderedFilter;

public interface FilteredRenderable extends ObjectSelector {

	public void addObjectRenderer(RenderedFilter pair);
	public void removeObjectRenderer(RenderedFilter pair);
	public List<RenderedFilter> getObjectRenderers();
	public void setObjectRenderers(List<RenderedFilter> renderers);
	
	public void addAutomaticObjectRenderer(RenderedFilter pair);
	public void removeAutomaticObjectRenderer(RenderedFilter pair);
	public List<RenderedFilter> getAutomaticObjectRenderers();
	
	public void addLinkRenderer(RenderedFilter renderer);
	public void removeLinkRenderer(RenderedFilter renderer);
	public List<RenderedFilter> getLinkRenderers();	
	public void setLinkRenderers(List<RenderedFilter> renderers);
	
	public void setNodeLabelProvider(NodeLabelProvider provider);
	public NodeLabelProvider getNodeLabelProvider();
	
	public void redraw();
}
