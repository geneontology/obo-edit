package org.oboedit.gui;

import java.util.List;

import org.obo.filters.*;

public interface FilteredRenderable {

	public void addObjectRenderer(RenderedFilter pair);

	public void removeObjectRenderer(RenderedFilter pair);

	public List<RenderedFilter> getObjectRenderers();
	
	public void addLinkRenderer(RenderedFilter renderer);
	public void removeLinkRenderer(RenderedFilter renderer);
	public List<RenderedFilter> getLinkRenderers();
}
