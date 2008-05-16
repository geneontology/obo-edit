package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.List;

import org.bbop.framework.ComponentConfiguration;
import org.obo.filters.Filter;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.filter.RenderedFilter;

import org.apache.log4j.*;

public class OntologyEditorConfiguration implements ComponentConfiguration {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OntologyEditorConfiguration.class);
	public static final int SHOW_TOOLBAR_NEVER = 0;
	public static final int SHOW_TOOLBAR_ON_HOTKEY = 1;
	public static final int SHOW_TOOLBAR_ALWAYS = 2;

	protected Filter<?> linkFilter;
	protected Filter<?> termFilter;
	protected List<RenderedFilter> linkRenderers = new ArrayList<RenderedFilter>();
	protected List<RenderedFilter> objectRenderers = new ArrayList<RenderedFilter>();
	protected int showToolbar = SHOW_TOOLBAR_ON_HOTKEY;
	protected String toolbarPosition = BorderLayout.NORTH;
	protected String dragActionID;
	protected String rootAlgorithm = null;
	protected boolean live = true;
	protected boolean revertToDefaultAction = true;
	protected String basicHTML = "$name$";

	public OntologyEditorConfiguration() {
	}

	public OntologyEditorConfiguration(Filter<?> termFilter,
			Filter<?> linkFilter, List<RenderedFilter> objectRenderers,
			List<RenderedFilter> linkRenderers, String basicHTML,
			int showToolbar, String toolbarPosition, String dragActionID,
			boolean revertToDefaultAction, boolean live, String rootAlgorithm) {
		super();
		this.termFilter = termFilter;
		this.linkFilter = linkFilter;
		this.showToolbar = showToolbar;
		this.toolbarPosition = toolbarPosition;
		this.dragActionID = dragActionID;
		this.live = live;
		this.rootAlgorithm = rootAlgorithm;
		this.revertToDefaultAction = revertToDefaultAction;
		this.linkRenderers = linkRenderers;
		this.objectRenderers = objectRenderers;
		if (basicHTML != null)
			this.basicHTML = basicHTML;
	}

	public int getShowToolbar() {
		return showToolbar;
	}

	public void setShowToolbar(int showToolbar) {
		this.showToolbar = showToolbar;
	}

	public String getDragActionID() {
		return dragActionID;
	}

	public void setDragActionID(String dragActionID) {
		this.dragActionID = dragActionID;
	}

	public boolean isLive() {
		return live;
	}

	public void setLive(boolean live) {
		this.live = live;
	}

	public String getRootAlgorithm() {
		return rootAlgorithm;
	}

	public void setRootAlgorithm(String rootAlgorithm) {
		this.rootAlgorithm = rootAlgorithm;
	}

	public Filter<?> getLinkFilter() {
		return linkFilter;
	}

	public void setLinkFilter(Filter<?> linkFilter) {
		this.linkFilter = linkFilter;
	}

	public Filter<?> getTermFilter() {
		return termFilter;
	}

	public void setTermFilter(Filter<?> termFilter) {
		this.termFilter = termFilter;
	}

	public boolean isRevertToDefaultAction() {
		return revertToDefaultAction;
	}

	public void setRevertToDefaultAction(boolean revertToDefaultAction) {
		this.revertToDefaultAction = revertToDefaultAction;
	}

	public String getToolbarPosition() {
		return toolbarPosition;
	}

	public void setToolbarPosition(String toolbarPosition) {
		this.toolbarPosition = toolbarPosition;
	}

	public List<RenderedFilter> getLinkRenderers() {
		return linkRenderers;
	}

	public void setLinkRenderers(List<RenderedFilter> linkRenderers) {
		this.linkRenderers = linkRenderers;
	}

	public List<RenderedFilter> getObjectRenderers() {
		return objectRenderers;
	}

	public void setObjectRenderers(List<RenderedFilter> objectRenderers) {
		this.objectRenderers = objectRenderers;
	}

	public String getBasicHTML() {
		return basicHTML;
	}

	public void setBasicHTML(String basicHTML) {
		this.basicHTML = basicHTML;
	}
}
