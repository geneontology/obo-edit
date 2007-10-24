package org.oboedit.gui.components;

import java.awt.BorderLayout;

import org.bbop.framework.ComponentConfiguration;
import org.obo.filters.Filter;

public class OntologyEditorConfiguration implements ComponentConfiguration {
	public static final int SHOW_TOOLBAR_NEVER = 0;
	public static final int SHOW_TOOLBAR_ON_HOTKEY = 1;
	public static final int SHOW_TOOLBAR_ALWAYS = 2;
	
	protected Filter<?> linkFilter;
	protected Filter<?> termFilter;
	protected int showToolbar = SHOW_TOOLBAR_ON_HOTKEY;
	protected String toolbarPosition = BorderLayout.NORTH;
	protected String dragActionID;
	protected String rootAlgorithm = null;
	protected boolean live = true;
	protected boolean revertToDefaultAction = true;

	public OntologyEditorConfiguration() {
	}

	public OntologyEditorConfiguration(Filter<?> termFilter,
			Filter<?> linkFilter, int showToolbar, String toolbarPosition,
			String dragActionID, boolean revertToDefaultAction, boolean live,
			String rootAlgorithm) {
		super();
		this.termFilter = termFilter;
		this.linkFilter = linkFilter;
		this.showToolbar = showToolbar;
		this.toolbarPosition = toolbarPosition;
		this.dragActionID = dragActionID;
		this.live = live;
		this.rootAlgorithm = rootAlgorithm;
		this.revertToDefaultAction = revertToDefaultAction;
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
}
