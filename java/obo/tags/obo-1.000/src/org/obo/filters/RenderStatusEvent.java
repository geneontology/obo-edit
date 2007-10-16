package org.obo.filters;

import java.util.EventObject;

public class RenderStatusEvent extends EventObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected boolean rendererVisible;

	public RenderStatusEvent(Object source, boolean rendererVisible) {
		super(source);
		this.rendererVisible = rendererVisible;
	}

	public boolean isRendererVisible() {
		return rendererVisible;
	}
}
