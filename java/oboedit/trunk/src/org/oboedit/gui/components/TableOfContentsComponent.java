package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;

public class TableOfContentsComponent extends AbstractGUIComponent {
	
	protected ReloadListener listener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			TableOfContentsComponent.this.reload();
		}
	};

	public TableOfContentsComponent(String id) {
		super(id);
	}
	
	public void reload() {
		
	}
	
	@Override
	public void init() {
		super.init();
		reload();
		GUIUtil.addReloadListener(listener);
	}
	
	@Override
	public void cleanup() {
		super.cleanup();
		GUIUtil.removeReloadListener(listener);
	}

}
