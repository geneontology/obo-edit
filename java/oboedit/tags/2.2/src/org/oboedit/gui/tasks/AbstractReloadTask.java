package org.oboedit.gui.tasks;

import org.bbop.framework.GUITask;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.util.GUIUtil;

public abstract class AbstractReloadTask implements GUITask {
	protected ReloadListener listener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (e.isRoot() || e.isReasoner())
				AbstractReloadTask.this.reload();
		}
	};
	
	public abstract void reload();
	
	public void install() {
		GUIUtil.addReloadListener(listener);
	}

	public void shutdown() {
		GUIUtil.removeReloadListener(listener);
	}
}
