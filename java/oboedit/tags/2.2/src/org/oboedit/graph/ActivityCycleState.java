package org.oboedit.graph;

import org.bbop.util.CycleState;
import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.activities.PActivity;

public abstract class ActivityCycleState implements CycleState {

	protected LinkDatabaseCanvas canvas;
	protected String desc;
	protected PActivity activity;

	public boolean alwaysActivate() {
		return false;
	}

	public int getTerminationMode() {
		return PActivity.TERMINATE_WITHOUT_FINISHING;
	}

	public synchronized void apply() {
		if (activity != null) {
			activity.terminate(getTerminationMode());
		}
		activity = getActivity();
		if (activity != null) {
			canvas.getRoot().addActivity(activity);
		}
	}

	public abstract PActivity getActivity();

	public String getDesc() {
		return desc;
	}

	public void setDesc(String desc) {
		this.desc = desc;
	}

	public void halt() {
		if (activity != null) {
			activity.terminate(getTerminationMode());
		}
	}

	public LinkDatabaseCanvas getCanvas() {
		return canvas;
	}

	public void setCanvas(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
	}
}
