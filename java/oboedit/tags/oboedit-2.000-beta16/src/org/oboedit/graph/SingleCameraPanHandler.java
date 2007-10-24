package org.oboedit.graph;

import java.awt.event.MouseEvent;

import edu.umd.cs.piccolo.PCamera;
import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PPanEventHandler;

public class SingleCameraPanHandler extends PPanEventHandler {

	protected void pan(PInputEvent e) {
		if (isDefaultCamera(e)) {
			super.pan(e);
		}
	}

	protected boolean isDefaultCamera(PInputEvent e) {
		return e.getComponent() instanceof PCanvas
				&& ((PCanvas) e.getComponent()).getCamera().equals(
						e.getCamera());
	}

	protected void dragActivityStep(PInputEvent e) {
		if (isDefaultCamera(e)) {
			super.dragActivityStep(e);
		}
	}
}
