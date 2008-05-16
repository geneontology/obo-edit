package org.oboedit.piccolo;

import java.awt.Graphics2D;

import edu.umd.cs.piccolo.PCamera;
import edu.umd.cs.piccolo.util.PPaintContext;

import org.apache.log4j.*;

public class FullPaintCamera extends PCamera {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FullPaintCamera.class);

	public void paintUnclipped(Graphics2D g) {
		PPaintContext paintContext = new PPaintContext(g);

		paintCameraView(paintContext);
	}
}
