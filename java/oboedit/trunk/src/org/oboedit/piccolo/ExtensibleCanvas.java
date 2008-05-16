package org.oboedit.piccolo;

import edu.umd.cs.piccolo.PCamera;
import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.PLayer;
import edu.umd.cs.piccolo.PRoot;
import edu.umd.cs.piccolo.util.PUtil;

import org.apache.log4j.*;

public class ExtensibleCanvas extends PCanvas {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExtensibleCanvas.class);
	protected PCamera createDefaultCamera() {
		PRoot r = createRoot();
		PLayer l = createLayer();
		PCamera c = createCamera();
		
		r.addChild(c); 
		r.addChild(l); 
		c.addLayer(l);
		
		return c;	
	}
	
	protected PRoot createRoot() {
		return new ExtensibleRoot();
	}
	
	protected PLayer createLayer() {
		return new PLayer();
	}
	
	protected PCamera createCamera() {
		return new PCamera();
	}
}
