package org.oboedit.piccolo;

import java.awt.Shape;
import java.awt.geom.GeneralPath;

import org.bbop.swing.ShapeExtender;
import org.bbop.swing.ShapeMorpher;

import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.activities.PInterpolatingActivity;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PUtil;

public class MorphActivity extends PInterpolatingActivity {

	
	
	protected Shape morphTo;
	protected ShapeExtender shapeExtender;

	public MorphActivity(PPath node, Shape morphTo, ShapeExtender shapeExtender, long duration) {
		super(duration, PUtil.DEFAULT_ACTIVITY_STEP_RATE);
		this.node = node;
		this.morphTo = morphTo;
		this.shapeExtender = shapeExtender;
	}
	
	protected ShapeMorpher morpher;
	protected PPath node;
	protected GeneralPath scratch = new GeneralPath();
	
	protected void activityStarted() {
		morpher = new ShapeMorpher(node.getPathReference(), morphTo, 5, 2, shapeExtender);
		super.activityStarted();
	}

	public void setRelativeTargetValue(float zeroToOne) {				
		Shape shape = morpher.getShapeAtTime(zeroToOne, scratch);
		node.getPathReference().reset();
		node.getPathReference().append(shape, false);
		node.updateBoundsFromPath();
	}

	public boolean isAnimation() {
		return true;
	}
}
