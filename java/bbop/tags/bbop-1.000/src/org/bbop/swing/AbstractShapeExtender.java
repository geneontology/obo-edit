package org.bbop.swing;

import java.awt.Shape;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.util.LinkedList;
import java.util.List;

public abstract class AbstractShapeExtender implements ShapeExtender {

	public Shape[] extend(Shape a, Shape b) {
		Shape [] out = new Shape[2];
		PathOpList source = new PathOpList(a, false);
		PathOpList target = new PathOpList(b, false);
		/*
		System.err.println("original source =  "+source.toString());
		System.err.println("original target =  "+target.toString());
		*/
		int [] sourceIndices = source.getSubpathIndices();
		int [] targetIndices = target.getSubpathIndices();
		
		int i;
		for(i=0; i < sourceIndices.length && i < targetIndices.length; i++) {
			int endSourceIndex;
			if (i >= sourceIndices.length - 1)
				endSourceIndex = source.size()-1;
			else
				endSourceIndex = sourceIndices[i+1]-1;
			
			int endTargetIndex;
			if (i >= targetIndices.length - 1)
				endTargetIndex = target.size()-1;
			else
				endTargetIndex = targetIndices[i+1]-1;
			
			addPoints(source, sourceIndices[i], endSourceIndex, target, targetIndices[i], endTargetIndex);			
		}
		source.flushPendingOps();
		target.flushPendingOps();
		
		if (i < sourceIndices.length) {
			appendEmptySubpaths(target, source, sourceIndices, sourceIndices.length - targetIndices.length);
		} else if (i < targetIndices.length) {
			appendEmptySubpaths(source, target, targetIndices, targetIndices.length - sourceIndices.length);
		}
		/*
		System.err.println("morphed source =  "+source.toString());
		System.err.println("morphed target =  "+target.toString());
		*/
		out[0] = source.getShape();
		out[1] = target.getShape();
		return out;
	}

	protected void appendEmptySubpaths(PathOpList target, PathOpList source, int[] sourceIndices, int subpathCount) {
		
		Point2D.Float origin;
		if (target.size() == 0)
			origin = new Point2D.Float();
		else
			origin = target.getSegmentEndpoint(target.size()-1, null);

		int startIndex = sourceIndices[sourceIndices.length - subpathCount];
		for(int i=startIndex; i < source.size(); i++) {
			PathOp op = source.getSegment(i);
			PathOp newOp;
			if (op.op == PathIterator.SEG_MOVETO) {
				float [] coords = { origin.x, origin.y};
				newOp = new PathOp(op.op, coords);
				target.addPendingOp(-1, newOp);
			} else if (op.op == PathIterator.SEG_CLOSE) {
				newOp = new PathOp(op.op, new float[0]);
			} else {
				float [] coords = { origin.x, origin.y};
				newOp = new PathOp(PathIterator.SEG_LINETO, coords);
				target.addPendingOp(-1, newOp);			
			}
		}
		target.flushPendingOps();
	}

	protected abstract void addPoints(PathOpList source, int i, int endSourceIndex, PathOpList target, int j, int endTargetIndex);
}
