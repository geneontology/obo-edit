package org.oboedit.graph;

import java.awt.geom.Rectangle2D;
import java.util.Map;

import org.bbop.swing.ShapeUtil;
import org.bbop.util.CycleState;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.BoundsUtil;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PCamera;
import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.util.PBounds;

import org.apache.log4j.*;

public abstract class BoundsGuarantorCycleState extends ActivityCycleState {

	protected final static Logger logger = Logger.getLogger(BoundsGuarantorCycleState.class);
	
	protected BoundsGuarantor boundsGuarantor;

	public void setBoundsGuarantor(BoundsGuarantor boundsGuarantor) {
		this.boundsGuarantor = boundsGuarantor;
	}

	public boolean getZoom() {
		return true;
	}

	public boolean alwaysActivate() {
		return true;
	}

	@Override
	public synchronized void apply() {
		super.apply();
		boundsGuarantor.scheduleBoundsChange(activity);
	}

	public PActivity getActivity() {
		Rectangle2D bounds = getNewBounds();
		if (bounds != null)
			// && !canvas.getCamera().getViewBounds().intersects(bounds)
			// && !canvas.getCamera().getViewBounds().contains(bounds))
			if (getZoom())
				return canvas.getCamera().animateViewToCenterBounds(bounds,
						getZoom(), getBoundsDuration());
			else
				return canvas.getCamera().animateViewToPanToBounds(bounds,
						getBoundsDuration());
		return null;
	}

	public boolean isActive() {
		Rectangle2D bounds = getNewBounds();
		if (bounds == null) {
			logger.error("Null bounds!");
			return true;
		}
		Rectangle2D camBounds = canvas.getCamera().getViewBounds();
		logger.error("isActive:  CAM BOUNDS = " + camBounds);
		logger.error("isActive: NODE BOUNDS = " + bounds);
		if (getZoom())
			return BoundsUtil
					.snugFit(bounds, camBounds, BoundsUtil.DEFAULT_ERR);
		else
			return camBounds.contains(bounds);
	}

	public abstract Rectangle2D getNewBounds();

	public long getBoundsDuration() {
		return canvas.getLayoutDuration();
	}
}
