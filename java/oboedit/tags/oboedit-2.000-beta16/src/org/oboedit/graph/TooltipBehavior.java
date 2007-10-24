package org.oboedit.graph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Point2D;

import javax.swing.Timer;

import org.oboedit.graph.TooltipFactory;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.WordBubbleNode;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.activities.PActivity.PActivityDelegate;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.util.PPickPath;

public class TooltipBehavior implements ViewBehavior {

	public static long DEFAULT_TOOLTIP_VISIBILITY_DELAY = 1000;

	protected long tooltipVisibleDelay = DEFAULT_TOOLTIP_VISIBILITY_DELAY;

	protected class TooltipEventHandler extends PBasicInputEventHandler {
		protected PNode lastEntered;

		protected TooltipFactory lastTooltipFactory;

		protected PNode currentTooltip;

		protected WordBubbleNode tooltipHolder;

		protected Point2D lastMousePos;

		protected ActionListener timerListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showTooltip();
			}
		};

		public void mouseMoved(PInputEvent event) {
			lastMousePos = event.getPosition();
		}

		public void hideTooltip() {
			hideTooltip(currentTooltip, tooltipHolder);
		}

		public void showTooltip() {
			currentTooltip = lastTooltipFactory.getTooltip(canvas, lastEntered);
			if (currentTooltip != null && lastMousePos != null) {
				Point2D pointsAt = canvas.getCamera().viewToLocal(lastMousePos);
				tooltipHolder = new WordBubbleNode();
				tooltipHolder.setContents(currentTooltip);
				tooltipHolder.setPointsAt(pointsAt);
				tooltipHolder.setBubbleOffset(new Point2D.Double(0, canvas
						.getCamera().getHeight()
						- tooltipHolder.getBubbleBounds().getHeight()));
				canvas.getCamera().addChild(tooltipHolder);
				final PActivity a = tooltipHolder
						.animateScaleInFromPoint(getTooltipFadeInTime());
				canvas.getCamera().addActivity(a);
			}
		}

		public void hideTooltip(final PNode destroyedTooltip,
				final PNode destroyedHolder) {
			if (destroyedHolder != null) {
				PActivity activity = destroyedHolder.animateToTransparency(0,
						getTooltipFadeOutTime());
				System.err.println("activity = "+activity);
				activity.setDelegate(new PActivityDelegate() {
					public void activityFinished(PActivity arg0) {
						if (canvas.getCamera().getChildrenReference().contains(
								destroyedHolder))
							canvas.getCamera().removeChild(destroyedHolder);
					}

					public void activityStarted(PActivity activity) {
					}

					public void activityStepped(PActivity activity) {
					}
				});
			}
		}

		Timer popupTimer = new Timer(Integer.MAX_VALUE, timerListener);
		{
			popupTimer.setRepeats(false);
		}

		public void mouseEntered(PInputEvent event) {
			popupTimer.stop();
			hideTooltip(currentTooltip, tooltipHolder);
			PPickPath path = event.getPath();
			lastEntered = null;
			lastTooltipFactory = null;
			for (int i = path.getNodeStackReference().size() - 1; i >= 0; i--) {
				PNode node = (PNode) path.getNodeStackReference().get(i);
				TooltipFactory tf = (TooltipFactory) node
						.getAttribute(TooltipFactory.KEY);
				if (tf != null) {
					lastEntered = node;
					lastTooltipFactory = tf;
					long delay = tf.getDelay();
					popupTimer.setInitialDelay((int) Math.min(delay,
							getTooltipVisibleDelay()));

					popupTimer.start();
					return;
				}
			}
			// PNode node = event.getPickedNode();
		}

		public void mouseExited(PInputEvent event) {
			popupTimer.stop();
			hideTooltip(currentTooltip, tooltipHolder);
		}
	}

	protected TooltipEventHandler tooltipHandler = new TooltipEventHandler();

	protected RelayoutListener relayoutListener = new RelayoutListener() {
		public void relayoutComplete() {
		}

		public void relayoutStarting() {
			tooltipHandler.hideTooltip();
		}
	};

	protected LinkDatabaseCanvas canvas;

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addRelayoutListener(relayoutListener);
		canvas.addInputEventListener(tooltipHandler);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		if (this.canvas != null) {
			this.canvas.removeInputEventListener(tooltipHandler);
			this.canvas.removeRelayoutListener(relayoutListener);
		}
		this.canvas = null;
	}

	public long getTooltipFadeInTime() {
		return canvas.getLayoutDuration();
	}

	public long getTooltipFadeOutTime() {
		long time = canvas.getLayoutDuration() / 10;
		return Math.max(time, 1);
	}

	public long getTooltipVisibleDelay() {
		return tooltipVisibleDelay;
	}

	public void setTooltipVisibleDelay(long tooltipVisibleDelay) {
		this.tooltipVisibleDelay = tooltipVisibleDelay;
	}

}
