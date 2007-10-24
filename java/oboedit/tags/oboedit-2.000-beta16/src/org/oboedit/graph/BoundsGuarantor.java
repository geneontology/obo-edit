package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.geom.Line2D;
import java.util.Iterator;

import org.bbop.util.CycleState;
import org.bbop.util.StateCycler;
import org.obo.datamodel.PathCapable;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PLayoutNode;
import org.oboedit.piccolo.PiccoloBoxLayout;
import org.oboedit.piccolo.StatusMessageDisplayer;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.nodes.PText;

public class BoundsGuarantor extends KeyedBehavior {

	protected StateCycler boundsGuarantor = new StateCycler();
	protected StatusMessageDisplayer messageDisplayer;
	protected PActivity boundsChangeActivity;

	public BoundsGuarantor() {
		setKeyCode(KeyEvent.VK_V);

	}

	public synchronized void scheduleBoundsChange(PActivity boundsChangeActivity) {
		PActivity oldBoundsActivity = this.boundsChangeActivity; 
		if (oldBoundsActivity != null
				&& oldBoundsActivity.isStepping()) {
			oldBoundsActivity.terminate();
		}
		this.boundsChangeActivity = boundsChangeActivity;
	}

	public void guaranteeViewBounds() {
		boundsGuarantor.enforceCurrentState();
	}

	public void updateViewBounds() {
		boundsGuarantor.cycleStates();
		messageDisplayer.showStatusMessage(getCyclerMessage(boundsGuarantor),
				3000);
	}

	protected PNode getCyclerMessage(StateCycler cycler) {
		PLayoutNode out = new PLayoutNode();
		out.setLayoutManager(new PiccoloBoxLayout(PiccoloBoxLayout.VERT));
		if (cycler.getDesc() != null) {
			Font headerFont = new Font("Arial", Font.ITALIC, 18);
			PText header = new PText(cycler.getDesc());
			header.setFont(headerFont);
			header.setTextPaint(Color.white);
			out.addChild(header);
			PPath line = new PPath(
					new Line2D.Double(0, 0, header.getWidth(), 0));
			line.setStrokePaint(Color.white);
			line.setStroke(new BasicStroke(1));
			out.addChild(line);
			PNode spacer = new PNode();
			spacer.setBounds(0, 0, 12, 12);
			out.addChild(spacer);
		}
		Font font = new Font("Arial", Font.PLAIN, 12);
		Iterator it = cycler.getStates().iterator();
		while (it.hasNext()) {
			CycleState state = (CycleState) it.next();
			PText text = new PText(state.getDesc());
			text.setFont(font);
			text.setTextPaint(Color.white);
			if (state.equals(cycler.getCurrentState()))
				text.setFont(new Font("Arial", Font.BOLD, 18));
			out.addChild(text);
		}
		return out;
	}

	@Override
	public void install(final LinkDatabaseCanvas canvas) {
		super.install(canvas);
		canvas.addRelayoutListener(new RelayoutListener() {

			public void relayoutComplete() {
				guaranteeViewBounds();
			}

			public void relayoutStarting() {
				guaranteeViewBounds();
			}

		});
		canvas.addFocusedNodeListener(new FocusedNodeListener() {
			public void focusedChanged(PathCapable oldFocus,
					PathCapable newFocus) {
				guaranteeViewBounds();
			}
		});
		installDefaultCyclers();
		messageDisplayer = new StatusMessageDisplayer(canvas.getCamera());
	}

	protected void installDefaultCyclers() {
		addBoundsGuarantor(new PanToFocusedGuarantor(canvas));
		/*
		 * boundsGuarantor.addState(new
		 * ZoomToFocusedNeighborhoodGuarantor(canvas));
		 */
		addBoundsGuarantor(new ZoomToFocusedGuarantor(canvas));
		addBoundsGuarantor(new ZoomToAllGuarantor(canvas));

	}

	public void addBoundsGuarantor(BoundsGuarantorCycleState g) {
		boundsGuarantor.addState(g);
		g.setBoundsGuarantor(this);
	}

	public void removeBoundsGuarantor(BoundsGuarantorCycleState g) {
		boundsGuarantor.addState(g);
		g.setBoundsGuarantor(null);
	}

	@Override
	public void setKeyCode(int keyCode) {
		super.setKeyCode(keyCode);

		boundsGuarantor.setDesc("Changed view mode ("
				+ KeyEvent.getKeyText(keyCode) + " key)");
	}

	@Override
	protected void action() {
		updateViewBounds();
	}
}
