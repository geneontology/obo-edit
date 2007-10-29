package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyBoundsAdapter;
import java.awt.event.HierarchyEvent;
import java.awt.image.ImageObserver;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.Timer;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.swing.SwingUtil;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;

public class TermImageDisplayComponent extends AbstractGUIComponent {

	protected Image image;

	protected String imageLoc;

	protected JLabel label = new JLabel();

	protected JComponent comp = new JComponent() {
		@Override
		public void paint(Graphics g) {
			super.paint(g);
			Rectangle r = g.getClipBounds();
			if (image != null) {
				g.drawImage(image, r.x, r.y, null);
			}
		}
	};

	protected Timer timer;

	public static String getImmediateFile(String id) {
		File f = new File(GUIManager.getManager().getPrefsDir(), "images/"
				+ id.replace(':', '_') + ".jpg");
		if (f.exists())
			try {
				return f.toURL().toString();
			} catch (MalformedURLException e) {
				return null;
			}
		else
			return null;
	}

	public static String getFile(LinkedObject lo) {
		OBOProperty partof = (OBOProperty) SessionManager.getManager()
				.getSession().getObject("part_of");
		lo = getBestObject(SessionManager.getManager().getReasoner(), partof,
				lo);
		if (lo == null)
			return null;
		else
			return getImmediateFile(lo.getID());
	}

	public static LinkedObject getBestObject(ReasonedLinkDatabase reasoner,
			OBOProperty prop, LinkedObject lo) {
		if (getImmediateFile(lo.getID()) != null)
			return lo;
		List<LinkedObject> superparts = new ArrayList<LinkedObject>(reasoner
				.getParentsOfType(lo, prop));
		List<LinkedObject> temp = new LinkedList<LinkedObject>();
		for (int i = 0; i < superparts.size(); i++)
			if (getImmediateFile(superparts.get(i).getID()) != null) {
				temp.add(superparts.get(i));
			}
		superparts = temp;
		superparts = ReasonerUtil.getMostSpecific(reasoner, superparts, prop);
		if (superparts.size() > 0) {
			return superparts.iterator().next();
		} else
			return null;
	}

	protected SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			image = null;
			LinkedObject lo = e.getSelection().getTermSubSelection();
			String s = getImmediateFile(lo.getID());
			if (s == null) {
				OBOProperty partof = (OBOProperty) SessionManager.getManager()
						.getSession().getObject("part_of");
				if (partof == null)
					return;
				ReasonedLinkDatabase reasoner = SessionManager.getManager()
						.getReasoner();
				LinkedObject settleForMe = getBestObject(reasoner, partof, e
						.getSelection().getTermSubSelection());
				if (settleForMe != null)
					s = getImmediateFile(settleForMe.getID());
				if (s == null) {
					label.setText("No image found");
				} else
					label.setText("No image found for " + lo.getName()
							+ ", showing image for " + settleForMe.getName());
				add(label, "South");
			} else {
				remove(label);
			}
			if (s != null) {
				imageLoc = s;
				buildImage();
			}
			validate();
			repaint();
		}
	};

	protected class TimerActionListener implements ActionListener {
		protected Timer timer;

		protected MediaTracker tracker;

		public TimerActionListener(MediaTracker tracker) {
			this.tracker = tracker;
		}

		public void actionPerformed(ActionEvent e) {
			if (tracker.checkAll())
				timer.stop();
			repaint();
		}

		public void setTimer(Timer timer) {
			this.timer = timer;
		}
	}

	protected void buildImage() {
		if (timer != null) {
			if (timer.isRunning())
				timer.stop();
			timer = null;
		}
		if (comp.getWidth() > 0 && comp.getHeight() > 0 && imageLoc != null) {
			final MediaTracker tracker = new MediaTracker(comp);
			image = Toolkit.getDefaultToolkit().getImage(imageLoc)
					.getScaledInstance(comp.getWidth(), comp.getHeight(),
							Image.SCALE_FAST);
			tracker.addImage(image, 0);
			TimerActionListener listener = new TimerActionListener(tracker);
			final Timer newTimer = new Timer(200, listener);
			listener.setTimer(newTimer);
			timer = newTimer;
			timer.start();
		}
	}

	protected File directory;

	public TermImageDisplayComponent(String id) {
		super(id);
		setLayout(new BorderLayout());
		add(comp, "Center");
		directory = new File(GUIManager.getManager().getPrefsDir(), "images");
		addHierarchyBoundsListener(new HierarchyBoundsAdapter() {
			@Override
			public void ancestorResized(HierarchyEvent arg0) {
				buildImage();
			}
		});
	}

	protected void buildMap() {
		for (File file : directory.listFiles()) {
			String name = file.getName();
			int dotIndex = name.indexOf(".");
			String id;
			if (dotIndex >= 0) {
				id = name.substring(0, dotIndex).replace('_', ':');
			} else
				id = name;
		}
	}

	@Override
	public void init() {
		super.init();
		buildMap();
		SelectionManager.getManager().addSelectionListener(selectionListener);
	}

	@Override
	public void cleanup() {
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);
	}

}
