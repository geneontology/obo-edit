package org.oboeditplugins.imageplugin.component;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyBoundsAdapter;
import java.awt.event.HierarchyEvent;
import java.awt.event.MouseEvent;
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

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.swing.ScalingComponent;
import org.bbop.swing.SwingUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboeditplugins.imageplugin.util.ImageUtil;

public class TermImageDisplayComponent extends AbstractGUIComponent {

	protected Image image;

	protected String imageLoc;

	protected JLabel label = new JLabel();

	protected JLabel imageLabel = new JLabel();

	protected SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			image = null;
			LinkedObject lo = e.getSelection().getTermSubSelection();
			if (lo == null) {
				imageLabel.setText("");
				return;
			}
			String s = ImageUtil.getImmediateFile(lo.getID());
			if (s == null) {
				OBOProperty partof = (OBOProperty) SessionManager.getManager()
						.getSession().getObject("part_of");
				if (partof == null)
					return;
				ReasonedLinkDatabase reasoner = SessionManager.getManager()
						.getReasoner();
				LinkedObject settleForMe = ImageUtil.getBestObject(reasoner,
						partof, e.getSelection().getTermSubSelection());
				if (settleForMe != null)
					s = ImageUtil.getImmediateFile(settleForMe.getID());
				else {

				}
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
				imageLabel.setText("<html><img src='" + s + "'></html>");
			} else
				imageLabel.setText("");
			validate();
			repaint();
		}
	};

	protected File directory;

	public TermImageDisplayComponent(String id) {
		super(id);
		setLayout(new BorderLayout());
		add(new ScalingComponent(imageLabel), "Center");
		directory = new File(GUIManager.getManager().getPrefsDir(), "images");
	}

	protected void buildMap() {
		if (!directory.exists())
			directory.mkdirs();
		System.err.println("directory = " + directory);
		File[] files = directory.listFiles();
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
		try {
			buildMap();
		} catch (Throwable t) {
			t.printStackTrace();
		}
		SelectionManager.getManager().addSelectionListener(selectionListener);
	}

	@Override
	public void cleanup() {
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);
	}

}
