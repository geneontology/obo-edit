package org.oboedit.gui.components.imageplugin.component;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.image.ImageObserver;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.io.FileUtil;
import org.bbop.swing.ScalingComponent;
import org.bbop.swing.SwingUtil;
import org.bbop.util.ClassUtil;
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
import org.oboedit.gui.components.imageplugin.util.ImageUtil;
import org.apache.log4j.*;

public class TermImageDisplayComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermImageDisplayComponent.class);

	protected JLabel explanation = new JLabel();

	protected JLabel imageLabel = new JLabel();

	protected SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			LinkedObject lo = e.getSelection().getTermSubSelection();
			if (lo == null) {
			    imageLabel.setText("<html></html>");
			    return;
			}
			String s = ImageUtil.getImmediateFile(lo.getID());
			if (s == null) {  // No image file for this term
				// See if it has a parent term to try getting the image for
				OBOProperty partof = (OBOProperty) SessionManager.getManager()
						.getSession().getObject("part_of");
				if (partof == null) {  // No parent term to try
					imageLabel.setText("<html></html>");
					repaint();
					return;
				}
				ReasonedLinkDatabase reasoner = SessionManager.getManager()
					.getReasoner();
				LinkedObject settleForMe = ImageUtil.getBestObject(reasoner,
										   partof, e.getSelection().getTermSubSelection());
				if (settleForMe != null)
					s = ImageUtil.getImmediateFile(settleForMe.getID());

				if (s != null)
					explanation.setText("No image found for " + lo.getName()
							+ ", showing image for " + settleForMe.getName());
				add(explanation, "South");
			} else {
				remove(explanation);
			}
			if (s != null) {
				imageLabel.setText("<html><img src='" + s + "'></html>");
			} else {
				imageLabel.setText("<html></html>");
			}
			validate();
			repaint();
		}
	};

	protected File directory;

	public TermImageDisplayComponent(String id) {
		super(id);
		setLayout(new BorderLayout());
		add(new ScalingComponent(imageLabel), "Center");
		createTermImagesFromResource();
	}

	private void createTermImagesFromResource() {
		directory = new File(GUIManager.getManager().getPrefsDir(), "images");
		logger.info("Directory for term images: " + directory);
		// See if user's directory for term images exists yet--if not, create it
		// and install the term images we have in the jar.
		if (!directory.exists()) {
			logger.info(directory + " not found--getting term image files from jar.");
			directory.mkdirs();
			// Copy all term images from resources to this directory
			List<URL> builtInImages = ClassUtil.getResources(Preferences.class.getClassLoader(),
									 "/org/oboedit/gui/resources/term-images/**", "gif",
									 "jpg", "svg");
			String available = "";
			for (final URL url : builtInImages) {
				String path = url.getPath().substring(url.getPath().indexOf("!") + 1);
				String imageFile = path.substring(path.lastIndexOf("/")+1);
				String goTerm = imageFile.substring(0, imageFile.indexOf(".")-1);
				available += " " + goTerm + "\n";
//				System.out.println("url = " + url.getFile() + ", path = " + path); // DEL
				try {
					FileUtil.ensureExists(new File(directory.toString() + "/" + imageFile), path);
				} catch (Exception e) {
					logger.warn("Couldn't create " + directory.toString() + "/" + imageFile + " from resource " + path);
				}
			}
			JOptionPane.showMessageDialog(null, "Terms for which image files were found:\n" + available);
		}
	}

	protected void buildMap() {
		if (!directory.exists())
			directory.mkdirs();
//		System.err.println("directory = " + directory);
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
