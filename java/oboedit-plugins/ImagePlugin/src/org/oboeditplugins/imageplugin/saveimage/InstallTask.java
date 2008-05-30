package org.oboedit.gui.components.imageplugin.saveimage;

import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.swing.Icon;
import javax.swing.JButton;

import net.infonode.docking.View;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUITask;
import org.bbop.framework.IOManager;
import org.bbop.framework.dock.idw.BitmapIcon;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.framework.dock.idw.IDWUtil;
import org.bbop.framework.dock.idw.ViewListener;
import org.bbop.framework.event.GUIComponentEvent;
import org.bbop.framework.event.GUIComponentListener;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.Preferences;

public class InstallTask implements GUITask {

	protected GUIComponentListener componentListener = new GUIComponentListener() {

		public void componentHidden(GUIComponentEvent event) {
			if (event.getComponent() instanceof LinkDatabaseCanvas) {
				LinkDatabaseCanvas canvas = (LinkDatabaseCanvas) event
						.getComponent();
				canvas.removeMenuFactory(menuFactory);
			}
		}

		public void componentShown(GUIComponentEvent event) {
			if (event.getComponent() instanceof LinkDatabaseCanvas) {
				LinkDatabaseCanvas canvas = (LinkDatabaseCanvas) event
						.getComponent();
				canvas.addMenuFactory(menuFactory);
			}
		}

	};

	// Now done in DefaultGUIStartupTask
// 	protected Icon cameraIcon = new BitmapIcon(
//  			Toolkit
//  					.getDefaultToolkit()
//  					.createImage(
//  							getClass()
// 									.getResource(
// //											"/org.oboedit.gui.components.imageplugin/resources/" +
// 										"org/oboedit/gui/components/imageplugin/resources/" +
// 										"tiny_camera_icon.gif")));

// 	protected ViewListener viewListener = new ViewListener() {

// 		public void viewCreated(View v, final GUIComponent c) {
// 			System.out.println("InstallTask.viewCreated " + v); // DEL
// 			installViewButtons(v, c);
// 		}

// 		public void viewDestroyed(View v, GUIComponent c) {
// 			uninstallViewButtons(v, c);
// 		}
// 	};

	protected SaveScreenMenuFactory menuFactory = new SaveScreenMenuFactory();
	protected MultiMap<GUIComponent, JButton> buttonMap =
		new MultiHashMap<GUIComponent, JButton>();

	public static void saveImage(final GUIComponent c) {
		ImagePainter painter = new ImagePainter() {
			public void paint(Graphics2D g) {
				((Component) c).paint(g);
			}

			public int getHeight() {
				return ((Component) c).getHeight();
			}

			public int getWidth() {
				return ((Component) c).getWidth();
			}
		};
		SaveImageUtil.savePainter(painter);
	}

	// Now done in DefaultGUIStartupTask
// 	protected void installViewButtons(View v, final GUIComponent c) {
// 		final JButton cameraButton = IDWUtil.createFlatHighlightButton(
// 				// final JButton custom =
// 				// ButtonFactory.createFlatHighlightButton(
// 				cameraIcon, "Click to save an image file of this component", 0,
// 				null);
// 		System.out.println("installViewButtons: view = " + v + ", component = " + c + ", cameraIcon = " + cameraIcon); // DEL
// 		buttonMap.add(c, cameraButton);
// 		cameraButton.addActionListener(new ActionListener() {
// 			public void actionPerformed(ActionEvent e) {
// 				saveImage(c);
// 			}
// 		});

// 		v.getCustomTitleBarComponents().add(cameraButton);
// 	}
	
	protected void uninstallViewButtons(View v, GUIComponent c) {
		for(JButton button : buttonMap.get(c)) {
			v.getCustomTitleBarComponents().remove(button);
		}
	}

	public void install() {
		ImageIO.scanForPlugins();
		IOManager.getManager().installDataAdapter(new EPSImageAdapter());
		Collection<String> formats = new ArrayList<String>();
		for (String s : ImageIO.getWriterMIMETypes()) {
			Iterator<ImageWriter> it = ImageIO.getImageWritersByMIMEType(s);
			while (it.hasNext()) {
				ImageWriter writer = it.next();
				String formatName = writer.getOriginatingProvider()
						.getFormatNames()[0];
				IOManager.getManager().installDataAdapter(
						new ImageAdapter(formatName, formatName + " adapter"));

			}
		}
		ComponentManager.getManager().addComponentListener(componentListener);
		installMenuFactories();
		if (ComponentManager.getManager().getDriver() instanceof IDWDriver) {
			IDWDriver driver = (IDWDriver) ComponentManager.getManager()
					.getDriver();
//			driver.addViewListener(viewListener);
//			installViewButtons();
		}
	}

	// Now done in DefaultGUIStartupTask
// 	protected void installViewButtons() {
// 		if (ComponentManager.getManager().getDriver() instanceof IDWDriver) {
// 			IDWDriver driver = (IDWDriver) ComponentManager.getManager()
// 					.getDriver();
// 			for (View v : driver.getAllViews()) {
// 				installViewButtons(v, IDWDriver.getComponent(v));
// 			}
// 		}
// 	}

	protected void installMenuFactories() {
		for (GUIComponent c : ComponentManager.getManager()
				.getActiveComponents()) {
			if (c instanceof LinkDatabaseCanvas) {
				LinkDatabaseCanvas canvas = (LinkDatabaseCanvas) c;
				canvas.addMenuFactory(menuFactory);
			}
		}
	}

	protected void uninstallMenuFactories() {
		for (GUIComponent c : ComponentManager.getManager()
				.getActiveComponents()) {
			if (c instanceof LinkDatabaseCanvas) {
				LinkDatabaseCanvas canvas = (LinkDatabaseCanvas) c;
				canvas.removeMenuFactory(menuFactory);
			}
		}
	}

	public void shutdown() {
		ComponentManager.getManager()
				.removeComponentListener(componentListener);
//		if (ComponentManager.getManager().getDriver() instanceof IDWDriver) {
//			IDWDriver driver = (IDWDriver) ComponentManager.getManager()
//					.getDriver();
//			driver.removeViewListener(viewListener);
//		}
		uninstallMenuFactories();
	}

	public static void main(String[] args) {
		for (String s : ImageIO.getWriterFormatNames()) {
			System.out.println(s);
		}
	}

}
