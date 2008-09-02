package org.oboedit.gui.components.imageplugin.saveimage;

/** This is the task that saves an image file of the component when the user clicks on the
    little camera icon in the component titlebar.  The camera icons are installed in
    DefaultGUIStartupTask. */

import java.awt.Component;
import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;

import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUITask;
import org.bbop.framework.IOManager;

public class InstallTask implements GUITask {

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

	public void install() {
		ImageIO.scanForPlugins();
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
		IOManager.getManager().installDataAdapter(new EPSImageAdapter());
	}

	public void shutdown() {
		// Is there anything we should do here??
	}

	public static void main(String[] args) {
		for (String s : ImageIO.getWriterFormatNames()) {
			System.out.println(s);
		}
	}

}
