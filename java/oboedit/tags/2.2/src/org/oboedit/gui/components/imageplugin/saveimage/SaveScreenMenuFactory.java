package org.oboedit.gui.components.imageplugin.saveimage;

import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;
import javax.swing.JMenuItem;

import org.oboedit.graph.RightClickMenuFactory;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.FullPaintCamera;
import edu.umd.cs.piccolo.event.PInputEvent;

public class SaveScreenMenuFactory implements RightClickMenuFactory {

	public static interface ScreenSaveDriver {

	}

	public SaveScreenMenuFactory() {
	}

	public List<JMenuItem> getMenuItems(final LinkDatabaseCanvas canvas,
			PInputEvent e) {
		JMenuItem item = new JMenuItem("Save current graph image...");

		item.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				saveCanvas(canvas);
			}

		});
		List<JMenuItem> items = new LinkedList<JMenuItem>();
		items.add(item);
		return items;
	}

	protected void saveCanvas(final LinkDatabaseCanvas canvas) {
		ImagePainter painter = new ImagePainter() {
			public void paint(Graphics2D g) {
				FullPaintCamera camera = (FullPaintCamera) canvas.getCamera();
				camera.paintUnclipped(g);
			}

			public int getHeight() {
				return (int) canvas.getCamera().getUnionOfLayerFullBounds().getHeight();
			}

			public int getWidth() {
				return (int) canvas.getCamera().getUnionOfLayerFullBounds().getWidth();
			}
		};
		SaveImageUtil.savePainter(painter);
	}

}
