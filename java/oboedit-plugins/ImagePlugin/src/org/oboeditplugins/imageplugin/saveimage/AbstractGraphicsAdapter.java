package org.oboeditplugins.imageplugin.saveimage;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.util.Arrays;

import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterUI;
import org.bbop.dataadapter.DefaultIOOperation;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.bbop.dataadapter.FileAdapterUI;
import org.bbop.dataadapter.IOOperation;
import org.bbop.util.CollectionUtil;
import org.obo.dataadapter.AbstractAdapter;

public abstract class AbstractGraphicsAdapter extends AbstractAdapter {
	public static IOOperation<ImagePainter, Void> STORE_IMAGE = new DefaultIOOperation<ImagePainter, Void>(
			"store_image", "Store Image", ImagePainter.class, Void.class);

	protected AdapterConfiguration config;

	public AdapterConfiguration getConfiguration() {
		return config;
	}

	public DataAdapterUI getPreferredUI() {
		FileAdapterUI ui = new FileAdapterUI();
		ui.setReadOperation(null);
		ui.setWriteOperation(STORE_IMAGE);
		return ui;
	}

	public IOOperation[] getSupportedOperations() {
		return CollectionUtil.array(STORE_IMAGE);
	}

	protected abstract Graphics2D getGraphics(String path, int width, int height)
			throws DataAdapterException;

	protected abstract void flushGraphics(Graphics2D g, String path)
			throws DataAdapterException;

	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			AdapterConfiguration configuration, INPUT_TYPE input)
			throws DataAdapterException {
		if (op.equals(STORE_IMAGE)) {
			if (configuration instanceof FileAdapterConfiguration) {
				FileAdapterConfiguration config = (FileAdapterConfiguration) configuration;
				ImagePainter p = (ImagePainter) input;
				Graphics2D g = getGraphics(config.getWritePath(), p.getWidth(),
						p.getHeight());
				g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
						RenderingHints.VALUE_ANTIALIAS_ON);
				p.paint(g);
				flushGraphics(g, config.getWritePath());
				return null;
			}
			throw new DataAdapterException("Bad configuration");
		} else
			throw new DataAdapterException("Bad operation");
	}
}
