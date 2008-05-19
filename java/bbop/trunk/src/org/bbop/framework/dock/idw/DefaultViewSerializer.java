package org.bbop.framework.dock.idw;

import java.awt.Component;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javax.swing.Icon;
import javax.swing.JLabel;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;

import net.infonode.docking.View;
import net.infonode.docking.ViewSerializer;
import net.infonode.docking.util.StringViewMap;
import net.infonode.docking.util.ViewMap;

import org.apache.log4j.*;

public class DefaultViewSerializer implements ViewSerializer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultViewSerializer.class);

	protected IDWDriver driver;

	protected StringViewMap viewMap;

	public static class GarbageView extends View {
		private static int idgen = 0;

		public GarbageView() {
			super("Garbage" + idgen, null, new ComponentConfigCard(
					new AbstractGUIComponent("dummy:"+idgen) {

					}));
			idgen++;
		}

	}

	public DefaultViewSerializer(StringViewMap viewMap, IDWDriver driver) {
		this.driver = driver;
		this.viewMap = viewMap;
	}

	public View readView(ObjectInputStream in) throws IOException {
		boolean initialLoad = driver.getCurrentPerspective() == null;
		String id = in.readUTF();
		View v = viewMap.getView(id);
		if (v == null) {
			String factoryID = ComponentManager.getFactoryID(id);
			GUIComponentFactory factory = ComponentManager.getManager()
					.getFactory(factoryID);
			if (factory != null
					&& (!initialLoad || factory.isRestoreOnStartup())) {
				v = driver.createView(factory, id, null);

			} else
				// v = null;
				v = new GarbageView();
			if (v != null)
				driver.addView(v);
		} else
			driver.fireCreatedView(v, driver.getComponent(v));
		return v;
	}

	public void writeView(View view, ObjectOutputStream out) throws IOException {
		GUIComponent c = ((ComponentConfigCard) view.getComponent())
				.getComponent();
		out.writeUTF(c.getID());
	}
}
