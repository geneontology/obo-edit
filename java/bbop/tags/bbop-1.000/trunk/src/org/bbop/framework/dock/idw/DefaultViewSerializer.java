package org.bbop.framework.dock.idw;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;

import net.infonode.docking.View;
import net.infonode.docking.ViewSerializer;
import net.infonode.docking.util.StringViewMap;
import net.infonode.docking.util.ViewMap;

public class DefaultViewSerializer implements ViewSerializer {

	protected IDWDriver driver;

	protected StringViewMap viewMap;

	public DefaultViewSerializer(StringViewMap viewMap, IDWDriver driver) {
		this.driver = driver;
		this.viewMap = viewMap;
	}

	public View readView(ObjectInputStream in) throws IOException {
		String id = in.readUTF();
		View v = viewMap.getView(id);
		if (v == null) {
			String factoryID = ComponentManager.getFactoryID(id);
			GUIComponentFactory factory = ComponentManager.getManager()
					.getFactory(factoryID);
			if (factory != null) {
				v = driver.createView(factory, id, null);
				driver.addView(v);
			}
		}
		return v;
	}

	public void writeView(View view, ObjectOutputStream out) throws IOException {
		GUIComponent c = ((ComponentConfigCard) view.getComponent())
				.getComponent();
		out.writeUTF(c.getID());
	}
}
