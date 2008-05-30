package org.oboedit.gui.components.imageplugin.util;

import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.OnTheFlyReasoner;
import org.obo.util.ReasonerUtil;
import org.oboedit.controller.SessionManager;

public class ImageUtil {
	private ImageUtil() {
	}

	public static String getURL(LinkedObject lo) {
		String s = ImageUtil.getImmediateFile(lo.getID());
		if (s != null)
			return s;
		else {
			OBOProperty partof = (OBOProperty) SessionManager.getManager()
					.getSession().getObject("part_of");
			if (partof == null)
				return null;
			ReasonedLinkDatabase reasoner;
			if (SessionManager.getManager().getUseReasoner())
				reasoner = SessionManager.getManager().getReasoner();
			else
				reasoner = new OnTheFlyReasoner(SessionManager.getManager()
						.getCurrentLinkDatabase());
			LinkedObject settleForMe = ImageUtil.getBestObject(reasoner,
					partof, lo);
			if (settleForMe != null)
				return ImageUtil.getImmediateFile(settleForMe.getID());

		}
		return null;
	}

	public static String getImmediateFile(String id) {
		File f = new File(GUIManager.getManager().getPrefsDir(), "images/"
				+ id.replace(":", "_") + ".jpg");
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
		if (reasoner == null)
			return null;
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

}
