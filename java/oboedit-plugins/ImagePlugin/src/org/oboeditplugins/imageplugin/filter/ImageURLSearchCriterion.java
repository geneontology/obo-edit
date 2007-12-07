package org.oboeditplugins.imageplugin.filter;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Collection;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.filters.AbstractStringCriterion;
import org.oboeditplugins.imageplugin.util.ImageUtil;

public class ImageURLSearchCriterion extends
		AbstractStringCriterion<IdentifiedObject> {

	public ImageURLSearchCriterion() {
	}

	public String getID() {
		return "image_url";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	public Collection<String> getValues(Collection<String> scratch,
			IdentifiedObject obj) {
		if (obj instanceof LinkedObject) {
			String url = ImageUtil.getURL((LinkedObject) obj);
			if (url != null)
				scratch.add(url);
		}
		return scratch;
	}
	
	@Override
	public String toString() {
		return "Image URL";
	}

}
