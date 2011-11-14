package org.oboedit.gui.components.imageplugin.filter;

import java.util.Collection;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.filters.AbstractStringCriterion;
import org.oboedit.gui.components.imageplugin.util.ImageUtil;

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
