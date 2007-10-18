package org.oboedit.verify.impl;

import org.obo.datamodel.*;
import org.obo.filters.CommentSearchCriterion;
import org.obo.filters.NameSearchCriterion;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.*;
import org.oboedit.verify.*;

import java.util.*;

public class NameCheck extends AbstractTextCheck {
	
	public static final FieldPathSpec PATH_SPEC = new FieldPathSpec(
			NameSearchCriterion.CRITERION);

	protected static Collection<FieldPathSpec> PATH_SPECS = Collections
			.singletonList(PATH_SPEC);
	
	public NameCheck() {
		setAllowNewlines(false);
		setAllowBlank(false);
		setSentenceStructureChecks(false);
		setAllowExtended(Preferences.getPreferences()
				.getAllowExtendedCharacters());

	}

	public String getID() {
		return "NAME_CHECK";
	}

	@Override
	public String getDescription() {
		return "Name checks";
	}

	@Override
	public String getWarningLabel(IdentifiedObject io, byte condition, int index) {
		if (VerificationManager.isTextCommitCondition(condition))
			return "Name";
		else
			return "Name of <a href='file:" + io.getID() + "'>"
					+ io.getID() + "</a>";
	}

	@Override
	protected String getWarningLabel(FieldPath path, byte condition) {
		if (VerificationManager.isTextCommitCondition(condition))
			return "Name";
		else
			return "Name of <a href='file:" + path.getObject().getID() + "'>"
					+ path.getObject().getID() + "</a>";
	}

	@Override
	public Collection getStrings(IdentifiedObject io) {
		Collection out = new LinkedList();
		out.add(io.getName());
		return out;
	}


	public Collection<FieldPathSpec> getPaths() {
		return PATH_SPECS;
	}
}
