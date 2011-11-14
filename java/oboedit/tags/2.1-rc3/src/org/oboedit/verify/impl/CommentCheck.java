package org.oboedit.verify.impl;

import org.obo.datamodel.*;
import org.obo.filters.CommentSearchCriterion;
import org.obo.history.CommentChangeHistoryItem;
import org.obo.history.HistoryItem;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.*;

import java.util.*;

import org.apache.log4j.*;

public class CommentCheck extends AbstractTextCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CommentCheck.class);

	public static final FieldPathSpec PATH_SPEC = new FieldPathSpec(
			CommentSearchCriterion.CRITERION);

	protected static Collection<FieldPathSpec> PATH_SPECS = Collections
			.singletonList(PATH_SPEC);

	public CommentCheck() {
		setAllowNewlines(true);
		setAllowBlank(true);
		setSentenceStructureChecks(true);
		setAllowExtended(Preferences.getPreferences()
				.getAllowExtendedCharacters());

	}

	public String getID() {
		return "COMMENT_CHECK";
	}

	@Override
	public String getDescription() {
		return "Comment checks";
	}

	@Override
	public String getWarningLabel(IdentifiedObject io, byte condition, int index) {
		if (VerificationManager.isTextCommitCondition(condition))
			return "Comment";
		else
			return "Comment of <a href='file:" + io.getID() + "'>" + io.getID()
					+ "</a>";
	}

	@Override
	protected String getWarningLabel(FieldPath path, byte condition) {
		if (VerificationManager.isTextCommitCondition(condition))
			return "Comment";
		else
			return "Comment of <a href='file:" + path.getObject().getID()
					+ "'>" + path.getObject().getID() + "</a>";
	}

	@Override
	public HistoryItem getFieldChangeHistoryItem(
			IdentifiedObject currentObject, String newText) {
		return new CommentChangeHistoryItem((CommentedObject) currentObject,
				newText);
	}

	@Override
	public Collection<String> getStrings(IdentifiedObject io) {
		if (io instanceof CommentedObject) {
			Collection<String> out = new LinkedList<String>();
			out.add(((CommentedObject) io).getComment());
			return out;
		} else
			return Collections.emptyList();
	}

	public Collection<FieldPathSpec> getPaths() {
		return PATH_SPECS;
	}
}
