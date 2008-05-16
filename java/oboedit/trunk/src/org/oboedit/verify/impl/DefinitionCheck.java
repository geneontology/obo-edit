package org.oboedit.verify.impl;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;
import org.obo.filters.DbxrefSearchCriterion;
import org.obo.filters.DefinitionDbxrefSearchCriterion;
import org.obo.filters.DefinitionSearchCriterion;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.HistoryItem;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.*;
import org.oboedit.verify.*;

import java.util.*;

import org.apache.log4j.*;

public class DefinitionCheck extends AbstractTextCheck implements FieldCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefinitionCheck.class);

	public static final FieldPathSpec TERM_PATH_SPEC = new FieldPathSpec();

	public static final FieldPathSpec DEF_PATH_SPEC = new FieldPathSpec(
			DefinitionSearchCriterion.CRITERION);

	public static final FieldPathSpec DEF_DBXREF_PATH_SPEC = new FieldPathSpec(
			DefinitionDbxrefSearchCriterion.CRITERION);

	protected static Collection<FieldPathSpec> PATH_SPECS = new LinkedList<FieldPathSpec>();

	static {
		PATH_SPECS.add(DEF_DBXREF_PATH_SPEC);
		PATH_SPECS.add(DEF_PATH_SPEC);
		PATH_SPECS.add(TERM_PATH_SPEC);
	}

	public DefinitionCheck() {
		setAllowNewlines(true);
		setAllowBlank(true);
		setSentenceStructureChecks(true);
		setAllowExtended(Preferences.getPreferences()
				.getAllowExtendedCharacters());

	}

	/*
	 * @Override protected void appendAdditionalWarnings(Collection out,
	 * OBOSession session, IdentifiedObject currentObject, byte condition) { if
	 * (currentObject instanceof DefinedObject) { DefinedObject dfo =
	 * (DefinedObject) currentObject; FieldPath defPath = new
	 * FieldPath(currentObject, DefinitionSearchCriterion.CRITERION,
	 * dfo.getDefinition()); if (dfo.getDefinition() == null ||
	 * dfo.getDefinition().length() == 0) { if (dfo.getDefDbxrefs().size() > 0) {
	 * out.add(new CheckWarning(getWarningLabel(currentObject, condition, 0) + "
	 * has definition references, " + "but no definition.", true, this, new
	 * FieldPath(currentObject, DefinitionDbxrefSearchCriterion.CRITERION,
	 * FieldPath.EMPTY))); } } if
	 * (Preferences.getPreferences().getUsePersonalDefinition()) { if
	 * (dfo.getDefinition().equals(
	 * Preferences.getPreferences().getPersonalDefinition())) out .add(new
	 * TextCheckWarning(getWarningLabel( currentObject, condition, 0) + " seems
	 * to have an " + "auto-generated definition " + "that was never edited.",
	 * false, this, 0, dfo.getDefinition().length(), defPath, "def:auto_text")); }
	 * if (dfo.getDefDbxrefs().size() == 0) { if (dfo.getDefinition() != null &&
	 * dfo.getDefinition().length() > 0) out.add(new
	 * CheckWarning(getWarningLabel(currentObject, condition, 0) + " has a
	 * definition with " + "no references.", true, this, defPath)); } } }
	 */
	public String getID() {
		return "DEFINITION_CHECK";
	}

	@Override
	public String getDescription() {
		return "Definition checks";
	}

	@Override
	public HistoryItem getFieldChangeHistoryItem(
			IdentifiedObject currentObject, String newText) {
		return new DefinitionChangeHistoryItem((DefinedObject) currentObject,
				newText);
	}

	@Override
	public String getWarningLabel(IdentifiedObject io, byte condition, int index) {
		if (VerificationManager.isTextCommitCondition(condition))
			return "Definition";
		else
			return "Definition of <a href='file:" + io.getID() + "'>"
					+ io.getID() + "</a>";
	}

	@Override
	protected String getWarningLabel(FieldPath path, byte condition) {
		if (VerificationManager.isTextCommitCondition(condition))
			return "Definition";
		else
			return "Definition of <a href='file:" + path.getObject().getID()
					+ "'>" + path.getObject().getID() + "</a>";
	}

	@Override
	public Collection getStrings(IdentifiedObject io) {
		if (io instanceof DefinedObject) {
			Collection out = new LinkedList();
			out.add(((DefinedObject) io).getDefinition());
			return out;
		} else
			return Collections.EMPTY_LIST;
	}

	protected void appendAdditionalWarnings(Collection out, OBOSession session,
			FieldPath path, byte condition) {

		IdentifiedObject currentObject = path.getObject();
		DefinedObject dfo = (DefinedObject) currentObject;
		String def = dfo.getDefinition();
		if (path.matchesSpec(DEF_PATH_SPEC)) {
			if (Preferences.getPreferences().getUsePersonalDefinition()) {
				if (def.equals(Preferences.getPreferences()
						.getPersonalDefinition()))
					out.add(new TextCheckWarning(getWarningLabel(currentObject,
							condition, 0)
							+ " seems to have an "
							+ "auto-generated definition "
							+ "that was never edited.", false, this, 0, dfo
							.getDefinition().length(), path, "def:auto_text"));
			}
			if ((def == null || def.length() == 0)
					&& dfo.getDefDbxrefs().size() > 0) {
				out.add(new CheckWarning(
						getWarningLabel(currentObject, condition, 0)
								+ " has definition references, "
								+ "but no definition.", true, this, path));
			}
		}
		if (path.matchesSpec(DEF_DBXREF_PATH_SPEC)) {
			if (dfo.getDefDbxrefs().size() == 0) {
				if (def != null && def.length() > 0)
					out.add(new CheckWarning(getWarningLabel(currentObject,
							condition, 0)
							+ " has a definition with " + "no references.",
							true, this, path));
			}
		}
	}

	public Collection<FieldPathSpec> getPaths() {
		return PATH_SPECS;
	}
}
