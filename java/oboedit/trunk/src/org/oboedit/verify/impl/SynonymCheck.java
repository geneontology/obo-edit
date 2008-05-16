package org.oboedit.verify.impl;

import org.obo.datamodel.*;
import org.obo.filters.SynonymSearchCriterion;
import org.obo.filters.SynonymTextSearchCriterion;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.*;
import org.oboedit.verify.*;

import java.util.*;

import org.apache.log4j.*;

public class SynonymCheck extends AbstractTextCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymCheck.class);
	public static final FieldPathSpec PATH_SPEC = new FieldPathSpec(
			SynonymSearchCriterion.CRITERION,
			SynonymTextSearchCriterion.CRITERION);

	protected static Collection<FieldPathSpec> PATH_SPECS = new LinkedList<FieldPathSpec>();
	static {
		PATH_SPECS.add(PATH_SPEC);
	}

	public SynonymCheck() {
		setAllowNewlines(false);
		setAllowBlank(false);
		setSentenceStructureChecks(false);
		setAllowExtended(Preferences.getPreferences()
				.getAllowExtendedCharacters());

	}

	public String getID() {
		return "SYNONYM_CHECK";
	}

	@Override
	public String getDescription() {
		return "Synonym checks";
	}

	@Override
	public String getWarningLabel(IdentifiedObject io, byte condition, int index) {
		if (VerificationManager.isTextCommitCondition(condition))
			return "Synonym " + (index > 0 ? index + " " : "");
		else
			return "Synonym " + (index > 0 ? index + " " : "") + "of "
					+ "<a href='file:" + io.getID() + "'>" + io.getID()
					+ "</a>";
	}

	@Override
	public String getWarningLabel(FieldPath path, byte condition) {
		int index = -1;
		Synonym s = (Synonym) path.getParentPath().getLastValue();
		if (path.getObject() instanceof SynonymedObject) {
			List<Synonym> l = new LinkedList<Synonym>(((SynonymedObject) path
					.getObject()).getSynonyms());
			index = l.indexOf(s);
		}
		if (VerificationManager.isTextCommitCondition(condition))
			return "Synonym " + (index > 0 ? index + " " : "");
		else
			return "Synonym " + (index > 0 ? index + " " : "") + "of "
					+ "<a href='file:" + path.getObject().getID() + "'>"
					+ path.getObject().getID() + "</a>";

	}
	
	@Override
	public Collection<CheckWarning> check(OBOSession session, FieldPath path, byte condition, boolean checkObsoletes) {
		if (path.getLastValue() instanceof Synonym) {
			path = new FieldPath(path, SynonymTextSearchCriterion.CRITERION, null);
			path = path.resolve().iterator().next();
		}
		return super.check(session, path, condition, checkObsoletes);
	}

	@Override
	public Collection getStrings(IdentifiedObject io) {
		if (io instanceof SynonymedObject) {
			Collection out = new LinkedList();
			Iterator it = ((SynonymedObject) io).getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				out.add(s.getText());
			}
			return out;
		} else
			return Collections.EMPTY_LIST;
	}

	public Collection<FieldPathSpec> getPaths() {
		return PATH_SPECS;
	}
}
