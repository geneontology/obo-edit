package org.oboedit.gui.components;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.filters.GeneralDbxrefSearchCriterion;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.DelDbxrefHistoryItem;
import org.obo.history.HistoryItem;

import org.apache.log4j.*;

public class GeneralDbxrefEditorComponent extends AbstractDbxrefEditorComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GeneralDbxrefEditorComponent.class);

	protected final static FieldPathSpec spec = new FieldPathSpec(
			GeneralDbxrefSearchCriterion.CRITERION);

	@Override
	protected String getDbxrefTitle() {
		return "General Dbxrefs";
	}
	
	@Override
	protected String getUserEventType() {
		return "gui.dbxref.general.add";
	}

	@Override
	protected HistoryItem getAddDbxrefItem(Dbxref ref) {
		return new AddDbxrefHistoryItem(currentObject.getID(), ref, false, null);
	}

	@Override
	protected HistoryItem getDelDbxrefItem(Dbxref ref) {
		return new DelDbxrefHistoryItem(currentObject.getID(), ref, false, null);
	}

	@Override
	protected FieldPath getPath(IdentifiedObject io) {
		return FieldPathSpec.createQueryPath(spec, io);
	}

	@Override
	public FieldPathSpec getPathSpec() {
		return spec;
	}

	public String getID() {
		return "DBXREF_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		if (io instanceof DbxrefedObject) {
			((DbxrefedObject) io).getDbxrefs().clear();
			((DbxrefedObject) io).getDbxrefs().addAll(getEditedDbxrefs());
		}		
	}

	@Override
	protected Dbxref createNewDbxref() {
		return new DbxrefImpl("XX", "<new dbxref>");
	}

}
