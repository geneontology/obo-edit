package org.oboedit.gui.components;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.filters.DefinitionDbxrefSearchCriterion;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.DelDbxrefHistoryItem;
import org.obo.history.HistoryItem;

import org.apache.log4j.*;

public class DefinitionDbxrefEditorComponent extends
	AbstractDbxrefEditorComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefinitionDbxrefEditorComponent.class);
	protected final static FieldPathSpec spec = new FieldPathSpec(
			DefinitionDbxrefSearchCriterion.CRITERION);

	public DefinitionDbxrefEditorComponent() {
	}
	
	@Override
	protected String getUserEventType() {
		return "gui.dbxref.def.add";
	}
	
	@Override
	protected String getDbxrefTitle() {
		return "Definition Dbxrefs";
	}

	@Override
	protected HistoryItem getAddDbxrefItem(Dbxref ref) {
		return new AddDbxrefHistoryItem(currentObject.getID(), ref, true, null);
	}

	@Override
	protected HistoryItem getDelDbxrefItem(Dbxref ref) {
		return new DelDbxrefHistoryItem(currentObject.getID(), ref, true, null);
	}
	
	@Override
	protected FieldPath getPath(IdentifiedObject io) {
		return FieldPathSpec.createQueryPath(spec, io);
	}

	@Override
	public FieldPathSpec getPathSpec() {
		return spec;
	}

	public void populateFields(IdentifiedObject io) {
		if (io instanceof DefinedObject) {
			((DefinedObject) io).getDefDbxrefs().clear();
			((DefinedObject) io).getDefDbxrefs().addAll(getEditedDbxrefs());
		}	
	}

	public String getID() {
		return "DEF_DBXREF_EDITOR";
	}

	@Override
	protected Dbxref createNewDbxref() {
		return new DbxrefImpl("XX", "<new dbxref>", Dbxref.DEFINITION);
	}

}
