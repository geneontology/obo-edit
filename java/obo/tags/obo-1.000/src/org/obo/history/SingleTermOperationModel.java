package org.obo.history;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.datamodel.impl.SingleTermSession;

public class SingleTermOperationModel extends DefaultOperationModel {

	protected IdentifiedObject term;

	public SingleTermOperationModel(OBOSession session, IdentifiedObject term) {
		this.term = term;
		super.setSession(new SingleTermSession(session, term));
	}
	
	public IdentifiedObject getTerm() {
		return term;
	}

	@Override
	public void setSession(OBOSession session) {
		throw new UnsupportedOperationException(
				"Cannot set the session of a SingleTermOperationModel; that is done automatically on instantiation");
		
	}
}
