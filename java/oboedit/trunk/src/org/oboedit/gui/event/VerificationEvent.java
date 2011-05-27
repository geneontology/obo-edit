package org.oboedit.gui.event;

import java.util.*;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.oboedit.verify.CheckTask;
import org.oboedit.verify.CheckWarning;

import org.apache.log4j.*;

public class VerificationEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(VerificationEvent.class);

	private static final long serialVersionUID = 1L;
	protected byte condition;
	protected Collection<CheckWarning> warnings;
	protected OBOSession session;
	protected FieldPath path;
	protected CheckTask checkTask;

	public VerificationEvent(Object source, CheckTask checkTask,
			Collection<CheckWarning> warnings, OBOSession session,
			FieldPath path, byte condition) {
		super(source);
		this.checkTask = checkTask;
		this.condition = condition;
		this.warnings = warnings;
		this.session = session;
		this.path = path;
	}
	
	public FieldPath getPath() {
		return path;
	}
	
	public IdentifiedObject getCurrentObject() {
		if (path == null)
			return null;
		else
			return path.getObject();
	}
	
	public OBOSession getSession() {
		return session;
	}

	public byte getCondition() {
		return condition;
	}

	public Collection<CheckWarning> getWarnings() {
		return warnings;
	}
}
