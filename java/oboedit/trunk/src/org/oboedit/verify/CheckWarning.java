package org.oboedit.verify;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import javax.swing.Action;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class CheckWarning {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CheckWarning.class);

	protected Check source;
	protected String message;
	protected boolean isFatal;
	protected Collection<QuickFix> fixes;
	protected FieldPath path;
	protected String type;

	public CheckWarning(String message, boolean isFatal) {
		this(message, isFatal, null, (IdentifiedObject) null,
				new LinkedList<QuickFix>());
	}

	public CheckWarning(String message, boolean isFatal, IdentifiedObject object) {
		this(message, isFatal, null, object, new LinkedList<QuickFix>());
	}

	public CheckWarning(String message, boolean isFatal, Check source,
			FieldPath field) {
		this(message, isFatal, source, field, null, new LinkedList<QuickFix>());
	}

	public CheckWarning(String message, boolean isFatal, Check source,
			FieldPath field, Collection<QuickFix> fixes) {
		this(message, isFatal, source, field, null, fixes);
	}

	public CheckWarning(String message, boolean isFatal, Check source,
			IdentifiedObject object) {
		this(message, isFatal, source, object, new LinkedList<QuickFix>());
	}

	public CheckWarning(String message, boolean isFatal, Check source,
			IdentifiedObject object, Collection<QuickFix> fixes) {
		this(message, isFatal, source, new FieldPath(object), null, fixes);
	}

	public CheckWarning(String message, boolean isFatal, Check source,
			FieldPath path, String type, Collection<QuickFix> fixes) {
		this.source = source;
		this.message = message;
		this.isFatal = isFatal;
		this.fixes = fixes;
		this.path = path;
		this.type = type;
		Iterator<QuickFix> it = fixes.iterator();
		while (it.hasNext()) {
			QuickFix action = it.next();
			action.setWarning(this);
		}
		if (type == null)
			this.type = "problem";
	}

	@Override
	public int hashCode() {
		return source.hashCode() + message.hashCode() + (isFatal ? 1 : 0)
				+ path.hashCode() + type.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof CheckWarning) {
			CheckWarning cw = (CheckWarning) obj;
			return ObjectUtil.equals(source, cw.getSource())
					&& ObjectUtil.equals(message, cw.getMessage())
					&& ObjectUtil.equals(isFatal, cw.isFatal())
					&& ObjectUtil.equals(path, cw.getPath())
					&& ObjectUtil.equals(type, cw.getType());
		} else
			return false;
	}

	/**
	 * Returns a list of zero or more {@link javax.swing.Action} objects that
	 * can be used to fix the current warning. The actions are displayed in the
	 * status report when the user clicks the "quick fix" button. If a quick-fix
	 * was successful, it should set the "SUCCESS" property of the action to
	 * Boolean.TRUE. Warnings that were successfully fixed will be removed from
	 * the list.
	 * 
	 * @return a collection of fixes
	 */
	public Collection<QuickFix> getFixes() {
		return fixes;
	}

	public IdentifiedObject getObject() {
		return path.getObject();
	}

	public FieldPath getPath() {
		return path;
	}

	public Check getSource() {
		return source;
	}

	public String getMessage() {
		return message;
	}

	public boolean isFatal() {
		return isFatal;
	}

	@Override
	public String toString() {
		return "CheckWarning" + (isFatal ? " (fatal)" : "") + ": " + message;
	}

	public String getType() {
		return type;
	}
}
