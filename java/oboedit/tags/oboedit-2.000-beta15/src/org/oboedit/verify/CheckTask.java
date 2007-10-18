package org.oboedit.verify;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.SwingUtilities;

import org.bbop.util.AbstractTaskDelegate;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiableObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.VerificationEvent;
import org.oboedit.gui.event.VerificationListener;

public class CheckTask extends AbstractTaskDelegate<Collection<CheckWarning>> {

	protected boolean checkObsoletes;

	protected int checkIndex;

	protected Check currentCheck;
	protected int oindex;

	protected OBOSession session;
	protected Collection<Check> liveChecks;
	protected FieldPath path;
	protected byte condition;

	protected LinkedList<VerificationListener> verificationListeners = new LinkedList<VerificationListener>();

	public CheckTask(boolean checkObsoletes, Collection<Check> liveChecks,
			OBOSession session, FieldPath path, byte condition) {
		this.checkObsoletes = checkObsoletes;
		this.liveChecks = liveChecks;
		this.session = session;
		this.condition = condition;
		this.path = path;
		setSwingFriendly(true);
	}

	public boolean getCheckObsoletes() {
		return checkObsoletes;
	}

	public void helpRunCheck(FieldCheck fcheck, IdentifiedObject io,
			byte condition, Collection<CheckWarning> out) {
		if (io.isBuiltIn())
			return;
		if (!getCheckObsoletes() && TermUtil.isObsolete(io))
			return;
		Collection<FieldPathSpec> specs = fcheck.getPaths();
		List<FieldPath> paths = new LinkedList<FieldPath>();
		for (FieldPathSpec spec : specs) {
			Collection<FieldPath> qpaths = spec.createQueryPath(io).resolve();
			paths.addAll(qpaths);
			/*
			 * for (FieldPath qpath : qpaths) { paths.addAll(qpaths); }
			 */
		}
		FieldPath.coalesce(paths);
		for (FieldPath qpath : paths) {
			out.addAll(fcheck.check(session, qpath, condition,
					getCheckObsoletes()));
		}
	}

	public Collection doRunCheck(FieldPath path, byte condition) {
		oindex = 0;
		Collection<CheckWarning> out = new LinkedList<CheckWarning>();
		if (currentCheck.needsReasoner())
			currentCheck.setReasoner(SessionManager.getManager().getReasoner());
		IdentifiedObject currentObject = path.getObject();

		if (currentCheck instanceof FieldCheck) {
			FieldCheck fcheck = (FieldCheck) currentCheck;
			if (path != null && path.getLength() > 0) {
				boolean matches = false;
				for (FieldPathSpec spec : fcheck.getPaths()) {
					if (path.matchesSpec(spec)) {
						matches = true;
						break;
					}
				}
				if (matches)
					out.addAll(fcheck.check(session, path, condition,
							getCheckObsoletes()));
			} else if (path.getLength() == 0 && path.getObject() != null) {
				helpRunCheck(fcheck, path.getObject(), condition, out);
			} else {
				Iterator it2 = session.getObjects().iterator();
				for (oindex = 0; it2.hasNext(); oindex++) {
					if (cancelled)
						break;
					IdentifiableObject o = (IdentifiableObject) it2.next();
					if (!(o instanceof IdentifiedObject))
						continue;
					IdentifiedObject io = (IdentifiedObject) o;
					helpRunCheck(fcheck, io, condition, out);
				}
			}
		} else if (currentCheck instanceof ObjectCheck) {
			if (currentObject != null) {
				ObjectCheck ocheck = (ObjectCheck) currentCheck;
				Iterator it2 = session.getObjects().iterator();
				for (oindex = 0; it2.hasNext(); oindex++) {
					if (cancelled)
						break;
					IdentifiableObject o = (IdentifiableObject) it2.next();
					if (!(o instanceof IdentifiedObject))
						continue;
					IdentifiedObject io = (IdentifiedObject) o;
					if (io.isBuiltIn())
						continue;
					if (!getCheckObsoletes() && TermUtil.isObsolete(io))
						continue;

					out.addAll(ocheck.check(session, io, condition,
							getCheckObsoletes()));
				}
			} else {
				Iterator it2 = session.getObjects().iterator();
				for (oindex = 0; it2.hasNext(); oindex++) {
					if (cancelled)
						break;
					IdentifiableObject o = (IdentifiableObject) it2.next();
					if (!(o instanceof IdentifiedObject))
						continue;
					IdentifiedObject io = (IdentifiedObject) o;
					if (io.isBuiltIn())
						continue;
					if (!getCheckObsoletes() && TermUtil.isObsolete(io))
						continue;

					out.addAll(((ObjectCheck) currentCheck).check(session, io,
							condition, getCheckObsoletes()));
				}
			}
		} else if (currentCheck instanceof OntologyCheck) {
			out.addAll(((OntologyCheck) currentCheck).check(session,
					currentObject, condition, getCheckObsoletes()));
		}
		return out;
	}

	public Collection<CheckWarning> runChecks() {
		fireVerificationStartingEvent(new VerificationEvent(this, this, null,
				session, path, condition));
		Collection out = new LinkedList();
		if (liveChecks.size() == 0) {
			fireVerificationCompleteEvent(new VerificationEvent(this, this,
					out, session, path, condition));
			return out;
		}
		Iterator<Check> it = liveChecks.iterator();
		for (checkIndex = 0; it.hasNext(); checkIndex++) {
			currentCheck = it.next();
			if (cancelled)
				break;
			out.addAll(doRunCheck(path, condition));
		}
		fireVerificationCompleteEvent(new VerificationEvent(this, this, out,
				session, path, condition));
		return out;
	}

	public void addVerificationListener(VerificationListener listener) {
		verificationListeners.add(listener);
	}

	public void removeVerificationListener(VerificationListener listener) {
		verificationListeners.remove(listener);
	}

	@Override
	public Number getProgressValue() {
		if (currentCheck == null)
			return -1;
		if (currentCheck instanceof ObjectCheck
				|| currentCheck instanceof FieldCheck) {
			int percentage = 100 * checkIndex / liveChecks.size();
			int newVal = percentage
					+ ((100 * oindex / liveChecks.size()) / session
							.getObjects().size());
			return newVal;
		} else if (currentCheck instanceof OntologyCheck) {
			int percentage = 100 * checkIndex / liveChecks.size();
			Number n = currentCheck.getProgressValue();
			int newVal = percentage + (n == null ? 0 : n.intValue());
			return newVal;
		} else
			return 100 * checkIndex / liveChecks.size();
	}

	@Override
	public String getProgressString() {
		if (currentCheck == null)
			return null;
		String note = currentCheck.getProgressString();
		if (note == null)
			note = "";
		if (currentCheck instanceof ObjectCheck
				|| currentCheck instanceof FieldCheck) {
			return "<html>Running " + currentCheck.getDescription() + " ("
					+ (checkIndex + 1) + " of " + liveChecks.size() + ") "
					+ "; checking object " + oindex + " of "
					+ session.getObjects().size()+"<br><i>"+note+"</i></html>";
		} else
			return "<html>Running " + currentCheck.getDescription() + " ("
					+ (checkIndex + 1) + " of " + liveChecks.size() + ")<br><i>"+note+"</i></html>";
	}

	protected void fireVerificationStartingEvent(final VerificationEvent ve) {
		Iterator it = verificationListeners.iterator();
		while (it.hasNext()) {
			final VerificationListener listener = (VerificationListener) it
					.next();
			Runnable r = new Runnable() {
				public void run() {
					listener.verificationStarting(ve);
				}
			};
			SwingUtilities.invokeLater(r);
		}
	}

	protected void fireVerificationCompleteEvent(final VerificationEvent ve) {
		Iterator it = verificationListeners.iterator();
		while (it.hasNext()) {
			final VerificationListener listener = (VerificationListener) it
					.next();
			Runnable r = new Runnable() {
				public void run() {
					listener.verificationComplete(ve);
				}
			};
			SwingUtilities.invokeLater(r);
		}
	}

	@Override
	public void execute() {
		setResults(runChecks());
	}

}
