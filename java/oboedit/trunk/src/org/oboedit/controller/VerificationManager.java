package org.oboedit.controller;

import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;

import org.bbop.framework.GUIManager;
import org.bbop.io.IOUtil;
import org.obo.datamodel.*;
import org.oboedit.gui.*;
import org.oboedit.gui.event.VerificationListener;
import org.oboedit.verify.Check;
import org.oboedit.verify.CheckTask;
import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.impl.CommentCheck;
import org.oboedit.verify.impl.CycleCheck;
import org.oboedit.verify.impl.DanglingIntersectionCheck;
import org.oboedit.verify.impl.DbxrefCheck;
import org.oboedit.verify.impl.DefinitionCheck;
import org.oboedit.verify.impl.DisjointednessCheck;
import org.oboedit.verify.impl.IDCheck;
import org.oboedit.verify.impl.NameCheck;
import org.oboedit.verify.impl.NameRedundancyCheck;
import org.oboedit.verify.impl.SynonymCheck;

public class VerificationManager {
	public static final byte TEXT_EDIT_COMMIT = 1;

	public static final byte REASONER_ACTIVATED = 2;

	public static final byte SAVE = 4;

	public static final byte MANUAL = 8;

	public static final byte LOAD = 16;

	public static final byte TEXT_EDIT_THREAD = 32;

	public static final byte ALL = TEXT_EDIT_THREAD ^ TEXT_EDIT_COMMIT
			^ REASONER_ACTIVATED ^ LOAD ^ SAVE ^ MANUAL;

	public static final int MAX_WARNINGS = 100;

	protected static VerificationManager manager;

	protected Collection<VerificationListener> verificationListeners = new LinkedList<VerificationListener>();

	public static class VerificationConfiguration {
		protected byte warningConditions = ALL;

		protected boolean checkObsoletes = false;

		protected Collection checks = new LinkedList();

		public VerificationConfiguration() {
		}

		public void setCheckObsoletes(boolean checkObsoletes) {
			this.checkObsoletes = checkObsoletes;
		}

		public boolean getCheckObsoletes() {
			return checkObsoletes;
		}

		public byte getWarningConditions() {
			return warningConditions;
		}

		public void setWarningConditions(byte warningConditions) {
			this.warningConditions = warningConditions;
		}

		public Collection getChecks() {
			return checks;
		}

		public void setChecks(Collection checks) {
			this.checks.clear();
			Iterator it = checks.iterator();
			while (it.hasNext()) {
				Check check = (Check) it.next();
				addCheck(check);
			}
		}

		public void addCheck(Check check) {
			if (check != null)
				checks.add(check);
		}

		public void removeCheck(Check check) {
			checks.remove(check);
		}
	}

	protected VerificationConfiguration configuration = new VerificationConfiguration();

	public VerificationConfiguration getConfiguration() {
		return configuration;
	}

	public VerificationManager() {
		installDefaultChecks();
		GUIManager.addShutdownHook(new Runnable() {
			public void run() {
				try {
					writeSettings();
				} catch (IOException ex) {
					System.err
							.println("Could not write verification settings!");
					ex.printStackTrace();
				}
			}
		});
	}

	public static VerificationManager getManager() {
		if (manager == null)
			manager = new VerificationManager();
		return manager;
	}

	protected static Collection<Check> getDefaultChecks() {
		Collection<Check> out = new LinkedList<Check>();
		out.add(new CommentCheck());
		out.add(new DefinitionCheck());
		out.add(new DbxrefCheck());
		out.add(new SynonymCheck());
		out.add(new NameCheck());
		out.add(new NameRedundancyCheck());
		out.add(new DanglingIntersectionCheck());
		out.add(new CycleCheck());
		out.add(new DisjointednessCheck());
		out.add(new IDCheck());
		return out;
	}

	protected void installDefaultChecks() {
		VerificationConfiguration config = null;
		File profileFile = new File(GUIManager.getPrefsDir(), "verify.xml");
		if (profileFile.exists()) {

			String settings = IOUtil.readFile(profileFile.getAbsolutePath());
			settings = settings.replace(
					"org.geneontology.oboedit.verify.VerificationEngine",
					"org.oboedit.controller.VerificationManager");
			XMLDecoder decoder = new XMLDecoder(new ByteArrayInputStream(
					settings.getBytes()));
			// throw away list object
			try {
				config = (VerificationConfiguration) decoder.readObject();
				decoder.close();
			} catch (Throwable t) {
				// ignore it and restore default configuration
			}

		}
		if (config == null) {
			config = getConfiguration();
		}
		Iterator it = getDefaultChecks().iterator();
		while (it.hasNext()) {
			Check check = (Check) it.next();
			boolean found = false;
			Iterator it2 = config.getChecks().iterator();
			while (it2.hasNext()) {
				Check check2 = (Check) it2.next();
				if (check2 == null)
					continue;
				if (check.getClass().equals(check2.getClass())) {
					found = true;
					break;
				}
			}
			if (!found)
				config.getChecks().add(check);
		}
		// make sure there aren't any nulls hanging around
		it = config.getChecks().iterator();
		while (it.hasNext()) {
			Check check = (Check) it.next();
			if (check == null) {
				it.remove();
			}
		}

		setConfiguration(config);
	}

	public void writeSettings() throws IOException {
		File profileFile = new File(GUIManager.getPrefsDir(), "verify.xml");

		XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(
				new FileOutputStream(profileFile)));
		encoder.writeObject(getConfiguration());
		encoder.close();
	}

	public void setConfiguration(VerificationConfiguration configuration) {
		this.configuration = configuration;
	}

	public void installCheck(Check check) {
		configuration.addCheck(check);
	}

	public void removeCheck(Check check) {
		configuration.removeCheck(check);
	}

	public byte getWarningConditions() {
		// return warningConditions;
		return configuration.getWarningConditions();
	}

	public void setWarningConditions(byte warningConditions) {
		configuration.setWarningConditions(warningConditions);
	}

	public void setCheckObsoletes(boolean checkObsoletes) {
		configuration.setCheckObsoletes(checkObsoletes);
	}

	public boolean getCheckObsoletes() {
		return configuration.getCheckObsoletes();
	}

	public Collection getChecks() {
		return configuration.getChecks();
	}

	public static void setConditionAtField(Check check, byte field, boolean b) {
		boolean b1 = getConditionAtField(check, field);
		if (b1 != b) {
			byte condition = check.getConfiguration().getCondition();
			condition = (byte) (condition ^ field);
			check.getConfiguration().setCondition(condition);
		}
	}

	public static boolean getConditionAtField(Check check, byte field) {
		return getConditionAtField(check.getConfiguration().getCondition(),
				field);
	}

	public static boolean getConditionAtField(byte condition, byte field) {
		return (condition & field) > 0;
	}

	public static boolean isReasonerActivatedCondition(Check check) {
		return getConditionAtField(check, REASONER_ACTIVATED);
	}

	public static boolean isSaveCondition(Check check) {
		return getConditionAtField(check, SAVE);
	}

	public static boolean isLoadCondition(Check check) {
		return getConditionAtField(check, LOAD);
	}

	public static boolean isManualCondition(Check check) {
		return getConditionAtField(check, MANUAL);
	}

	public static boolean isTextThreadCondition(Check check) {
		return getConditionAtField(check, TEXT_EDIT_THREAD);
	}

	public static boolean isTextThreadCondition(byte condition) {
		return getConditionAtField(condition, TEXT_EDIT_THREAD);
	}

	public static boolean isTextCommitCondition(Check check) {
		return getConditionAtField(check, TEXT_EDIT_COMMIT);
	}

	public static boolean isTextCommitCondition(byte condition) {
		return getConditionAtField(condition, TEXT_EDIT_COMMIT);
	}

	public static boolean isReasonerActivatedCondition(byte condition) {
		return getConditionAtField(condition, REASONER_ACTIVATED);
	}

	public static boolean isSaveCondition(byte condition) {
		return getConditionAtField(condition, SAVE);
	}

	public static boolean isManualCondition(byte condition) {
		return getConditionAtField(condition, MANUAL);
	}

	protected boolean shouldRun(Check check, byte condition) {
		boolean reasonerCondition = !check.needsReasoner()
				|| (check.needsReasoner() && SessionManager.getManager()
						.getUseReasoner());
		int configCondition = check.getConfiguration().getCondition();
		int bytes = (configCondition & condition);
		return reasonerCondition && bytes > 0;
	}

	public static String getMessage(Collection warnings, String prefixText,
			String suffixText, boolean useHTML) {

		boolean isFatal = isFatal(warnings);

		StringBuffer out = new StringBuffer();
		if (useHTML)
			out.append("<html><body>");
		if (prefixText != null) {
			if (useHTML)
				out.append("<center>");
			out.append(prefixText + "\n");
			if (useHTML)
				out.append("</center>");
		}
		Iterator it = warnings.iterator();
		for (int i = 1; it.hasNext(); i++) {
			CheckWarning warning = (CheckWarning) it.next();
			if (warning.isFatal() != isFatal)
				continue;

			if (useHTML)
				out.append("<ul>");
			if (useHTML)
				out.append("<li> ");
			else
				out.append(i + ") ");
			out.append(warning.getMessage());
			if (useHTML)
				out.append("</ul>");
		}
		if (suffixText != null) {
			if (useHTML)
				out.append("<center>");
			out.append(suffixText + "\n");
			if (useHTML)
				out.append("</center>");
		}
		if (useHTML)
			out.append("</body></html>");
		return out.toString();
	}

	public static boolean isFatal(Collection warnings) {
		Iterator it = warnings.iterator();
		while (it.hasNext()) {
			CheckWarning warning = (CheckWarning) it.next();
			if (warning.isFatal())
				return true;
		}
		return false;
	}

	public static int countFatal(Collection warnings) {
		int count = 0;
		Iterator it = warnings.iterator();
		while (it.hasNext()) {
			CheckWarning warning = (CheckWarning) it.next();
			if (warning.isFatal())
				count++;
		}
		return count;
	}

	public Collection runCheck(Check check, OBOSession session,
			IdentifiedObject currentObject, byte condition) {
		return runChecks(Collections.singleton(check), session, new FieldPath(
				currentObject), condition);
	}

	public Collection runChecks(OBOSession session, IdentifiedObject io,
			byte condition) {
		return runChecks(session, new FieldPath(io), condition);
	}

	public Collection<CheckWarning> runChecks(OBOSession session,
			FieldPath path, byte condition) {
		List<Check> liveChecks = getLiveChecks(condition);
		return runChecks(liveChecks, session, path, condition);
	}

	protected List<Check> getLiveChecks(byte condition) {
		List<Check> liveChecks = new LinkedList<Check>();
		Iterator<Check> it = configuration.getChecks().iterator();
		while (it.hasNext()) {
			Check check = it.next();
			if (shouldRun(check, condition))
				liveChecks.add(check);
		}
		return liveChecks;
	}

	public CheckTask getCheckTask(OBOSession session, IdentifiedObject io,
			byte condition) {
		return getCheckTask(session, new FieldPath(io), condition);
	}

	public CheckTask getCheckTask(OBOSession session, FieldPath path,
			byte condition) {
		List<Check> liveChecks = getLiveChecks(condition);
		return getCheckTask(getCheckObsoletes(), liveChecks, session, path,
				condition);
	}

	public CheckTask getCheckTask(Collection<Check> liveChecks,
			OBOSession session, FieldPath path, byte condition) {
		return getCheckTask(getCheckObsoletes(), liveChecks, session, path,
				condition);
	}

	public CheckTask getCheckTask(boolean checkObsoletes,
			Collection<Check> liveChecks, OBOSession session, FieldPath path,
			byte condition) {
		CheckTask task = new CheckTask(checkObsoletes, liveChecks, session,
				path, condition);
		for (VerificationListener listener : verificationListeners) {
			task.addVerificationListener(listener);
		}
		return task;
	}

	public Collection<CheckWarning> runChecks(Collection<Check> liveChecks,
			OBOSession session, FieldPath path, byte condition) {
		CheckTask task = getCheckTask(getCheckObsoletes(), liveChecks, session,
				path, condition);
		task.run();
		if (task.getException() != null) {
		    System.err.println("runChecks: CheckTask got an exception.  Here's the stack trace.");
			task.getException().printStackTrace();
		    System.err.println("runChecks: re-throwing exception.");
			throw new RuntimeException(task.getException());
		}
		return task.getResults();
	}

	public void addVerificationListener(VerificationListener listener) {
		verificationListeners.add(listener);
	}

	public void removeVerificationListener(VerificationListener listener) {
		verificationListeners.remove(listener);
	}
}
