package org.oboedit.verify.impl;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;

import org.bbop.util.ObjectUtil;
import org.bbop.util.StringUtil;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.filters.DefinitionDbxrefSearchCriterion;
import org.obo.filters.GeneralDbxrefSearchCriterion;
import org.obo.filters.SynonymDbxrefSearchCriterion;
import org.obo.filters.SynonymSearchCriterion;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.DelDbxrefHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.TermUtil;
import org.obo.util.TextUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.AbstractCheck;
import org.oboedit.verify.CheckConfiguration;
import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.FieldCheck;
import org.oboedit.verify.OntologyCheck;
import org.oboedit.verify.QuickFix;

import org.apache.log4j.*;

public class DbxrefCheck extends AbstractCheck implements FieldCheck,
	OntologyCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefCheck.class);
	public static final FieldPathSpec DEF_DBXREF_PATH_SPEC = new FieldPathSpec(
			DefinitionDbxrefSearchCriterion.CRITERION);

	public static final FieldPathSpec DBXREF_PATH_SPEC = new FieldPathSpec(
			GeneralDbxrefSearchCriterion.CRITERION);

	public static final FieldPathSpec SYNONYM_DBXREF_PATH_SPEC = new FieldPathSpec(
			SynonymSearchCriterion.CRITERION,
			SynonymDbxrefSearchCriterion.CRITERION);

	protected static Collection<FieldPathSpec> PATH_SPECS = new LinkedList<FieldPathSpec>();

	static {
		PATH_SPECS.add(DEF_DBXREF_PATH_SPEC);
		PATH_SPECS.add(SYNONYM_DBXREF_PATH_SPEC);
		PATH_SPECS.add(DBXREF_PATH_SPEC);
	}

	protected JCheckBox missingDbCheckBox = new JCheckBox("Check for missing "
			+ "database");
	protected JCheckBox missingIDCheckBox = new JCheckBox("Check for missing "
			+ "ID");
	protected JCheckBox badCharacterCheckBox = new JCheckBox(
			"Check for non-URL " + "characters");
	protected JCheckBox validURLCheckBox = new JCheckBox(
			"Check that URL dbxrefs " + "are valid URLs");
	protected JCheckBox descriptionCheckBox = new JCheckBox("Check for "
			+ "consistency in dbxref descriptions");

	protected class ConfigurationPanel extends JPanel implements ActionListener {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			updateConfiguration();
		}
	}

	protected ConfigurationPanel configurationPanel = new ConfigurationPanel();

	public static class DbxrefCheckConfiguration extends CheckConfiguration {
		protected boolean doMissingDbCheck = true;
		protected boolean doMissingIDCheck = true;
		protected boolean doBadCharactersCheck = true;
		protected boolean doValidURLCheck = true;
		protected boolean doDescriptionsCheck = true;

		public DbxrefCheckConfiguration() {

		}

		public boolean getDoBadCharactersCheck() {
			return doBadCharactersCheck;
		}

		public void setDoBadCharactersCheck(boolean doBadCharactersCheck) {
			this.doBadCharactersCheck = doBadCharactersCheck;
		}

		public boolean getDoDescriptionsCheck() {
			return doDescriptionsCheck;
		}

		public void setDoDescriptionsCheck(boolean doDescriptionsCheck) {
			this.doDescriptionsCheck = doDescriptionsCheck;
		}

		public boolean getDoMissingDbCheck() {
			return doMissingDbCheck;
		}

		public void setDoMissingDbCheck(boolean doMissingDbCheck) {
			this.doMissingDbCheck = doMissingDbCheck;
		}

		public boolean getDoMissingIDCheck() {
			return doMissingIDCheck;
		}

		public void setDoMissingIDCheck(boolean doMissingIDCheck) {
			this.doMissingIDCheck = doMissingIDCheck;
		}

		public boolean getDoValidURLCheck() {
			return doValidURLCheck;
		}

		public void setDoValidURLCheck(boolean doValidURLCheck) {
			this.doValidURLCheck = doValidURLCheck;
		}
	}

	public DbxrefCheck() {
	}

	protected void updateConfiguration() {
		DbxrefCheckConfiguration dbc = (DbxrefCheckConfiguration) configuration;
		dbc.setDoMissingDbCheck(missingDbCheckBox.isSelected());
		dbc.setDoMissingIDCheck(missingIDCheckBox.isSelected());
		dbc.setDoBadCharactersCheck(badCharacterCheckBox.isSelected());
		dbc.setDoValidURLCheck(validURLCheckBox.isSelected());
		dbc.setDoDescriptionsCheck(descriptionCheckBox.isSelected());
	}

	@Override
	protected void initConfiguration() {
		configuration.setCondition((byte) (
						   VerificationManager.TEXT_EDIT_THREAD |
						   VerificationManager.TEXT_EDIT_COMMIT
						   | VerificationManager.SAVE | VerificationManager.MANUAL));
	}

	@Override
	protected CheckConfiguration createConfiguration() {
		return new DbxrefCheckConfiguration();
	}

	@Override
	public JComponent getConfigurationPanel() {
		configurationPanel.removeAll();
		configurationPanel.setLayout(new BoxLayout(configurationPanel,
				BoxLayout.Y_AXIS));

		missingDbCheckBox.setOpaque(false);
		missingIDCheckBox.setOpaque(false);
		badCharacterCheckBox.setOpaque(false);
		validURLCheckBox.setOpaque(false);
		descriptionCheckBox.setOpaque(false);

		DbxrefCheckConfiguration dbc = (DbxrefCheckConfiguration) configuration;
		missingDbCheckBox.setSelected(dbc.getDoMissingDbCheck());
		missingIDCheckBox.setSelected(dbc.getDoMissingIDCheck());
		badCharacterCheckBox.setSelected(dbc.getDoBadCharactersCheck());
		validURLCheckBox.setSelected(dbc.getDoValidURLCheck());
		descriptionCheckBox.setSelected(dbc.getDoDescriptionsCheck());

		configurationPanel.add(missingDbCheckBox);
		configurationPanel.add(missingIDCheckBox);
		configurationPanel.add(badCharacterCheckBox);
		configurationPanel.add(validURLCheckBox);
		configurationPanel.add(descriptionCheckBox);

		return configurationPanel;
	}

	public Collection<CheckWarning> check(OBOSession session, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {
		LinkedList<CheckWarning> out = new LinkedList<CheckWarning>();
		Collection<IdentifiedObject> checkSet;
		Map<String, Collection<DbxrefDescRecord>> descMap = null;
		boolean doDescCheck = ((DbxrefCheckConfiguration) configuration)
				.getDoDescriptionsCheck();
		if (doDescCheck)
			descMap = new HashMap<String, Collection<DbxrefDescRecord>>();

		if (currentObject != null && !doDescCheck) {
			checkSet = Collections.singleton(currentObject);
		} else
			checkSet = session.getObjects();

		Iterator<IdentifiedObject> it = checkSet.iterator();
		for (int i = 0; it.hasNext(); i++) {
			IdentifiedObject io = it.next();
			int percentage = 100 * i / session.getObjects().size();
			setProgressValue(percentage);
			setProgressString("checking object " + (i + 1) + " of "
					+ session.getObjects().size());
			if (!checkObsoletes && TermUtil.isObsolete(io))
				continue;
			if (doDescCheck) {
				mapDbxrefs(descMap, io);
			}
			if (currentObject != null && !io.equals(currentObject))
				continue;
			if (io instanceof DbxrefedObject) {
				checkDbxrefs(((DbxrefedObject) io).getDbxrefs(), io, out,
						"Dbxref");
			}
			if (io instanceof DefinedObject) {
				checkDbxrefs(((DefinedObject) io).getDefDbxrefs(), io, out,
						"Definition dbxref");
			}
			if (io instanceof SynonymedObject) {
				Iterator<Synonym> it2 = ((SynonymedObject) io).getSynonyms().iterator();
				for (int sindex = 0; it2.hasNext(); sindex++) {
					Synonym s = it2.next();
					checkDbxrefs(s.getXrefs(), io, out, "Synonym " + i
							+ " dbxref ");
				}
			}
			if (isCancelled() || out.size() > VerificationManager.MAX_WARNINGS)
				return out;

		}

		if (doDescCheck) {
			for(String key : descMap.keySet()) {
				final Collection<DbxrefDescRecord> c = descMap.get(key);
				if (c != null && c.size() > 1) {
					final Map<String, Collection<DbxrefDescRecord>> textMap = new HashMap<String, Collection<DbxrefDescRecord>>();
					for (final DbxrefDescRecord record : c) {
						if (currentObject != null
								&& !currentObject.equals(record.getObj()))
							continue;
						Collection<DbxrefDescRecord> records = textMap.get(record.getDesc());
						if (records == null) {
							records = new LinkedList<DbxrefDescRecord>();
							textMap.put(record.getDesc(), records);
						}
						records.add(record);
					}
					if (textMap.size() > 1) {
						StringBuffer message = new StringBuffer("The dbxref "
								+ key + " has several different descriptions: ");
						
						LinkedList<QuickFix> fixes = new LinkedList<QuickFix>();
						if (condition != VerificationManager.TEXT_EDIT_COMMIT) {
							HistoryItem item = getDbxrefDescItem(textMap, null);
							fixes.add(new DefaultHistoryQuickFix(message.toString(), item));
						}
						int i = -1;
						for (final String desc : textMap.keySet()) {
							i++;
							if (i > 0 && textMap.keySet().size() > 2)
								message.append(", ");
							if (i > 0 && i == textMap.keySet().size() - 1)
								message.append(" and ");

							if (desc != null 
									&& condition != VerificationManager.TEXT_EDIT_COMMIT) {
								HistoryItem item = getDbxrefDescItem(textMap, desc);
								String msg = "Set all dbxref " + "descriptions to '" + desc + "'.";
								fixes.add(new DefaultHistoryQuickFix(msg, item));
							}
							Collection<DbxrefDescRecord> records = textMap.get(desc);
							if (desc == null)
								message.append("no description in ");
							else
								message.append("'" + desc + "' in ");
							int j = -1;
							for (DbxrefDescRecord record : records) {
								j++;
								if (j > 0 && records.size() > 2)
									message.append(", ");
								if (j > 0 && j == records.size() - 1)
									message.append(" and ");
								message.append("<a href='file:"
										+ record.getObj().getID() + "'>"
										+ record.getObj().getID() + "</a>");
							}

						}
						message.append(".");
						out.add(new CheckWarning(message.toString(), false,
								this, currentObject, fixes));
						if (out.size() > VerificationManager.MAX_WARNINGS)
							return out;
					}
				}
			}
		}

		return out;
	}

	protected static HistoryItem getDbxrefDescItem(Map<String, Collection<DbxrefDescRecord>> textMap, String desc) {
		TermMacroHistoryItem mitem = new TermMacroHistoryItem(
				"Set dbxref descriptions to " + desc);
		for(Collection<DbxrefDescRecord> records : textMap.values()) {
			for (DbxrefDescRecord record : records) {
				IdentifiedObject io = record.getObj();
				if (io instanceof DbxrefedObject) {
					DbxrefedObject dbo = (DbxrefedObject) io;
					for(Dbxref ref : dbo.getDbxrefs()) {
						String key = ref.getDatabase() + ":" + ref.getDatabaseID();
						if (key.equals(record.getDbxref())
								&& !ObjectUtil.equals(ref.getDesc(), desc)) {
							HistoryItem item = new DelDbxrefHistoryItem(record
									.getObj().getID(), ref, false, null);
							mitem.addItem(item);
							ref = (Dbxref) ref.clone();
							ref.setDesc(desc);
							item = new AddDbxrefHistoryItem(record.getObj()
									.getID(), ref, false, null);
							mitem.addItem(item);
						}
					}
				}
				if (io instanceof DefinedObject) {
					DefinedObject dbo = (DefinedObject) io;
					for(Dbxref ref : dbo.getDefDbxrefs()) {
						String key = ref.getDatabase() + ":" + ref.getDatabaseID();
						if (key.equals(record.getDbxref())
								&& !ObjectUtil.equals(ref.getDesc(), desc)) {
							HistoryItem item = new DelDbxrefHistoryItem(record
									.getObj().getID(), ref, true, null);
							mitem.addItem(item);
							ref = (Dbxref) ref.clone();
							ref.setDesc(desc);
							item = new AddDbxrefHistoryItem(record.getObj()
									.getID(), ref, true, null);
							mitem.addItem(item);
						}
					}
				}
				if (io instanceof SynonymedObject) {
					SynonymedObject dbo = (SynonymedObject) io;
					for(Synonym s : dbo.getSynonyms()) {
						for(Dbxref ref : s.getXrefs()) {
							String key = ref.getDatabase() + ":" + ref.getDatabaseID();
							if (key.equals(record.getDbxref())
									&& !ObjectUtil.equals(ref.getDesc(), desc)) {
								HistoryItem item = new DelDbxrefHistoryItem(
										record.getObj().getID(), ref, false, s
												.getText());
								mitem.addItem(item);
								ref = (Dbxref) ref.clone();
								ref.setDesc(desc);
								item = new AddDbxrefHistoryItem(record.getObj()
										.getID(), ref, false, s.getText());
								mitem.addItem(item);
							}
						}
					}
				}
			}
		}
		return mitem;
	}

	protected void mapDbxrefs(Map<String, Collection<DbxrefDescRecord>> descMap, IdentifiedObject io) {
		for(Dbxref dbxref : TextUtil.getAllDbxrefs(io)) {
			addMapRecord(descMap, dbxref, io);
		}
	}

	protected void addMapRecord(Map<String, Collection<DbxrefDescRecord>> descMap, Dbxref ref, IdentifiedObject obj) {
		String key = ref.getDatabase() + ":" + ref.getDatabaseID();
		DbxrefDescRecord record = new DbxrefDescRecord(obj, key, ref.getDesc());
		Collection<DbxrefDescRecord> c = descMap.get(key);
		if (c == null) {
			c = new LinkedList<DbxrefDescRecord>();
			descMap.put(key, c);
		}
		c.add(record);
	}

	protected void checkDbxrefs(Collection<Dbxref> dbxrefs,
			IdentifiedObject currentObject, LinkedList<CheckWarning> out, String title) {
		int i = 0;
		for (Dbxref ref : dbxrefs) {
			checkDbxref(ref, currentObject, out, i++, title);
		}
	}

	protected static String getWarningHeader(String title, int index,
			IdentifiedObject currentObject) {
		String indexStr = "";
		if (index > 0)
			indexStr = " " + (index + 1);
		return title + indexStr + " of <a href='file:" + currentObject.getID()
				+ "'>" + currentObject.getID() + "</a> ";
	}

	protected static boolean isURLStarter(String prefix) {
		String[] starters = { "ftp", "http", "https", "telnet", "ssh" };
		for (int i = 0; i < starters.length; i++)
			if (prefix.equalsIgnoreCase(starters[i]))
				return true;
		return false;
	}

	protected static class DbxrefDescRecord {
		protected IdentifiedObject obj;
		protected String dbxref;
		protected String desc;
		protected boolean complained = false;

		public DbxrefDescRecord(IdentifiedObject obj, String dbxref, String desc) {
			this.obj = obj;
			this.dbxref = dbxref;
			this.desc = desc;
		}

		public void setComplained(boolean complained) {
			this.complained = complained;
		}

		public boolean getComplained() {
			return complained;
		}

		public String getDbxref() {
			return dbxref;
		}

		public void setDbxref(String dbxref) {
			this.dbxref = dbxref;
		}

		public String getDesc() {
			return desc;
		}

		public void setDesc(String desc) {
			this.desc = desc;
		}

		public IdentifiedObject getObj() {
			return obj;
		}

		public void setObj(IdentifiedObject obj) {
			this.obj = obj;
		}
	}

	protected void checkDbxref(Dbxref ref, IdentifiedObject currentObject,
			LinkedList<CheckWarning> out, int index, String title) {
		checkDbxref(ref, new FieldPath(currentObject), out, index, title);
	}

	protected void checkDbxref(Dbxref ref, FieldPath field, LinkedList<CheckWarning> out,
			int index, String title) {
		DbxrefCheckConfiguration dbconfig = (DbxrefCheckConfiguration) configuration;
		if (dbconfig.getDoMissingDbCheck() && ((ref.getDatabase().length() == 0) ||
                                                       ref.getDatabase().equals("XX"))) {
			out.add(new CheckWarning(getWarningHeader(title, index, field
					.getObject())
					+ " has no database name.", false, this, field));
		}
		if (dbconfig.getDoMissingIDCheck() && (ref.getDatabaseID().length() == 0 ||
                                                       ref.getDatabaseID().equals("0") ||
                                                       ref.getDatabaseID().equals("<new xref>"))) {
			out.add(new CheckWarning(getWarningHeader(title, index, field
					.getObject())
					+ " has no ID.", false, this, field));
		}
		if (dbconfig.getDoValidURLCheck() && isURLStarter(ref.getDatabase())) {
			try {
				new URL(ref.getDatabase() + ":" + ref.getDatabaseID());
			} catch (MalformedURLException ex) {
				out.add(new CheckWarning(getWarningHeader(title, index, field
						.getObject())
						+ " has a database prefix that looks like a URL prefix, but is not a valid URL.",
						false, this, field));
			}
		}
		if (dbconfig.getDoBadCharactersCheck()) {
			if (!StringUtil.containsOnlyValidURICharacters(ref.getDatabaseID())
					|| !StringUtil.containsOnlyValidURICharacters(ref
							.getDatabase())) {
				Collection<QuickFix> fixes = new ArrayList<QuickFix>();
				fixes.add(new DefaultHistoryQuickFix(
						"Remove non-URI characters", getNonURIReplaceItem(
								field, ref, '\0')));
				fixes.add(new DefaultHistoryQuickFix(
						"Replace non-URI characters with underscores",
						getNonURIReplaceItem(field, ref, '_')));
				fixes.add(new DefaultHistoryQuickFix(
						"Replace non-URI characters with dashes",
						getNonURIReplaceItem(field, ref, '-')));
				// Wouldn't it be useful to say what the bad characters are??
				CheckWarning warning = new CheckWarning(getWarningHeader(title,
						index, field.getObject())
						+ " contains non-URI characters (such as >).", false, this, field,
						fixes);
				out.add(warning);
			}
		}
	}

	protected static HistoryItem getNonURIReplaceItem(FieldPath field,
			Dbxref ref, char replaceChar) {
		TermMacroHistoryItem item = new TermMacroHistoryItem();
		Synonym s = null;
		if (field.getLastField().equals(SynonymDbxrefSearchCriterion.CRITERION)) {
			FieldPath parentPath = field.getParentPath();
			s = (Synonym) parentPath.getLastValue();
		}
		Dbxref newDbxref = SessionManager
				.getManager()
				.getSession()
				.getObjectFactory()
				.createDbxref(
						replaceNonURICharacters(ref.getDatabase(), replaceChar),
						replaceNonURICharacters(ref.getDatabaseID(),
								replaceChar), ref.getDesc(), ref.getType(), s);
		item.addItem(new DelDbxrefHistoryItem(field.getObject().getID(), ref,
				ref.isDefRef(), s == null ? null : s.getText()));
		item.addItem(new AddDbxrefHistoryItem(field.getObject().getID(),
				newDbxref, newDbxref.isDefRef(),
				s == null ? null : s.getText()));
		return item;
	}

	protected static String replaceNonURICharacters(String s, char replaceChar) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (StringUtil.isValidURICharacter(c))
				out.append(c);
			else if (replaceChar != '\0')
				out.append(replaceChar);
		}
		return out.toString();
	}

	public String getID() {
		return "dbxref_check";
	}

	@Override
	public String getDescription() {
		return "Dbxref Check";
	}

	public Collection<CheckWarning> check(OBOSession session, FieldPath path,
			byte condition, boolean checkObsoletes) {
		LinkedList<CheckWarning> out = new LinkedList<CheckWarning>();

		if (path.getLastValue() instanceof Dbxref) {
			checkDbxref((Dbxref) path.getLastValue(), path, out, 0, "Dbxref");
		}

		return out;
	}

	public Collection<FieldPathSpec> getPaths() {
		return PATH_SPECS;
	}

}
