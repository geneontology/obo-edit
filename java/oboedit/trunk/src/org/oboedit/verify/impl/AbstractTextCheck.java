package org.oboedit.verify.impl;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.Character.UnicodeBlock;
import java.net.*;
import java.util.*;

import javax.swing.*;
import org.apache.log4j.*;

import org.apache.log4j.Logger;
import org.bbop.framework.GUIManager;
import org.bbop.io.FileUtil;
import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.verify.*;

import com.swabunga.spell.engine.SpellDictionary;
import com.swabunga.spell.engine.SpellDictionaryHashMap;
import com.swabunga.spell.engine.Word;
import com.swabunga.spell.event.DefaultWordFinder;
import com.swabunga.spell.event.SpellCheckEvent;
import com.swabunga.spell.event.SpellCheckListener;
import com.swabunga.spell.event.SpellChecker;
import com.swabunga.spell.event.StringWordTokenizer;
import com.swabunga.spell.event.WordFinder;

public abstract class AbstractTextCheck extends AbstractCheck implements
FieldCheck {

	//	initialize logger
	protected final static Logger logger = Logger.getLogger(CycleCheck.class);

	public static class TooManyWarningsException extends RuntimeException {
		protected Collection<CheckWarning> warnings;

		public TooManyWarningsException(Collection<CheckWarning> warnings) {
			this.warnings = warnings;
		}

		public Collection<CheckWarning> getWarnings() {
			return warnings;
		}
	}

	protected class ConfigurationPanel extends JPanel implements ActionListener {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			updateConfiguration();
		}
	}

	public static class AbstractCheckConfiguration extends CheckConfiguration {
		protected byte repeatedWordCondition = VerificationManager.ALL;

		protected byte repeatedWhitespaceCondition = VerificationManager.ALL;

		protected byte sentenceCaseCondition = VerificationManager.ALL;

		protected byte sentenceSeparationCondition = VerificationManager.ALL;

		protected byte finalPunctuationCondition = VerificationManager.ALL;

		protected byte spellcheckCondition = VerificationManager.TEXT_EDIT_THREAD
		^ VerificationManager.TEXT_EDIT_COMMIT 
		^ VerificationManager.MANUAL;

		public AbstractCheckConfiguration() {
			super();
		}

		public byte getRepeatedWordCondition() {
			return repeatedWordCondition;
		}

		public void setRepeatedWordCondition(byte repeatedWordCondition) {
			this.repeatedWordCondition = repeatedWordCondition;
		}

		public byte getRepeatedWhitespaceCondition() {
			return repeatedWhitespaceCondition;
		}

		public void setRepeatedWhitespaceCondition(
				byte repeatedWhitespaceCondition) {
			this.repeatedWhitespaceCondition = repeatedWhitespaceCondition;
		}

		public byte getSentenceCaseCondition() {
			return sentenceCaseCondition;
		}

		public void setSentenceCaseCondition(byte sentenceCaseCondition) {
			this.sentenceCaseCondition = sentenceCaseCondition;
		}

		public byte getSentenceSeparationCondition() {
			return sentenceSeparationCondition;
		}

		public void setSentenceSeparationCondition(
				byte sentenceSeparationCondition) {
			this.sentenceSeparationCondition = sentenceSeparationCondition;
		}

		public void setFinalPunctuationCondition(byte finalPunctuationCondition) {
			this.finalPunctuationCondition = finalPunctuationCondition;
		}

		public byte getFinalPunctuationCondition() {
			return finalPunctuationCondition;
		}

		public byte getSpellcheckCondition() {
			return spellcheckCondition;
		}

		public void setSpellcheckCondition(byte spellcheckCondition) {
			this.spellcheckCondition = spellcheckCondition;
		}
	}

	protected class ReplacementFix implements TextReplaceQuickFix {

		protected FieldPath path;

		protected String newText;

		protected String desc;

		protected CheckWarning warning;

		public ReplacementFix(String desc, FieldPath path, String newText) {
			this.desc = desc;
			this.path = path;
			this.newText = newText;
		}

		public HistoryItem getItem() {
			return getFieldChangeHistoryItem(path.getObject(), newText);
		}

		public String getNewText() {
			return newText;
		}

		public FieldPath getPath() {
			return path;
		}

		public String getDesc() {
			return desc;
		}

		public ReloadLevel getLevel() {
			return QuickFix.ReloadLevel.TERM;
		}

		public CheckWarning getWarning() {
			return warning;
		}

		public void setWarning(CheckWarning warning) {
			this.warning = warning;
		}

	}

	protected String[] selections = { "Threaded checks only",
			"On Text Edit / Manual", "Always", "Never" };

	protected JComboBox finalPunctuationList = new JComboBox(selections);

	protected JComboBox repeatedWordList = new JComboBox(selections);

	protected JComboBox repeatedWhitespaceList = new JComboBox(selections);

	protected JComboBox spellcheckList = new JComboBox(selections);

	protected JComboBox sentenceCaseList = new JComboBox(selections);

	protected JComboBox sentenceSeparationList = new JComboBox(selections);

	protected ConfigurationPanel configurationPanel = new ConfigurationPanel();

	protected boolean allowNewlines = false;

	protected boolean allowBlank = false;

	protected boolean allowExtended = false;

	protected boolean sentenceStructureChecks = false;

	protected static Collection defaultPeriodWords = new LinkedList();

	protected static Set periodWords = null;

	protected static Set allowedRepeats = null;

	protected static Set alwaysLowercaseWords = null;

	//	{
	//		defaultPeriodWords.add("i.e.");
	//		defaultPeriodWords.add("e.g.");
	//		defaultPeriodWords.add("etc.");
	//	}

	protected static SpellChecker stdspellChecker;
	protected static SpellChecker usrspellChecker;

	//standard dictionary spell check
	public SpellChecker getSpellChecker() {
//		logger.debug("standard spell check... ");
		if (stdspellChecker == null) {
			SpellDictionary dictionary = null;
			try {
				FileUtil.ensureExists(Preferences.getStandardDictionaryFile(), "org/oboedit/resources/standard.dict");
				dictionary = new SpellDictionaryHashMap(Preferences.getStandardDictionaryFile());
			} catch (IOException e) {
				logger.debug(e);
			}
			stdspellChecker = new SpellChecker(dictionary);
		}
		return stdspellChecker;
	}
	//user-defined (domain specific vocabulary) spell check
	public SpellChecker getUserDefSpellChecker() {
//		logger.debug("user-defined dict spell check");
		if (usrspellChecker == null) {
			SpellDictionary dictionary = null;
			try {
				FileUtil.ensureExists(Preferences.getUserDefDictionaryFile(), "org/oboedit/resources/user.dict");
				dictionary = new SpellDictionaryHashMap(Preferences.getUserDefDictionaryFile());
			} catch (IOException e) {
				logger.debug(e);
			}
			usrspellChecker = new SpellChecker(dictionary);
		}
		return usrspellChecker;
	}

	protected static Set getAllowedRepeats() {
		if (allowedRepeats == null){
			reloadWordSets();
			//logger.debug("getAllowedRepeats: allowedRepeats = " + allowedRepeats.toString());
		}
		if (allowedRepeats.isEmpty()) {
			try {
				FileUtil.ensureExists(Preferences.getAllowedRepeatsFile(),
				"org/oboedit/resources/allowedrepeats.dict");
				BufferedReader input = new BufferedReader( new FileReader(Preferences.getAllowedRepeatsFile()));
				String text;
				while ( (text = input.readLine() ) != null ) {
					allowedRepeats.add(text);
					//logger.debug("Legal repeatable word loaded from config file = " + text);
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return allowedRepeats;
	}

	protected static Set getPeriodWords() {
		if (periodWords == null){
			reloadWordSets();
			//logger.debug("getPeriodWords: periodWords = " + periodWords.toString());
		}
		if (periodWords.isEmpty()) {
			try {
				FileUtil.ensureExists(Preferences.getPeriodWordsFile(),
				"org/oboedit/resources/periodwords.dict");
				BufferedReader input = new BufferedReader( new FileReader(Preferences.getPeriodWordsFile()));
				String text;
				while ( (text = input.readLine() ) != null ) {
					periodWords.add(text.trim());
					//logger.debug("Legal period word loaded from config file = " + text);
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return periodWords;
	}

	protected static Set getAlwaysLowercaseWords() {
		if (alwaysLowercaseWords == null){
			reloadWordSets();
			//logger.debug("getAlwaysLowercaseWords: alwaysLowercaseWords = " + alwaysLowercaseWords.toString());
		}
		if (alwaysLowercaseWords.isEmpty()) {
			try {
				FileUtil.ensureExists(Preferences.getAlwaysLowercaseFile(),
				"org/oboedit/resources/alwayslowercase.dict");
				BufferedReader input = new BufferedReader( new FileReader(Preferences.getAlwaysLowercaseFile()));
				String lowercase;
				while ( (lowercase = input.readLine() ) != null ) {
					alwaysLowercaseWords.add(lowercase.trim());
					//logger.debug("Legal lowercase word loaded from config file = " + lowercase);
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return alwaysLowercaseWords;
	}


	protected static void reloadWordSets() {
		allowedRepeats = loadWordSet("allowedrepeats.dict");
		periodWords = loadWordSet("periodwords.dict");
		alwaysLowercaseWords = loadWordSet("alwayslowercase.dict");
		periodWords.addAll(defaultPeriodWords);
	}

	protected static void flushWordSets() {
		writeWordSet(allowedRepeats, "allowedrepeats.dict");
		writeWordSet(periodWords, "periodwords.dict");
		writeWordSet(alwaysLowercaseWords, "alwayslowercase.dict");
	}

	protected static Set loadWordSet(String filename) {
		Set out = new HashSet();
		try {
			BufferedReader reader = new BufferedReader(new FileReader(new File(
					GUIManager.getPrefsDir(), filename)));
			String line;
			while ((line = reader.readLine()) != null) {
				out.add(line);
			}
		} catch (IOException ex) {
		}
		return out;
	}

	protected static void writeWordSet(Collection words, String filename) {
		//logger.debug("words = " + words.toString());
		StringBuffer buf = new StringBuffer();
		Iterator it = words.iterator();
		//logger.debug("writeWordSet: it = " + it.toString());
		while (it.hasNext()) {
			buf.append(it.next()).append("%n");

		}
		writeWordSet(buf.toString(), filename);

		//logger.debug("writeWordSet: buf.toString(), filename = " + buf.toString() + filename);
	}


	protected static void writeWordSet(String words, String filename) {
		File file = new File(GUIManager.getPrefsDir(), filename);
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			writer.write(words.trim());
			writer.close();
		} catch (Exception ex) {
		}
	}

	protected static String getConditionWord(int condition) {

		if (condition == 0)
			return "Never";
		if (condition == VerificationManager.TEXT_EDIT_THREAD)
			return "Threaded checks only";
		if (condition == (VerificationManager.TEXT_EDIT_COMMIT
				| VerificationManager.TEXT_EDIT_THREAD | VerificationManager.MANUAL))
			return "On Text Edit / Manual";
		return "Always";
	}

	protected static byte getCondition(String word) {
		if (word.equals("Always")) {
			return VerificationManager.ALL;
		} else if (word.equals("Threaded checks only")) {
			return VerificationManager.TEXT_EDIT_THREAD;
		} else if (word.equals("On Text Edit / Manual")) {
			return VerificationManager.TEXT_EDIT_COMMIT
			^ VerificationManager.TEXT_EDIT_THREAD
			^ VerificationManager.MANUAL;
		} else {
			return 0;
		}
	}

	protected void updateConfiguration() {
		AbstractCheckConfiguration config = (AbstractCheckConfiguration) configuration;

		config.setRepeatedWordCondition(getCondition(repeatedWordList
				.getSelectedItem().toString()));
		config.setFinalPunctuationCondition(getCondition(finalPunctuationList
				.getSelectedItem().toString()));
		config
		.setRepeatedWhitespaceCondition(getCondition(repeatedWhitespaceList
				.getSelectedItem().toString()));
		config.setSentenceCaseCondition(getCondition(sentenceCaseList
				.getSelectedItem().toString()));
		config
		.setSentenceSeparationCondition(getCondition(sentenceSeparationList
				.getSelectedItem().toString()));
		config.setSpellcheckCondition(getCondition(spellcheckList
				.getSelectedItem().toString()));
		reloadWordSets();
		// read repeated word list, write to disk
	}

	@Override
	protected void initConfiguration() {
		configuration.setCondition((byte) (VerificationManager.TEXT_EDIT_THREAD
				| VerificationManager.TEXT_EDIT_COMMIT
				| VerificationManager.SAVE | VerificationManager.MANUAL));
	}

	protected abstract Collection getStrings(IdentifiedObject io);

	protected abstract String getWarningLabel(IdentifiedObject io,
			byte condition, int index);

	protected abstract String getWarningLabel(FieldPath path, byte condition);

	@Override
	public boolean needsReasoner() {
		return false;
	}

	@Override
	public void setReasoner(ReasonedLinkDatabase linkDatabase) {
	}

	@Override
	protected CheckConfiguration createConfiguration() {
		return new AbstractCheckConfiguration();
	}

	public boolean getSentenceStructureChecks() {
		return sentenceStructureChecks;
	}

	public boolean getAllowNewlines() {
		return allowNewlines;
	}

	public boolean getAllowExtended() {
		return allowExtended;
	}

	public boolean getAllowBlank() {
		return allowBlank;
	}

	public void setSentenceStructureChecks(boolean sentenceStructureChecks) {
		this.sentenceStructureChecks = sentenceStructureChecks;
	}

	public void setAllowExtended(boolean allowExtended) {
		this.allowExtended = allowExtended;
	}

	public void setAllowNewlines(boolean allowNewlines) {
		this.allowNewlines = allowNewlines;
	}

	public void setAllowBlank(boolean allowBlank) {
		this.allowBlank = allowBlank;
	}

	@Override
	public JComponent getConfigurationPanel() {
		configurationPanel.removeAll();
		configurationPanel.setLayout(new BoxLayout(configurationPanel,
				BoxLayout.Y_AXIS));
		JPanel springPanel = new JPanel();
		springPanel.setLayout(new SpringLayout());

		/*
		 * ActionListener actionListener = new ActionListener() { public void
		 * actionPerformed(ActionEvent e) { updateConfiguration(); } };
		 * finalPunctuationList.addActionListener(actionListener);
		 * repeatedWordList.addActionListener(actionListener);
		 * repeatedWhitespaceList.addActionListener(actionListener);
		 * sentenceCaseList.addActionListener(actionListener);
		 * sentenceSeparationList.addActionListener(actionListener);
		 */

		JLabel finalPunctuationLabel = new JLabel("Do final punctuation check");
		JLabel repeatedWordLabel = new JLabel("Do repeated word check");
		JLabel repeatedWhitespaceLabel = new JLabel(
		"Do repeated whitespace check");
		JLabel sentenceCaseLabel = new JLabel("Do sentence case check");
		JLabel sentenceSeparationLabel = new JLabel(
		"Do sentence separation check");
		JLabel spellcheckLabel = new JLabel("Do spell checks");

		springPanel.add(finalPunctuationLabel);
		springPanel.add(finalPunctuationList);
		springPanel.add(repeatedWordLabel);
		springPanel.add(repeatedWordList);
		springPanel.add(repeatedWhitespaceLabel);
		springPanel.add(repeatedWhitespaceList);
		springPanel.add(spellcheckLabel);
		springPanel.add(spellcheckList);

		if (getSentenceStructureChecks()) {
			springPanel.add(sentenceCaseLabel);
			springPanel.add(sentenceCaseList);
			springPanel.add(sentenceSeparationLabel);
			springPanel.add(sentenceSeparationList);
		}

		finalPunctuationList
		.setSelectedItem(getConditionWord(((AbstractCheckConfiguration) configuration)
				.getFinalPunctuationCondition()));
		repeatedWordList
		.setSelectedItem(getConditionWord(((AbstractCheckConfiguration) configuration)
				.getRepeatedWordCondition()));
		repeatedWhitespaceList
		.setSelectedItem(getConditionWord(((AbstractCheckConfiguration) configuration)
				.getRepeatedWhitespaceCondition()));
		sentenceCaseList
		.setSelectedItem(getConditionWord(((AbstractCheckConfiguration) configuration)
				.getSentenceCaseCondition()));
		sentenceSeparationList
		.setSelectedItem(getConditionWord(((AbstractCheckConfiguration) configuration)
				.getSentenceSeparationCondition()));
		spellcheckList
		.setSelectedItem(getConditionWord(((AbstractCheckConfiguration) configuration)
				.getSpellcheckCondition()));
		SpringUtilities.makeCompactGrid(springPanel, -1, 2, // rows, cols
				6, 6, // initX, initY
				6, 6);
		configurationPanel.add(springPanel);
		return configurationPanel;
	}

	public Collection<CheckWarning> check(OBOSession session, FieldPath path,
			byte condition, boolean checkObsoletes) {
		cancelled = false;
		if (!checkObsoletes && TermUtil.isObsolete(path.getObject()))
			return Collections.emptyList();

		Collection<CheckWarning> out = new LinkedList<CheckWarning>();
		if (path.getLastValue() instanceof String) {
			try {
				out.addAll(getWarnings(path, allowNewlines, allowBlank,
						allowExtended, sentenceStructureChecks, condition));
				//				logger.debug("check: added " + out.size() + " warnings"); // DEL
			} catch (TooManyWarningsException ex) {
				out.addAll(ex.getWarnings());
			}
		}
		appendAdditionalWarnings(out, session, path, condition);
		//		logger.debug("check: out.size = " + out.size()); // DEL
		return out;
	}

	protected void appendAdditionalWarnings(Collection out, OBOSession session,
			FieldPath path, byte condition) {
	}

	protected void appendAdditionalWarnings(Collection out, OBOSession session,
			IdentifiedObject currentObject, byte condition) {
	}

	protected int[] getCurrentPeriodWordRange(String text, int index) {
		int previousIndex;
		int nextIndex;
		for (previousIndex = index - 1; previousIndex > 0; previousIndex--) {
			char c = text.charAt(previousIndex);
			if (!Character.isLetterOrDigit(c) && c != '.') {
				previousIndex++;
				break;
			}
		}
		if (previousIndex == -1)
			previousIndex = 0;
		for (nextIndex = index; nextIndex < text.length(); nextIndex++) {
			char c = text.charAt(nextIndex);
			if (!Character.isLetterOrDigit(c) && c != '.') {
				// nextIndex++;
				break;
			}
		}
		int[] out = new int[2];
		out[0] = previousIndex;
		out[1] = nextIndex;
		return out;
	}

	protected int isLegalPeriodWord(String text, int index,
			IdentifiedObject currentObject) {
		int[] range = getCurrentPeriodWordRange(text, index);
		String currentWord = text.substring(range[0], range[1]);

		if (getPeriodWords().contains(currentWord)){
			//logger.debug("periodWords[] = " + periodWords.toString());
			//logger.debug("isLegalPeriodWord: currentWord = " + currentWord);
			//logger.debug(" returning " + range[1]);
			return range[1];
		}

		if (currentWord.length() == 2 && currentWord.charAt(1) == '.')
			return range[1];

		int numCount = 0;
		int periodCount = 0;
		int otherCount = 0;
		int dashCount = 0;
		for (int i = 0; i < currentWord.length(); i++) {
			char c = currentWord.charAt(i);
			if (Character.isDigit(c))
				numCount++;
			else if (c == '.')
				periodCount++;
			else if (c == '-')
				dashCount++;
			else
				otherCount++;
		}

		// if this is true, we've got an EC number
		if (otherCount == 0 && numCount > 1 && periodCount > 1)
			return range[1];

		try {
			Double.parseDouble(currentWord);
			return range[1];
		} catch (NumberFormatException ex) {
		}

		boolean isURL = false;
		try {
			new URL(currentWord);
			isURL = true;
		} catch (MalformedURLException ex) {
		}
		if (isURL) {
			return range[1];
		}
		return -1;
	}

	protected boolean doFinalPunctuationCheck(byte condition) {
		return getSentenceStructureChecks()
		&& (((AbstractCheckConfiguration) configuration)
				.getFinalPunctuationCondition() & condition) > 0;
	}

	protected boolean doRepeatedWordCheck(byte condition) {
		return (((AbstractCheckConfiguration) configuration)
				.getRepeatedWordCondition() & condition) > 0;
	}

	protected boolean doSpellCheck(byte condition) {
		//		logger.debug("doSpellCheck: condition = " + condition + ", getSpellCheckCondition = " +
		//			     ((AbstractCheckConfiguration) configuration).getSpellcheckCondition()); // DEL
		return (((AbstractCheckConfiguration) configuration)
				.getSpellcheckCondition() & condition) > 0;
	}

	protected boolean doRepeatedWhitespaceCheck(byte condition) {
		return (((AbstractCheckConfiguration) configuration)
				.getRepeatedWhitespaceCondition() & condition) > 0;
	}

	protected boolean doSentenceCaseCheck(byte condition) {
		return getSentenceStructureChecks()
		&& (((AbstractCheckConfiguration) configuration)
				.getSentenceCaseCondition() & condition) > 0;
	}

	protected boolean doSentenceSeparationCheck(byte condition) {
		return getSentenceStructureChecks()
		&& (((AbstractCheckConfiguration) configuration)
				.getSentenceSeparationCondition() & condition) > 0;
	}

	protected boolean isWord(String wordStr) {
		return wordStr.equals("a") || wordStr.equals("A")
		|| wordStr.equals("I") || wordStr.length() > 1;
	}

	protected boolean isRepeatAllowed(String word) {
		boolean allowed = getAllowedRepeats().contains(word);
		return allowed;
	}

	protected HistoryItem getFieldChangeHistoryItem(
			IdentifiedObject currentObject, String newText) {
		return null;
	}

	protected WordFinder wordFinder = new DefaultWordFinder() {
		@Override
		protected boolean isWordChar(int pos) {
			char curr = getText().charAt(pos);
			if (curr == '_')
				return false;
			else
				return super.isWordChar(pos);
		}

		@Override
		protected boolean isWordChar(char c) {
			if (c == '_')
				return false;
			else
				return super.isWordChar(c);
		}
	};

	protected static void addWarning(Collection<CheckWarning> out,
			CheckWarning w) throws TooManyWarningsException {
		//		logger.debug("out size = " + out.size() + ", addWarning " + w);  // DEL
		if ((w.isFatal() && out.size() >= VerificationManager.MAX_WARNINGS)) {
			logger.debug("Too many warnings for one term: " + out.size() + "; last warning is " + w); // DEL
			throw new TooManyWarningsException(out);
		}
		out.add(w);
	}

	protected Collection getWarnings(final FieldPath path,
			boolean allowNewlines, boolean allowBlank, boolean allowExtended,
			boolean sentenceStructureChecks, final byte condition)
	throws TooManyWarningsException {
		if (!(path.getLastValue() instanceof String))
			return new LinkedList();
		final Collection<CheckWarning> out = new LinkedList<CheckWarning>();
		try {
			final String text = path.getLastValue().toString();
			IdentifiedObject currentObject = path.getObject();

			if (doRepeatedWordCheck(condition)) {
				StringWordTokenizer tokenizer = new StringWordTokenizer(text,
						wordFinder); //tokenizer refers to the word in the array of words that is currently being looked at. 
				String last = null; //last means the previous word in the tokenizer (before the word that we are looking at.)
				int lastPos = -1;
				while (tokenizer.hasMoreWords()) {  
					final String word = tokenizer.nextWord();
					int start = tokenizer.getCurrentWordPosition();
					if (last != null) {
						if (word.equalsIgnoreCase(last)
								&& !isRepeatAllowed(word) && !tokenizer.isNewSentence()) {
							// ! Should also check whether there is punctuation between the repeated words, e.g.
							// "development. Development".
							QuickFix fixAction = new AbstractImmediateQuickFix(
									"Add \"" + word
									+ "\" to legally repeatable words") {
								public void run() {
									getAllowedRepeats().add(word);

									flushWordSets();
								}
							};
							Collection<QuickFix> fixes = new LinkedList<QuickFix>();
							fixes.add(fixAction);
							addWarning(out, new TextCheckWarning(getWarningLabel(
									path, condition)
									+ " contains the repeated "
									+ "word \""
									+ word
									+ "\".",
									//												     + " start = " + start + ", lastPos = " + lastPos, // DEL
									false, this, lastPos, start
									+ word.length(), path, fixes,
							"text:repeated_word"));
						}
					}
					last = word;
					lastPos = start;
				}
			}
			// Why not do this check while looking for repeated words?
			if (doSpellCheck(condition) && text.length() > 0) {
				//				logger.debug("Doing spell check on " + text); // DEL
				StringWordTokenizer tokenizer = new StringWordTokenizer(text, wordFinder);
				SpellCheckListener listener = new SpellCheckListener() {

					public void spellingError(final SpellCheckEvent arg0) {
						//logger.debug(arg0.getInvalidWord());
						QuickFix fixAction1 = new AbstractImmediateQuickFix(
								"Add \"" + arg0.getInvalidWord()
								+ "\" to user-defined dictionary") {

							public void run() {
								getSpellChecker().addToDictionary(
										arg0.getInvalidWord());
								// Add this word to the user defined dictionary: user.dict
								saveWord(arg0.getInvalidWord(), Preferences.getUserDefDictionaryFile());
								//refresh Text Editor to reflect changes
								Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
							}
						};

						QuickFix fixAction2 = new AbstractImmediateQuickFix(
								"Add \"" + arg0.getInvalidWord()
								+ "\" to standard system dictionary") {

							public void run() {
								getSpellChecker().addToDictionary(
										arg0.getInvalidWord());
								// Add this word to the standard dictionary: standard.dict
								saveWord(arg0.getInvalidWord(), Preferences.getStandardDictionaryFile());
//								refresh Text Editor to reflect changes
								Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
							}
						};

						Collection<QuickFix> fixes = new LinkedList<QuickFix>();
						fixes.add(fixAction1);
						fixes.add(fixAction2);
						Iterator<Word> it = arg0.getSuggestions().iterator();
						while (it.hasNext()) {
							String replacement = it.next().getWord();
							String newText = text.substring(0, arg0
									.getWordContextPosition())
									+ replacement
									+ text.substring(arg0
											.getWordContextPosition()
											+ arg0.getInvalidWord().length(),
											text.length());
							fixes.add(new ReplacementFix("Replace \""
									+ arg0.getInvalidWord() + "\" with \""
									+ replacement + "\"", path, newText));
						}

						addWarning(out, new TextCheckWarning(getWarningLabel(
								path, condition)
								+ " contains the misspelled "
								+ "word \""
								+ arg0.getInvalidWord() + "\".", false,
								AbstractTextCheck.this, arg0
								.getWordContextPosition(), arg0
								.getWordContextPosition()
								+ arg0.getInvalidWord().length(), path,
								fixes, "text:spelling_error"));
					}

				};
				// standard system dictionary spell check
				getSpellChecker().addSpellCheckListener(listener);
				getSpellChecker().checkSpelling(tokenizer);
				getSpellChecker().removeSpellCheckListener(listener);
				// user-defined dictionary spell check
				getUserDefSpellChecker().addSpellCheckListener(listener);
				getUserDefSpellChecker().checkSpelling(tokenizer);
				getUserDefSpellChecker().removeSpellCheckListener(listener);

			}
			boolean foundNoCapSentences = false;
			boolean foundNoSepSentences = false;
			boolean foundExtended = false;
			boolean foundNewlines = false;

			int repeatedWhitespaceStart = -1;
			int repeatedWhitespaceEnd = -1;

			if (!allowBlank && text.length() == 0) {
				addWarning(out, new CheckWarning(getWarningLabel(path,
						condition)
						+ " cannot be blank.", true, this, path));
				return out;
			} else if (allowBlank && text.length() == 0) {
				return out;
			}
			if (!allowExtended || !allowNewlines) {
				for (int i = 0; i < text.length(); i++) {
					char c = text.charAt(i);
					if (!allowExtended && !AbstractTextCheck.isLegal(c)
							&& !foundExtended) {
						out
						.add(new TextCheckWarning(getWarningLabel(path,
								condition)
								+ " cannot contain extended "
								+ "characters.", true, this, i, i + 1,
								path, "text:no_extended"));
						foundExtended = true;
					}
					if (!allowNewlines && c == '\n' && !foundNewlines) {
						addWarning(out, new TextCheckWarning(getWarningLabel(
								path, condition)
								+ " contains newlines.", false, this, i,
								i + 1, path, "text:no_newlines"));
						foundNewlines = true;
					}
				}
			}

			boolean foundRepeatedWhitespace = false;

			char finalChar = text.charAt(text.length() - 1);
			if (doFinalPunctuationCheck(condition)
					&& !(finalChar == '?' || finalChar == '.' || finalChar == '!')) {
				Collection<QuickFix> fixes = new ArrayList<QuickFix>();
				fixes.add(new ReplacementFix("End sentence with a period",
						path, text + "."));
				fixes.add(new ReplacementFix(
						"End sentence with a question mark", path, text + "?"));
				fixes.add(new ReplacementFix(
						"End sentence with an exclamation point", path, text
						+ "!"));
				addWarning(out, new TextCheckWarning(getWarningLabel(path,
						condition)
						+ " does not end with a period, "
						+ "question mark or exclamation point.", false, this,
						text.length() - 1, text.length(), path, fixes,
				"text:no_punctuation"));
			}
			StringBuffer word = new StringBuffer();
			StringBuffer sentence = new StringBuffer();
			for (int i = 0; i < text.length(); i++) {
				char c = text.charAt(i);
				//System.out.println("c = " + c);
				sentence.append(c);
				//System.out.println("sentence = " + sentence);
				int abbrevIndex = -1;
				if ((c == '?' || c == '!' || c == '.')&& ((abbrevIndex = isLegalPeriodWord(text, i,	currentObject)) == -1)
						&& isWord(word.toString())) {
					int[] ranges = getCurrentPeriodWordRange(text, i);
					//logger.debug("getWarnings: getCurrentPeriodWordRange(text, i)" + getCurrentPeriodWordRange(text, i));
					final String periodWord = text.substring(ranges[0],
							ranges[1]);
					//logger.debug("getWarnings: periodWord " + periodWord );
					//logger.debug("getWarnings: text.substring: " + text.substring(ranges[0], ranges[1]));
					word = new StringBuffer();
					//logger.debug("getWarnings: word = " + word );
					final String s = sentence.toString().trim();
					//logger.debug("getWarnings: s = " + s);

					if (doSentenceSeparationCheck(condition)
							&& i < text.length() - 1
							&& !Character.isWhitespace(text.charAt(i + 1))
							&& !foundNoSepSentences) {
						Collection<QuickFix> fixes = new LinkedList<QuickFix>();
						QuickFix fixAction = new AbstractImmediateQuickFix(
								"Add \"" + periodWord
								+ "\" to legal period-containing words") {
							public void run() {
								getPeriodWords().add(periodWord);
								flushWordSets();
							}
						};
						fixes.add(fixAction);
						String newText = text.substring(0, i + 1) + " "
						+ text.substring(i + 1, text.length());
						// Why would we not do this on TEXT_EDIT_COMMIT?
						if (condition != VerificationManager.TEXT_EDIT_COMMIT) {
							int firstWordIndex = i - 1;
							for (; firstWordIndex >= 0; firstWordIndex--)
								if (Character.isWhitespace(text
										.charAt(firstWordIndex)))
									break;
							if (firstWordIndex < 0)
								firstWordIndex = 0;
							String firstWord = text.substring(firstWordIndex,
									i + 1);
							int secondWordIndex = i + 1;
							for (; secondWordIndex < text.length(); secondWordIndex++)
								if (Character.isWhitespace(text
										.charAt(secondWordIndex)))
									break;
							String secondWord = text.substring(i + 1,
									secondWordIndex);
							fixes
							.add(new ReplacementFix(
									"Add the missing space between \""
									+ firstWord + "\" and \""
									+ secondWord + "\"", path,
									newText));

						}

						CheckWarning warning = new TextCheckWarning(
								getWarningLabel(path, condition)
								+ " contains sentences that are"
								+ " not separated by whitespace.",
								false, this, i - 1, i + 2, path, fixes,
						"text:joined_sentences");
						addWarning(out, warning);
						foundNoSepSentences = true;
					}
					int sentenceStartIndex = i - sentence.length() + 1;
					for (; sentenceStartIndex < text.length()
					&& Character.isWhitespace(text
							.charAt(sentenceStartIndex)); sentenceStartIndex++)
						;

					int firstSentenceWordEndIndex;
					for (firstSentenceWordEndIndex = sentenceStartIndex; firstSentenceWordEndIndex < text
					.length()
					&& !Character.isWhitespace(text
							.charAt(firstSentenceWordEndIndex)); firstSentenceWordEndIndex++)
						;

					final String firstSentenceWord = text.substring(
							sentenceStartIndex, firstSentenceWordEndIndex);
					if (doSentenceCaseCheck(condition)
							&& !getAlwaysLowercaseWords().contains(
									firstSentenceWord)
									&& !(Character.isUpperCase(s.charAt(0)) || !Character
											.isLetter(s.charAt(0)))
											&& !foundNoCapSentences) {
						QuickFix fixAction = new AbstractImmediateQuickFix(
								"Add \"" + firstSentenceWord
								+ "\" to legal always-lowercase words") {
							public void run() {
								getAlwaysLowercaseWords()
								.add(firstSentenceWord);
								flushWordSets();
							}
						};
						Collection<QuickFix> fixes = new LinkedList<QuickFix>();
						fixes.add(fixAction);
						String newText = text.substring(0, sentenceStartIndex)
						+ Character.toUpperCase(text
								.charAt(sentenceStartIndex))
								+ text.substring(sentenceStartIndex + 1, text
										.length());
						if (condition != VerificationManager.TEXT_EDIT_COMMIT) {
							fixes.add(new ReplacementFix(
									"Convert the character " + "to upper case",
									path, newText));
						}

						addWarning(out, new TextCheckWarning(getWarningLabel(
								path, condition)
								+ " contains sentences that do "
								+ "not start with a capital " + "letter.",
								false, this, sentenceStartIndex,
								sentenceStartIndex + 1, path, fixes,
						"text:no_starting_caps"));
						foundNoCapSentences = true;
					}
					sentence = new StringBuffer();
				} else if (abbrevIndex > 0) {
					word.append(text.substring(i, abbrevIndex));
					i = abbrevIndex + 1;
					/*
					 * } else if (Character.isLetterOrDigit(c) || c == ',' || c ==
					 * '+' || c == '-' || c == ':' || c == '=') {
					 */
				} else if (!Character.isWhitespace(c)) {
					word.append(c);
				} else {
					if (word.length() > 0) {

						word = new StringBuffer();
					}
				}

				if (doRepeatedWhitespaceCheck(condition)
						&& repeatedWhitespaceStart != -1
						&& !Character.isWhitespace(c)
						&& (i - repeatedWhitespaceStart > 1)) {
					repeatedWhitespaceEnd = i;
					int firstWordIndex = repeatedWhitespaceStart - 1;
					for (; firstWordIndex >= 0; firstWordIndex--)
						if (Character.isWhitespace(text.charAt(firstWordIndex)))
							break;
					String firstWord = text.substring(firstWordIndex + 1,
							repeatedWhitespaceStart);
					int secondWordIndex = repeatedWhitespaceEnd + 1;
					for (; secondWordIndex < text.length(); secondWordIndex++)
						if (Character
								.isWhitespace(text.charAt(secondWordIndex)))
							break;
					String secondWord = text.substring(repeatedWhitespaceEnd,
							secondWordIndex);

					String newText = text.substring(0, repeatedWhitespaceStart)
					+ " "
					+ text.substring(repeatedWhitespaceEnd, text
							.length());

					QuickFix fixAction = new ReplacementFix(
							"Replace repeated spaces with a single space",
							path, newText);
					Collection<QuickFix> fixes = new LinkedList<QuickFix>();
					fixes.add(fixAction);

					addWarning(out, new TextCheckWarning(getWarningLabel(path,
							condition)
							+ " contains "
							+ "several whitespace characters "
							+ "in a row between \""
							+ firstWord
							+ "\" and \""
							+ secondWord + "\"", false, this,
							repeatedWhitespaceStart, repeatedWhitespaceEnd,
							path, fixes, "text:repeated_whitespace"));
				}
				if (Character.isWhitespace(c)) {
					if (repeatedWhitespaceStart == -1)
						repeatedWhitespaceStart = i;
				} else
					repeatedWhitespaceStart = -1;
			} // end for loop

			/*
			 * words.add(word.toString()); int[] tempr = { text.length() -
			 * word.length(), text.length() }; wordRanges.add(tempr);
			 * 
			 * String lastWord = null;
			 * 
			 * Iterator it = words.iterator(); int wordIndex = 0; while
			 * (it.hasNext()) { final String wordStr = (String) it.next(); if
			 * (lastWord != null) { if (doRepeatedWordCheck(condition) &&
			 * isWord(wordStr) && wordStr.equalsIgnoreCase(lastWord) &&
			 * !isRepeatAllowed(wordStr)) { Action fixAction = new
			 * AbstractAction("Add \"" + wordStr + "\" to legally repeatable
			 * words") { public void actionPerformed(ActionEvent e) {
			 * getAllowedRepeats().add(wordStr); flushWordSets(); } };
			 * fixAction.putValue("GLOBAL", Boolean.TRUE); Collection fixes =
			 * new LinkedList(); fixes.add(fixAction); int[] r =
			 * wordRanges.get(wordIndex); out.add(new
			 * TextCheckWarning(getWarningLabel(path, condition) + " contains
			 * the repeated " + "word \"" + wordStr + "\".", false, this, r[0],
			 * r[1], path, fixes, "text:repeated_word")); } } lastWord =
			 * wordStr; wordIndex++; }
			 */
		} catch (TooManyWarningsException ex) {
			throw ex;
		}
		//		logger.debug("getWarnings: returning " + out.size() + " warnings"); // DEL
		return out;
	}

	public static boolean isLegal(char c) {
		// two special exceptions
		if (c == '\n' || c == '\t')
			return true;
		// no other iso control characters are allowed
		if (Character.isISOControl(c))
			return false;
		// only BASIC_LATIN and LATIN_1_SUPPLEMENT characters
		// are allowed
		return Character.UnicodeBlock.of(c).equals(
				Character.UnicodeBlock.BASIC_LATIN)
				|| Character.UnicodeBlock.of(c).equals(
						Character.UnicodeBlock.LATIN_1_SUPPLEMENT);
	}

	private void saveWord(String word, File dictFile) {
		if (dictFile == null || word == null || word.equals(""))
			return;
		try {
			FileWriter w = new FileWriter(dictFile.toString(), true);
			// Open with append.
			w.write(word);
			w.write("\n");
			w.close();
		} catch (IOException ex) {
			logger.info("Error writing to dictionary file");
		}
	}
}
