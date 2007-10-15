package org.bbop.commandline;

import java.util.*;

public class CommandLineParser implements Cloneable {

	protected List parseStack = new ArrayList();
	protected List stringList = new ArrayList();
	private boolean backtracking;
	protected List backtrackStack = new ArrayList();
	// protected ArgumentSignature failFrame;
	protected List failFrames = new ArrayList();
	protected static int idgen = 0;

	public static int getID() {
		return idgen++;
	}

	public Object clone() {
		try {
			return super.clone();
		} catch (Exception ex) {
			return null;
		}
	}

	private CommandLineParser(List stringList, List parseStack, List failFrames) {
		this.stringList = new ArrayList();
		this.stringList.addAll(stringList);
		this.parseStack = new ArrayList();
		if (parseStack != null)
			this.parseStack.addAll(parseStack);
		this.failFrames = new ArrayList();
		if (failFrames != null)
			this.failFrames.addAll(failFrames);
		backtracking = false;
	}

	protected class FallbackException extends RuntimeException {
		/**
		 * 
		 */
		private static final long serialVersionUID = -3585860541324273480L;
		ArgumentSignature targetFrame;
		ArgumentSignature initiatorFrame;

		public FallbackException(ArgumentSignature targetFrame,
				ArgumentSignature initiatorFrame) {
			this.targetFrame = targetFrame;
			this.initiatorFrame = initiatorFrame;
		}

		public ArgumentSignature getInitiatorFrame() {
			return initiatorFrame;
		}

		public ArgumentSignature getTargetFrame() {
			return targetFrame;
		}
	}

	public boolean hasMoreStrings() {
		return stringList.size() > 0;
	}

	public String getNextString() throws FailException {
		if (hasMoreStrings()) {
			failFrames.clear();
			return (String) stringList.remove(0);
		} else
			throw new FailException("Unexpected end of input!");
	}

	public String peekNextString() {
		if (stringList.size() == 0)
			return null;
		else
			return (String) stringList.get(0);
	}

	public void doAccept(ArgumentSignature sig) throws FailException {
		doAccept(sig, false);
	}

	public void doAccept(ArgumentSignature sig, boolean defaultMode)
			throws FailException {
		if (failFrames.contains(sig)) {
			throw new FailException("Backtrack halted to prevent "
					+ "infinite loop");
		}

		if (defaultMode && backtracking)
			throw new FailException("Default modes disabled "
					+ "while backtracking.");

		parseStack.add(0, sig);
		List argCopy = new ArrayList();
		argCopy.addAll(stringList);

		while (true) {
			try {
				if (sig.onlyAcceptAsLastResort() && hasMoreStrings()
						&& parseStack.size() > 0) {
					List myParseStack = new ArrayList();
					List myBacktrackStack = new ArrayList();
					List myFailFrames = new ArrayList();
					myParseStack.addAll(parseStack);
					myFailFrames.add(sig);
					myParseStack.remove(0);

					while (myParseStack.size() > 0) {
						ArgumentSignature testSig = (ArgumentSignature) myParseStack
								.remove(0);
						myBacktrackStack.add(testSig);
						boolean failed;
						try {
							CommandLineParser p = new CommandLineParser(
									stringList, myParseStack, myFailFrames);
							p.backtracking = true;
							p.doAccept(testSig.copy());

							failed = false;
						} catch (FailException ex) {
							failed = true;

						}
						if (!failed) {
							throw new FallbackException(testSig, sig);
						}
						myFailFrames.add(testSig);
					}
				}
				sig.accept(this);
				parseStack.remove(0);
				break;
			} catch (FailException ex) {
				// restore arguments
				stringList.clear();
				stringList.addAll(argCopy);
				parseStack.remove(0);
				throw ex;
			} catch (FallbackException ex) {
				if (ex.getTargetFrame() != sig) {
					parseStack.remove(0);
					throw ex;
				}
			}
		}
	}

	public static ArgumentSignature getSig1() {
		OrderedArgumentSignature oArgSpec = new OrderedArgumentSignature();
		oArgSpec.addSignature(new ValueSpec());

		TagSpec oSpec = new TagSpec("-o", oArgSpec);
		oSpec.addName("-out");
		OrderedArgumentSignature nameTagSig = new OrderedArgumentSignature();
		nameTagSig.addSignature(new ValueSpec());
		nameTagSig.addSignature(oSpec);
		nameTagSig.addSignature(new ValueSpec());

		OrderedArgumentSignature sig = new OrderedArgumentSignature();
		sig.addSignature(new ValueSpec());
		sig.addSignature(new TagSpec("-name", nameTagSig));
		sig.addSignature(new TagSpec("-v", new OrderedArgumentSignature()));
		sig.addSignature(new ValueSpec());
		return sig;
	}

	public static ArgumentSignature getSig2() {
		EnumArgumentSignature enumsig = new EnumArgumentSignature();
		enumsig
				.addSignature(new TagSpec("cat", new OrderedArgumentSignature()));
		enumsig
				.addSignature(new TagSpec("dog", new OrderedArgumentSignature()));
		TagSpec animalTag = new TagSpec("-animal", enumsig);

		EnumArgumentSignature animaltags2 = new EnumArgumentSignature();
		animaltags2.addSignature(new ValueSpec("monkey"));
		animaltags2.addSignature(new ValueSpec("walrus"));
		TagSpec animalTag2 = new TagSpec("-animal", animaltags2);

		EnumArgumentSignature sig = new EnumArgumentSignature();
		sig.addSignature(animalTag);
		sig.addSignature(animalTag2);

		UnorderedArgumentSignature sig2 = new UnorderedArgumentSignature();
		sig2.addSignature(sig, 0, 2);
		return sig2;
	}

	public static ArgumentSignature getSig3() {
		TagSpec vTag = new TagSpec("-v", new OrderedArgumentSignature());
		TagSpec qTag = new TagSpec("-q", new OrderedArgumentSignature());

		UnorderedArgumentSignature longList = new UnorderedArgumentSignature();
		longList.addSignature(new ValueSpec(), 1, Integer.MAX_VALUE);

		OrderedArgumentSignature finiteList = new OrderedArgumentSignature();
		finiteList.addSignature(new ValueSpec());
		finiteList.addSignature(new ValueSpec());
		finiteList.addSignature(new ValueSpec());

		TagSpec mainTag = new TagSpec("-f", longList);
		// TagSpec f2Tag = new TagSpec("-f2", longList.copy());
		TagSpec f2Tag = new TagSpec("-f2", finiteList.copy());
		// TagSpec mainTag = new TagSpec("-f", finiteList);

		UnorderedArgumentSignature mainList = new UnorderedArgumentSignature();
		mainList.addSignature(vTag, 0, Integer.MAX_VALUE);
		mainList.addSignature(qTag, 0, Integer.MAX_VALUE);
		mainList.addSignature(mainTag, 0, 1);
		mainList.addSignature(f2Tag, 0, Integer.MAX_VALUE);

		return mainList;
	}

	public static ArgumentSignature getSig4() {

		UnorderedArgumentSignature longList = new UnorderedArgumentSignature();
		longList.addSignature(new ValueSpec(".*\\..*", true), 1,
				Integer.MAX_VALUE);

		UnorderedArgumentSignature oneOrZero = new UnorderedArgumentSignature();
		oneOrZero.addSignature(new ValueSpec(".*\\..*", true), 0, 1);

		UnorderedArgumentSignature flatOptions = new UnorderedArgumentSignature(
				"flat file adapter settings");
		flatOptions.addSignature(new TagSpec("-dangling",
				new OrderedArgumentSignature()), 0, 1);
		flatOptions.addSignature(new TagSpec("-f", longList), 1, 2);

		EnumArgumentSignature adapterNames = new EnumArgumentSignature();
		adapterNames.addSignature(new TagSpec("FLAT", flatOptions), true);
		adapterNames.addSignature(new TagSpec("OBO",
				new OrderedArgumentSignature()));
		adapterNames.addSignature(new TagSpec("SERIAL",
				new OrderedArgumentSignature()));

		TagSpec loadTag = new TagSpec("-load", adapterNames);

		UnorderedArgumentSignature main = new UnorderedArgumentSignature();
		main.addSignature(loadTag, 0, 1, true);
		return main;
	}

	public static void main(String[] args) throws Exception {

		ArgumentSignature sig;
		boolean foundIndicator = false;
		if (args[0].equals("1")) {
			sig = getSig1();
			foundIndicator = true;
		} else if (args[0].equals("2")) {
			sig = getSig2();
			foundIndicator = true;
		} else if (args[0].equals("3")) {
			sig = getSig3();
			foundIndicator = true;
		} else if (args[0].equals("4")) {
			sig = getSig4();
			foundIndicator = true;
		} else
			sig = getSig4();

		List argList = new ArrayList();
		for (int i = (foundIndicator ? 1 : 0); i < args.length; i++)
			argList.add(args[i]);

	}
	
	public static String getUsageString(ArgumentSignature as) {
		return as.getShortDocumentation();
	}

	public static List parse(ArgumentSignature as, String[] args)
			throws UnfullfilledException, FailException {
		List argList = new ArrayList();
		for (int i = 0; i < args.length; i++)
			argList.add(args[i]);
		return parse(as, argList);
	}

	public static List parse(ArgumentSignature as, List args)
			throws UnfullfilledException, FailException {
		CommandLineParser p = new CommandLineParser(args, null, null);
		p.doAccept(as);
		if (p.hasMoreStrings())
			throw new FailException("Unexpected values " + p.stringList
					+ " found");
		return as.getValues();
	}
}
