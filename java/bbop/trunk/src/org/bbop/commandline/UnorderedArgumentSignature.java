package org.bbop.commandline;

import java.util.*;

public class UnorderedArgumentSignature implements ArgumentSignature {
	protected class CardinalityConstraint {
		int minCardinality;
		int maxCardinality;
		ArgumentSignature sig;
		int index;
		boolean canDefault;

		public boolean isSatisfied() {
			return index >= minCardinality && index <= maxCardinality;
		}

		public boolean isFull() {
			return index >= maxCardinality;
		}

		public void init(CommandLineParser p, boolean defaultMode) {
			index = 0;
			sig.init(p, defaultMode);
		}

		public CardinalityConstraint copy() {
			CardinalityConstraint out = new CardinalityConstraint(sig.copy(),
					minCardinality, maxCardinality, canDefault);
			out.index = index;
			return out;
		}

		public CardinalityConstraint(ArgumentSignature sig, int minCard,
				int maxCard, boolean canDefault) {
			this.sig = sig;
			this.maxCardinality = maxCard;
			this.minCardinality = minCard;
			this.canDefault = canDefault;
			index = 0;
		}

		public boolean getCanDefault() {
			return canDefault;
		}

		public String toString() {
			return sig + ":" + index;
		}

	}

	protected List signatures = new ArrayList();
	protected List usedSigs = new ArrayList();
	protected String shortDesc;
	protected int id = CommandLineParser.getID();

	public int getID() {
		return id;
	}

	public boolean equals(Object o) {
		if (o instanceof ArgumentSignature)
			return ((ArgumentSignature) o).getID() == id;
		else
			return false;
	}

	public int hashCode() {
		return id;
	}

	public boolean onlyAcceptAsLastResort() {
		return false;
	}

	public String toString() {
		if (shortDesc != null)
			return shortDesc;
		return super.toString();
	}

	public void init(CommandLineParser p, boolean defaultMode) {
		Iterator it = signatures.iterator();
		while (it.hasNext()) {
			CardinalityConstraint sig = (CardinalityConstraint) it.next();
			sig.init(p, false);
		}

		usedSigs.clear();

	}

	public void setOnlyAcceptAsLastResort(boolean lastResort) {
	}

	public void addSignature(ArgumentSignature sig) {
		addSignature(sig, 0, 1, false);
	}

	public void addSignature(ArgumentSignature sig, int minCardinality,
			int maxCardinality) {
		addSignature(sig, minCardinality, maxCardinality, false);
	}

	public void addSignature(ArgumentSignature sig, int minCardinality,
			int maxCardinality, boolean canDefault) {
		signatures.add(new CardinalityConstraint(sig, minCardinality,
				maxCardinality, canDefault));
	}

	public UnorderedArgumentSignature() {
		this(null);
	}

	public UnorderedArgumentSignature(String shortDesc) {
		this.shortDesc = shortDesc;
	}

	protected String formatStringList(String str, int startindex, int endindex,
			boolean mandatory, boolean nospacedemo) {
		if (startindex == endindex)
			return "";
		StringBuffer out = new StringBuffer();
		if (!mandatory)
			out.append("[");
		if (endindex - startindex == 1)
			out.append(str);
		else if (endindex - startindex < 3) {
			boolean first = true;
			for (; startindex < endindex; startindex++) {
				if (!first)
					out.append(" ");
				out.append(str + "<"+(startindex + 1)+">");
				first = false;
			}
		} else {
			out.append(str + (startindex + 1));
			if (!nospacedemo) {
				out.append(" " + str + "<"+(startindex + 2)+">");
			}
			String lastNum = "N";
			if (endindex < Integer.MAX_VALUE)
				lastNum = "" + (endindex + 1);
			out.append(" ... " + str + "<"+lastNum+">");
		}
		if (!mandatory)
			out.append("]");
		return out.toString();
	}

	public String getShortDocumentation() {
		if (shortDesc == null) {
			StringBuffer out = new StringBuffer();
			Iterator it = signatures.iterator();
			boolean first = true;
			while (it.hasNext()) {
				if (!first)
					out.append(" ");
				first = false;
				CardinalityConstraint cc = (CardinalityConstraint) it.next();
				String s = cc.sig.getShortDocumentation();
				String firstSection = formatStringList(s, 0, cc.minCardinality,
						true, false);
				String secondSection = formatStringList(s, cc.minCardinality,
						cc.maxCardinality, false, cc.minCardinality > 1);
				if (firstSection.length() > 0 && secondSection.length() > 0)
					out.append(firstSection + " " + secondSection);
				else
					out.append(firstSection + secondSection);
			}
			return out.toString();
		} else
			return "<" + shortDesc + ">";
	}

	public ArgumentSignature copy() {
		UnorderedArgumentSignature out = new UnorderedArgumentSignature();
		//	out.signatures.addAll(signatures);

		Iterator it = signatures.iterator();
		while (it.hasNext()) {
			CardinalityConstraint sig = (CardinalityConstraint) it.next();
			out.signatures.add(sig.copy());
		}
		out.shortDesc = shortDesc;
		out.usedSigs.addAll(usedSigs);
		out.id = id;
		return out;
	}

	public void accept(CommandLineParser p) throws FailException {
		while (true) {
			boolean stillWork = false;
			boolean gotSuccess = false;
			/*
			 boolean empty = !p.hasMoreStrings();
			 if (empty) {
			 System.err.println("calling uas.accept() with strings "+p.stringList);
			 throw new FailException(false);
			 }
			 */
			for (int attempts = 0; attempts < 2; attempts++) {
				for (int i = 0; i < signatures.size(); ) {
					CardinalityConstraint c = (CardinalityConstraint) signatures
							.get(i);
					if (c.isFull()) {
						i++;
						continue;
					} else
						stillWork = true;

					if (attempts > 0 && !c.getCanDefault()) {
						i++;
						continue;
					}
					
					ArgumentSignature sig = c.sig;

					sig.init(p, attempts > 0);
					try {

						try {
							p.doAccept(sig, attempts > 0);
							ArgumentSignature copySig = sig.copy();
							usedSigs.add(copySig);
							c.index++;
						} catch (CommandLineParser.FallbackException ex) {
							if (sig != ex.getInitiatorFrame()) {
								ArgumentSignature copySig = sig.copy();
								usedSigs.add(copySig);
								c.index++;
							}
							throw ex;
						}
						
						gotSuccess = true;
					} catch (FailException ex) {
						i++;
					}
				}
				if (gotSuccess)
					break;
			}
			if (!stillWork)
				return;
			if (!gotSuccess) {
				for (int i = 0; i < signatures.size(); i++) {
					CardinalityConstraint c = (CardinalityConstraint) signatures
							.get(i);
					if (!c.isSatisfied()) {
						throw new FailException("Not enough values found for "
								+ c);
					}
				}
				if (p.hasMoreStrings()) {
					Iterator it = signatures.iterator();
					while (it.hasNext()) {
						CardinalityConstraint cc = (CardinalityConstraint) it
								.next();
						cc.isFull();
					}
					throw new FailException("Unexpected values found: "
							+ p.stringList);
				} else {
					return;
				}
			}
		}
	}

	/*
	 protected ArgumentSignature doExit(CommandLineParser p) {

	 }
	 */
	public List getValues() throws UnfullfilledException {
		List out = new ArrayList();
		for (int i = 0; i < signatures.size(); i++) {
			CardinalityConstraint c = (CardinalityConstraint) signatures.get(i);
			/*
			 if (!c.isSatisfied())
			 throw new UnfullfilledException();
			 */
		}
		Iterator it = usedSigs.iterator();
		while (it.hasNext()) {
			ArgumentSignature as = (ArgumentSignature) it.next();
			out.addAll(as.getValues());
		}
		return out;
	}
}
