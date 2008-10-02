package org.obo.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.AbstractExplanation;
import org.obo.reasoner.impl.CompletenessExplanation;
import org.obo.reasoner.impl.CompletenessMatch;
import org.obo.reasoner.impl.ExternallyImpliedExplanation;

import org.apache.log4j.*;

public class ExplanationUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExplanationUtil.class);
	private ExplanationUtil() {
	}

	public static String getDescriptionReasoned(ReasonedLinkDatabase reasoner,
			LinkDatabase activeLinkDatabase, Link link, boolean documentation) {
		return getDescriptionReasoned(reasoner, activeLinkDatabase, link, null,
				new HashMap<Link, String>(), documentation);
	}

	public static boolean isGiven(ReasonedLinkDatabase reasoner, Link link) {
		Collection explanations = reasoner.getExplanations(link);
		Iterator it2 = explanations.iterator();
		while (it2.hasNext()) {
			Explanation e = (Explanation) it2.next();
			if (e.getExplanationType() == ExplanationType.GIVEN) {
				return true;
			}
		}
		return false;
	}

	public static boolean hasBeenTrimmed(ReasonedLinkDatabase reasoner,
			LinkDatabase activeLinkDatabase, Link link) {
		return !isGiven(reasoner, link)
				&& ReasonerUtil.shouldBeTrimmedNew(activeLinkDatabase, link);
	}

	public static String getDescriptionReasoned(ReasonedLinkDatabase reasoner,
			LinkDatabase activeLinkDatabase, Link link,
			Collection subExplanations, Map<Link, String> cachedExplanations,
			boolean documentation) {
		StringBuffer out = new StringBuffer();
		if (reasoner == null) {
		    logger.info("Can't get explanation--reasoner is null!");
		    return "";
		}

		Collection explanations = reasoner.getExplanations(link);
		out.append("<a name='" + link.getChild().getID().replace(':', '_')
				+ "-" + link.getType().getID().replace(':', '_') + "-"
				+ link.getParent().getID().replace(':', '_') + "'></a>");
		out.append("<b>Why is the link "
				+ HTMLUtil.getHTMLLink(link, !hasBeenTrimmed(reasoner,
						activeLinkDatabase, link))
				+ " in the ontology?</b><p>");
		if (documentation && explanations.size() > 1) {
			out
					.append("<b>Note:</b>There are several different explanations for why "
							+ "this link appears in the ontology. This is not "
							+ "necessarily a problem; in complex ontologies with "
							+ " cross products, it's not unusual for there to be several "
							+ "ways of deriving a link. Further, just because a link has "
							+ "multiple explanations does not mean that it is redundant. "
							+ "By default the reasoner considers a link "
							+ "redundant if it can be inferred "
							+ "by the reasoner, but has been explicitly "
							+ "asserted in the ontology as well.<br>");
		}
		if (explanations.size() > 1)
			out.append("<ol>\n");
		Iterator it2 = explanations.iterator();
		while (it2.hasNext()) {
			Explanation e = (Explanation) it2.next();
			if (explanations.size() > 1) {
				out.append("<li>");
			}
			if (e.getExplanationType() == ExplanationType.INTERSECTION) {
				out.append("The reasoner created an <b>is_a</b> link between "
						+ HTMLUtil.getHTMLLink(link.getChild(), true) + " and "
						+ HTMLUtil.getHTMLLink(link.getParent(), true)
						+ " because:");
				CompletenessExplanation ce = (CompletenessExplanation) e;
				out.append("<ol>\n");
				Iterator it3 = ce.getMatches().iterator();
				while (it3.hasNext()) {
					CompletenessMatch me = (CompletenessMatch) it3.next();
					// 
					/*
					 * out.append("<li>" +
					 * (TermUtil.isImplied(me.getMatchLink()) ? "<i>" : "") +
					 * TermUtil.getHTMLLink(me.getMatchLink(),
					 * !cachedExplanations.containsKey(me .getMatchLink()) &&
					 * !subExplanations.contains(me.getMatchLink())) +
					 * (TermUtil.isImplied(me.getMatchLink()) ? "</i>" : ""));
					 */
					out.append("<li>"
							+ (TermUtil.isImplied(me.getMatchLink()) ? "<i>"
									: "")
							+ HTMLUtil.getHTMLLink(me.getMatchLink(),
									!hasBeenTrimmed(reasoner,
											activeLinkDatabase, me
													.getMatchLink()))
							+ (TermUtil.isImplied(me.getMatchLink()) ? "</i>"
									: ""));
					out.append(" ");
					if (subExplanations != null) {
						out
						.append(HTMLUtil
								.getHTMLLink(
										(subExplanations.contains(me
												.getMatchLink()) ? "HIDE"
														: "EXPAND"),
														(subExplanations.contains(me
																.getMatchLink()) ? "<font size=-1><b>(hide explanation)</b></font>"
																		: "<font size=-1><b>(show explanation)</b></font>"),
																		me.getMatchLink(), true));
					}
					if (subExplanations != null
							&& !link.equals(me.getMatchLink())
							&& subExplanations.contains(me.getMatchLink())
							&& !cachedExplanations.containsKey(me
									.getMatchLink())) {

						out.append("<ol>\n");
						out.append("<li>\n");
						String expStr = (String) cachedExplanations.get(me
								.getMatchLink());
						if (expStr == null) {
							cachedExplanations.put(me.getMatchLink(), null);
							expStr = getDescriptionReasoned(reasoner,
									activeLinkDatabase, me.getMatchLink(),
									subExplanations, cachedExplanations,
									documentation);
							cachedExplanations.put(me.getMatchLink(), expStr);
						}
						out.append(expStr);
						out.append("</ol>\n");
					}
				}
				out.append("</ol>\n");
				out
						.append(" and "
								+ HTMLUtil.getHTMLLink(link.getParent(), true)
								+ " has a cross-product definition that says any term with the following relationships:");
				out.append("<ol>\n");
				it3 = ce.getMatches().iterator();
				while (it3.hasNext()) {
					CompletenessMatch me = (CompletenessMatch) it3.next();
					out.append("<li><b>"
							+ me.getCompletenessLink().getType().getID()
							+ "</b> "
							+ HTMLUtil.getHTMLLink(me.getCompletenessLink()
									.getParent(), true));
				}
				out.append("</ol>\n");
				out.append("is an implied subclass of " + link.getParent());
			} else if (e.getExplanationType() == ExplanationType.TRANSITIVITY) {
				out
						.append("The reasoner created a <b>"
								+ link.getType().getID()
								+ "</b> link between "
								+ HTMLUtil.getHTMLLink(link.getChild(), true)
								+ " and "
								+ HTMLUtil.getHTMLLink(link.getParent(), true)
								+ " because of transitivity. This new link is the result of applying the rules of transitivity to the following links:");
				out.append("<ol>");
				Iterator it3 = ((AbstractExplanation) e).getEvidence().iterator();
				while (it3.hasNext()) {
					Link ev = (Link) it3.next();
					out.append("<li>"
							+ (TermUtil.isImplied(ev) ? "<i>" : "")
							+ HTMLUtil.getHTMLLink(ev, !hasBeenTrimmed(
									reasoner, activeLinkDatabase, ev))
							+ (TermUtil.isImplied(ev) ? "</i>" : ""));
					out.append(" ");
					if (subExplanations != null) {
						out
								.append(HTMLUtil
										.getHTMLLink(
												(subExplanations.contains(ev) ? "HIDE"
														: "EXPAND"),
												(subExplanations.contains(ev) ? "<font size=-1><b>(hide explanation)</b></font>"
														: "<font size=-1><b>(show explanation)</b></font>"),
												ev, true));
					}
					if (subExplanations != null && !ev.equals(link)
							&& subExplanations.contains(ev)
							&& !cachedExplanations.containsKey(ev)) {

						out.append("<ol>");
						out.append("<li>");
						String expStr = (String) cachedExplanations.get(ev);
						if (expStr == null) {
							cachedExplanations.put(ev, null);
							expStr = getDescriptionReasoned(reasoner,
									activeLinkDatabase, ev, subExplanations,
									cachedExplanations, documentation);
							cachedExplanations.put(ev, expStr);
						}
						out.append(expStr);
						out.append("</ol>");
					}
				}
				out.append("</ol>");
				if (documentation) {
					out
							.append("The definition of transitivity can be broken down into two major cases:");
					out.append("<ol>");
					out
							.append("<li>If <font color=blue>B -<b>p</b>-> C</font> and <font  color=blue>A -<b>is_a</b>-> B</font>, then <font  color=blue>A -<b>p</b>-> C</font>, whether or not relationship type <b>p</b> is transitive.");
					out
							.append("<li>If <b>p</b> is transitive and <font color=blue>A -<b>p</b>-> B</font> and <font color=blue>B -<b>p</b>-> C</font>, then <font color=blue>A -<b>p</b>-> C</font>.");
					out.append("</ol>");
				}
			} else if (e.getExplanationType() == ExplanationType.GENUS) {
				out.append("This reasoner created an <b>is_a</b> link between "
						+ HTMLUtil.getHTMLLink(link.getChild(), true) + " and "
						+ HTMLUtil.getHTMLLink(link.getParent(), true)
						+ " because the cross-product definition of "
						+ HTMLUtil.getHTMLLink(link.getChild(), true)
						+ " names "
						+ HTMLUtil.getHTMLLink(link.getParent(), true)
						+ " as its genus term.");
				if (documentation)
					out.append(" An <b>is_a</b> link is always implied "
							+ "between a term and its genus.");
			} else if (e.getExplanationType() == ExplanationType.DIFFERENTIA) {
				out.append("This reasoner created a <b>"
						+ link.getType().getID() + "</b> link between "
						+ HTMLUtil.getHTMLLink(link.getChild(), true) + " and "
						+ HTMLUtil.getHTMLLink(link.getParent(), true)
						+ " because the cross-product definition of "
						+ HTMLUtil.getHTMLLink(link.getChild(), true)
						+ " lists "
						+ HTMLUtil.getHTMLLink(link.getParent(), true)
						+ " as a differentia term with type <b>"
						+ link.getType().getID() + "</b>.");
				if (documentation)
					out
							.append(" A link of the specified differentia type is always implied between a term and its differentia.");
			} else if (e.getExplanationType() == ExplanationType.IMPLIED_BY_EXTERNAL_REASONER) {
				out.append("This link was computed by an external reasoner.<br>");
				if (e instanceof ExternallyImpliedExplanation) {
					String message = ((ExternallyImpliedExplanation) e).getMessage(); 
					if (message != null) {
						out.append("The external reasoner says the following about this implied link:<br><br>");
						out.append(message);
						out.append("<br><br>");
					}					
				}
				if (e.getEvidence().size() > 0) {
					out.append("The following links constitute evidence for this implied link:");
					out.append("<ul>");
					for(Link elink : e.getEvidence()) {
						out.append("<li>"+HTMLUtil.getHTMLLink(link, true));
					}
					out.append("</ul>");
				}
				if (reasoner.isRedundant(link)) {
					if (documentation)
						out
								.append("<font color=red><b>Note:</b> This link has been marked redundant by the reasoner, because the reasoner can infer the link automatically. Redundant links are not necessarily a problem, but they can make an ontology more difficult to maintain.</font>");
					else
						out
								.append("<font color=red><b>Note:</b> This link has been marked redundant by the reasoner.</font>");
				}
			} else if (e.getExplanationType() == ExplanationType.GIVEN) {
				out
						.append("This link was explicitly added to the ontology by a human being.");
				if (reasoner.isRedundant(link)) {
					if (documentation)
						out
								.append("<font color=red><b>Note:</b> This link has been marked redundant by the reasoner, because the reasoner can infer the link automatically. Redundant links are not necessarily a problem, but they can make an ontology more difficult to maintain.</font>");
					else
						out
								.append("<font color=red><b>Note:</b> This link has been marked redundant by the reasoner.</font>");
				}
			}
		}
		if (explanations.size() > 1) {
			out.append("</ol>");
		}
		return out.toString();
	}
}
