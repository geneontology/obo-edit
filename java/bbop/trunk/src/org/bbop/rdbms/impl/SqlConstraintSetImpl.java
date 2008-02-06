package org.bbop.rdbms.impl;

import java.util.LinkedList;

import org.bbop.rdbms.ConstraintSet;
import org.bbop.rdbms.RelationalTerm;
import org.bbop.rdbms.WhereClause;
import org.bbop.rdbms.WhereClause.BooleanOperator;

public class SqlConstraintSetImpl extends AbstractRelationalTerm implements ConstraintSet {

	//protected LinkedList<String> constraints = new LinkedList<String>();
	protected LinkedList<RelationalTerm> constraints = new LinkedList<RelationalTerm>();
	protected BooleanOperator operator = BooleanOperator.AND;

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#getConstraints()
	 */
	public LinkedList<RelationalTerm> getConstraints() {
		return constraints;
	}

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#getOperator()
	 */
	public BooleanOperator getOperator() {
		return operator;
	}

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#setOperator(org.bbop.rdbms.WhereClause.BooleanOperator)
	 */
	public void setOperator(BooleanOperator operator) {
		this.operator = operator;
	}

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#isEmpty()
	 */
	public boolean isEmpty() {
		return (getConstraints().size() == 0);
	}
	
	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#setConstraints(java.util.LinkedList)
	 */
	public void setConstraints(LinkedList<String> terms) {
		constraints = new LinkedList<RelationalTerm>();
		for (String s : terms)
			constraints.add(new SqlSimpleTerm(s));
	}

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#addConstraint(java.lang.String)
	 */
	public void addConstraint(String s) {
		this.constraints.add(new SqlSimpleTerm(s));	
	}
	
	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#addEqualityConstraint(java.lang.String, java.lang.Object)
	 */
	public void addEqualityConstraint(String s, Object o) {
		this.constraints.add(new SqlSimpleTerm(s + "=" + o));
	}

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#toSQL()
	 */
	public String toSQL() {
		StringBuffer sb = new StringBuffer();
		int n=0;
		for (RelationalTerm c : constraints) {
			if (n>0) {
				sb.append(" "+operator+" ");
			}
			n++;
			sb.append(c.toSQL());
		}
		return sb.toString();
	}

	/* (non-Javadoc)
	 * @see org.bbop.rdbms.impl.ConstraintSet#addConstraint(org.bbop.rdbms.WhereClause)
	 */
	public void addConstraint(WhereClause subClause) {
		constraints.add(subClause);
	}

}
