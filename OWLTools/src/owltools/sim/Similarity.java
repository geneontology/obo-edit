package owltools.sim;

import java.io.PrintStream;

import org.semanticweb.owlapi.model.OWLObject;

import owltools.sim.SimEngine.SimilarityAlgorithmException;

/**
 * Represents a pairwise similarity between two OWLObjects.
 * 
 * todo: consider making a and b immuntable
 *  
 * @author cjm
 *
 */
public abstract class Similarity {
	OWLObject a;
	OWLObject b;
	Double score;
	
	public Similarity() {
		super();
	}
	
	public Double getScore() {
		return score;
	}
	public void setScore(Double score) {
		this.score = score;
	}
	public void setScore(int score) {
		this.score = (double) score;
	}
	
	
	public String toString() {
		return "S:"+score;
	}
	public void print() {
		print(System.out);
	}
	public void print(PrintStream s) {
		s.println(toString());
	}
	
	/**
	 * @param simEngine
	 * @param a
	 * @param b
	 * @throws SimilarityAlgorithmException
	 */
	public abstract void calculate(SimEngine simEngine, OWLObject a, OWLObject b) throws SimilarityAlgorithmException;
}
