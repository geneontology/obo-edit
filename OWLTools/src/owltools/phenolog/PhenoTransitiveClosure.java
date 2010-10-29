/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package owltools.phenolog;

import java.util.*;
import owltools.graph.OWLGraphWrapper;
import owltools.io.ParserWrapper;
import org.semanticweb.owlapi.model.OWLObject;

/**
 *
 * @author beladia
 */
public class PhenoTransitiveClosure {
    private String obolink;
    private String prefix;
    private HashSet<Pheno> hsp;
    private HashMap<String, Pheno> hmp;
    private HashMap<String, IndividualPair> hmpair;

    public String getobolink(){
        return this.obolink;
    }
    public String getprefix(){
        return this.prefix;
    }
    public HashSet<Pheno> gethsp(){
        return this.hsp;
    }
    public HashMap<String, Pheno> gethmp(){
        return this.hmp;
    }
    public HashMap<String, IndividualPair> gethmpair(){
        return this.hmpair;
    }

    public void setobolink(String obolink){
        this.obolink = obolink;
    }
    public void setprefix(String prefix){
        this.prefix = prefix;
    }
    public void sethsp(HashSet<Pheno> hsp){
        this.hsp = hsp;
    }
    public void sethmp(HashMap<String, Pheno> hmp){
        this.hmp = hmp;
    }
    public void sethmpair(HashMap<String, IndividualPair> hmpair){
        this.hmpair = hmpair;
    }

    private HashSet<Pheno> ptc(String obolink, String prefix, HashSet<Pheno> hsp, HashMap<String, Pheno> hmp, HashMap<String, IndividualPair> hmpair) {
        try {
            ParserWrapper pw = new ParserWrapper();
            OWLGraphWrapper owlg = pw.parseToOWLGraph(obolink);

            Set<OWLObject> ancs = null;

            HashSet<Pheno> chsp = (HashSet<Pheno>) hsp.clone();
            HashSet<Individual> tmpi = null;
            Pheno tmp = null;
            for (Pheno p : hsp) {
                ancs = owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId()));

                for (OWLObject c : ancs) {
                    if (owlg.getIdentifier(c).contains(prefix) && !(owlg.getIdentifier(c).equals(p.getId()))) {
                        if (hmp.get(owlg.getIdentifier(c)) == null) {
                            tmpi = new HashSet<Individual>();
                            tmpi.addAll(p.getIndividuals());
                            tmp = new Pheno(owlg.getIdentifier(c), owlg.getLabel(c), tmpi);
                            chsp.add(tmp);
                            hmp.put(tmp.getId(), tmp);
                        } else {
                            tmp = null;
                            tmp = hmp.get(owlg.getIdentifier(c));
                            tmpi = (HashSet<Individual>) tmp.getIndividuals();
                            tmpi.addAll(p.getIndividuals());
                            tmp.setIndividuals(tmpi);
                        }
                    }
                }
            }

            hsp = chsp;

            chsp = new HashSet<Pheno>();
            for (Pheno p : hsp) {
                if (((p.getIndividuals().size() / hmpair.size()) <= 0.1) && (owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId())).size() > 1)) {
                    chsp.add(p);
                } else {
                    if (owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId())).size() == 1) {
                        System.out.println("Removing No Ancestors: ID# " + p.getId() + " Label# " + p.getLabel());
                    } else {
                        System.out.println("Removing: ID# " + p.getId() + " Label# " + p.getLabel() + " Size# " + p.getIndividuals().size() + " Orthologs# " + hmpair.size());
                    }
                }
            }

            hsp = chsp;                        
        } catch (Exception e) {
            System.out.println("EXCEPTION IN OWL GRAPH");
        }

        return hsp;
    }

    public HashSet<Pheno> performtransiviteclosure(){
        return ptc(this.obolink, this.prefix, this.hsp, this.hmp, this.hmpair);
    }
    public HashSet<Pheno> performtransiviteclosure(String obolink, String prefix, HashSet<Pheno> hsp, HashMap<String, Pheno> hmp, HashMap<String, IndividualPair> hmpair){
        return ptc(obolink, prefix, hsp, hmp, hmpair);
    }
}