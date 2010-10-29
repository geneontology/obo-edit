package owltools.phenolog;


import java.io.*;
import java.util.*;
import org.apache.commons.math.distribution.*;

import owltools.graph.OWLGraphWrapper;
import owltools.io.ParserWrapper;
import org.semanticweb.owlapi.model.OWLObject;
/**
 *
 * Author        : Neeral Beladia
 * Date Created  : September 5, 2010
 * Class Purpose : The Main Class is the kickoff Class consisting of Main() method
 * Methods       : Main()
 *                 printhash()
 * Program Logic : 1. Read fly_genepheno.tsv data
 *                 2. Read through HashSet of GeneIDs read from fly_GenePheno.txt and compare with
 *                    all the Gene-Phenotype associations read from HashSet
 *                 3. Read Mice Gene ID - Phenotype ID data
 *                 4. Read Mice Gene ID - Gene Label data in a HashMap
 *                 5. Read Mice Phenotype ID - Phenotype Label data in a HashMap
 *                 6. Read through HashSet of GeneID-Phenotype ID
 *                    Assign the Gene Label and Phenotype Label to the corresponding ID
 *                    Create one Individual at a time and collect it in the HashSet of Individuals.
 *                 7. Read Ortholog Fly Gene ID - Mice Gene ID data
 */
public class Main {

    public static Pheno calculate_overlap(Pheno p1, Pheno p2, HashMap<String, IndividualPair> hm) {
        //System.out.println("P1: "+p1.getId()+", #g = "+p1.getIndividuals().size()+", P2: "+p2.getId()+", #g = "+p2.getIndividuals().size());
        Iterator i1 = p1.getIndividuals().iterator();
        Individual g1;
        HashSet<IndividualPair> ip = new HashSet<IndividualPair>();
        Pheno p = new Pheno();
        
        int overlap = 0;
        while(i1.hasNext()){
            g1 = (Individual) i1.next();
            if (p2.getIndividuals().contains(hm.get(g1.getId()).getMember2())) {
                overlap = overlap + 1;                
                ip.add(hm.get(g1.getId()));
            }
        }
        
        p.setClosestOverlap(overlap);
        p.setClosestOverlapPairs(ip);

        return p;
    }

    public static HashSet<Individual> getpermutedgenes(int sampsz, int pop_size, ArrayList<Individual> ls_ind1) {
        HashSet<Individual> hsg = new HashSet<Individual>();
        Random rand = new Random(System.currentTimeMillis());
        int newIndex;

        while (hsg.size() <= sampsz) {
            newIndex = rand.nextInt(pop_size);
            Individual ind = (Individual) ls_ind1.get(newIndex);
            hsg.add(new Individual(ind.getId(), ind.getLabel(), ind.getOrthologs()));
        }
        return hsg;
    }


    /*
     * The main() method is the first method that gets called.
     * All relevant programming blocks are either implemented inline within the method
     * or through functionality offered through methods residing within other classes.
     */
    public static void main(String[] args) {

/**

        try {
            ParserWrapper pw = new ParserWrapper();
            OWLGraphWrapper owlg = pw.parseToOWLGraph("http://purl.org/obo/obo/MP.obo");
            Set<OWLObject> ancs = null;

            ancs = owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier("MP:0001293"));

            for (OWLObject c : ancs) {
                if (owlg.getIdentifier(c).contains("MP:")) {
                    System.out.println("ID: " + owlg.getIdentifier(c) + " ,Label: " + owlg.getLabel(c));
                }
            }



        } catch (Exception e) {
            System.out.println("EXCEPTION SP2 GRAPH");
        }

        if (1 == 1) {
            return;
        }
**/

        HashSet<GenePheno> gpset = new HashSet<GenePheno>();
        HashMap<String, Individual> hm_ind;
        HashSet<IndividualPair> hs_indpair;

        // TODO code application logic here

        //STEP 1: Read fly_genepheno.tsv data
        try {
            File myFile = new File("fly_genepheno.tsv");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String[] result;
            String line = null;
            while ((line = reader.readLine()) != null) {
                result = line.split("\t");

                // result[0] : Gene ID
                // result[1] : Gene Label
                // result[6] : Phenotype ID
                // result[7] : Phenotype Label
                if (result.length >= 8) {
                    if ((result[0] != null) && (result[1] != null) && (result[6] != null) && (result[7] != null)) {
                        if (result[6].contains("FBbt")) {
                            gpset.add(new GenePheno(result[0], result[1], result[6], result[7]));
                        }
                    }
                }
            }
            reader.close();

        } catch (FileNotFoundException e) {
            System.out.println("Could not open Fly_GenePheno File");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        
        // STEP2: Read through HashSet of GeneIDs read from fly_GenePheno.txt and compare with
        //        all the Gene-Phenotype associations read from HashSet
                
        HashSet<Attribute> gp_at;
        HashSet<Individual> ind1;
        
        ind1 = null;
        hm_ind = null;
                
        for (GenePheno tmp2_gp : gpset) {
            Individual tmp2_ind;
            if (ind1 == null){
                ind1 = new HashSet<Individual>();
                hm_ind = new HashMap<String, Individual>();
            }

            if (hm_ind.get(tmp2_gp.getid()) != null){
                tmp2_ind = (Individual)hm_ind.get(tmp2_gp.getid());
                tmp2_ind.getAttributes().add(new Attribute(tmp2_gp.getphenoid(), tmp2_gp.getphenolabel()));
            }
            else {
                gp_at = new HashSet<Attribute>();
                gp_at.add(new Attribute(tmp2_gp.getphenoid(), tmp2_gp.getphenolabel()));
                tmp2_ind = new Individual(tmp2_gp.getid(), tmp2_gp.getlabel(), gp_at);
                ind1.add(tmp2_ind);
                hm_ind.put(tmp2_gp.getid(), tmp2_ind);
            }
        }// End of Gene-Phenotype loop.

        //Output elements of Species-I Individual hashset
        //printhash(ind1);




        //STEP 3: Read Mice Gene ID - Phenotype ID data
        gpset.clear();
        try {
            File myFile = new File("MGI_PhenoGenoMP.rpt");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String[] result;

            String line = null;
            while ((line = reader.readLine()) != null) {
                result = line.split("\t");

                if (result.length >= 6) {
                    // result[5] : Gene ID
                    // result[3] : Phenotype ID
                    if ((result[5] != null) && (result[3] != null)) {
                        if (!(result[5].contains(",")) && (result[3].contains("MP:"))) {
                            gpset.add(new GenePheno(result[5], result[3]));
                        }
                    }
                }
            }
            reader.close();


        } catch (FileNotFoundException e) {
            System.out.println("Could not open MGI_PhenoGenoMP.rpt File");
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        
        //STEP 4: Read Mice Gene ID - Gene Label data in a HashMap
        HashMap<String, String> hm_gene = new HashMap<String, String>();
        try {
            File myFile = new File("MGI_Coordinate.rpt");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String[] result;

            String line = null;
            while ((line = reader.readLine()) != null) {
                result = line.split("\t");

                if (result.length >= 3) {

                    // result[0] : Gene ID
                    // result[2] : Gene Label
                    if ((result[0] != null) && (result[2] != null)) {
                        hm_gene.put(result[0], result[2]);
                    }
                }
            }
            reader.close();

            //Output elements of hashmap
            //for (String s: hm_gene.keySet()){
            //    System.out.println(s + " : "+hm_gene.get(s));
            //}
        } catch (FileNotFoundException e) {
            System.out.println("Could not open MGI_Coordinate.rpt File");
        } catch (Exception ex) {
            ex.printStackTrace();
        }


        System.out.println("HERO-2");


        //STEP 5: Read Mice Phenotype ID - Phenotype Label data in a HashMap
        HashMap<String, String> hm_pheno = new HashMap<String, String>();
        try {
            File myFile = new File("VOC_MammalianPhenotype.rpt");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String[] result;

            String line = null;
            while ((line = reader.readLine()) != null) {
                result = line.split("\t");

                if (result.length >= 2) {

                    // result[0] : Phenotype ID
                    // result[1] : Phenotype Label
                    if ((result[0] != null) && (result[1] != null)) {
                        hm_pheno.put(result[0], result[1]);
                    }
                }
            }
            reader.close();

        } catch (FileNotFoundException e) {
            System.out.println("Could not open VOC_MammalianPhenotype.rpt File");
        } catch (Exception ex) {
            ex.printStackTrace();
        }


        // STEP6: Read through HashSet of GeneID-Phenotype ID
        //        Assign the Gene Label and Phenotype Label to the corresponding ID
        //        Create one Individual at a time and collect it in the HashSet of Individuals.

        HashSet<Individual> ind2;

        ind2 = null;        
  
        for (GenePheno tmp2_gp : gpset) {
            Individual tmp2_ind;
            if (ind2 == null){
                ind2 = new HashSet<Individual>();
            }

            if (hm_ind.get(tmp2_gp.getid()) != null){
                tmp2_ind = (Individual)hm_ind.get(tmp2_gp.getid());
                tmp2_ind.getAttributes().add(new Attribute(tmp2_gp.getphenoid(), hm_pheno.get(tmp2_gp.getphenoid())));
            }
            else {
                gp_at = new HashSet<Attribute>();
                gp_at.add(new Attribute(tmp2_gp.getphenoid(), hm_pheno.get(tmp2_gp.getphenoid())));
                tmp2_ind = new Individual(tmp2_gp.getid(), hm_gene.get(tmp2_gp.getid()), gp_at);
                ind2.add(tmp2_ind);
                hm_ind.put(tmp2_gp.getid(), tmp2_ind);
            }
        }// End of Gene-Phenotype loop.


        hs_indpair = null;
        //STEP 7: Read Ortholog Fly Gene ID - Mice Gene ID data
        try {
            File myFile = new File("Orthologs.tsv");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);
            IndividualPair rd_indpair;

            String geneid1;
            String geneid2;
            String[] result;

            String line = null;
            rd_indpair = null;
            while ((line = reader.readLine()) != null) {
                result = line.split("\t");

                geneid1 = null;
                geneid2 = null;

                if (result.length >= 4 && result[3].contains("MGI:") && result[6].equals("InParanoid data set") &&
                        result[5].equals("orthologue")) {
                    geneid1 = result[0];
                    geneid2 = result[3];
                }

                if (hs_indpair == null) {
                    hs_indpair = new HashSet<IndividualPair>();
                }

                if ((geneid1 != null) && (geneid2 != null)) {
                    if ((hm_ind.get(geneid1) != null) && (hm_ind.get(geneid2) != null)) {
                        rd_indpair = new IndividualPair(hm_ind.get(geneid1), hm_ind.get(geneid2));
                        rd_indpair.getMember1().setOrthologs(rd_indpair.getMember1().getOrthologs() + 1);
                        rd_indpair.getMember2().setOrthologs(rd_indpair.getMember2().getOrthologs() + 1);
                        hs_indpair.add(rd_indpair);
                    } else if (hm_ind.get(geneid1) != null) {
                        rd_indpair = new IndividualPair(hm_ind.get(geneid1), new Individual(geneid2));
                        rd_indpair.getMember1().setOrthologs(rd_indpair.getMember1().getOrthologs() + 1);
                        rd_indpair.getMember2().setOrthologs(rd_indpair.getMember2().getOrthologs() + 1);
                        hs_indpair.add(rd_indpair);
                    } else if (hm_ind.get(geneid2) != null) {
                        rd_indpair = new IndividualPair(new Individual(geneid1), hm_ind.get(geneid2));
                        rd_indpair.getMember1().setOrthologs(rd_indpair.getMember1().getOrthologs() + 1);
                        rd_indpair.getMember2().setOrthologs(rd_indpair.getMember2().getOrthologs() + 1);
                        hs_indpair.add(rd_indpair);
                    }
                }
            }
            reader.close();

        } catch (FileNotFoundException e) {
            System.out.println("Could not open Orthologs File");
        } catch (Exception ex) {
            ex.printStackTrace();
        }


        // Loop through Ortholog pairs and create a new hashset of 1:1 individual pairs
         HashMap<String, IndividualPair> hm_indpair1 = new HashMap<String, IndividualPair>();
         HashSet<IndividualPair> hs_indpair1 = new HashSet<IndividualPair>();
         for (IndividualPair indpair : hs_indpair) {
            if ((indpair.getMember1().getOrthologs() == 1) && (indpair.getMember2().getOrthologs() == 1) &&
                    (indpair.getMember1().getAttributes().size() > 0) && (indpair.getMember2().getAttributes().size() > 0)) {
                hs_indpair1.add(indpair);
                hm_indpair1.put(indpair.getMember1().getId(), indpair);

            }
        }


        System.out.println("Size of Original Orthologs: "+hs_indpair.size());
        System.out.println("Size of New 1:1 Orthologs: "+hs_indpair1.size());
        //hs_indpair.clear();

 

        // Calculate distance between each phenotype in Species 1 vs. each phenotype in Species 2
        // To do this, create a hashset for Species I and II, Phenotype -> Set of associated Individuals

        HashSet<Pheno> ph1; // phenotypes in set 1
        HashSet<Pheno> ph2; // phenotypes in set 2
        HashMap<String, Pheno> hm1 = new HashMap<String, Pheno>();
        HashMap<String, Pheno> hm2 = new HashMap<String, Pheno>();
        HashSet<Individual> hs_ind;
        Individual g;
        Pheno ph;

        int mval = 0;
        int nval = 0;
        int bigN = 0;
        int overlap = 0;

        ph1 = null;
        ph2 = null;
        hs_ind = null;
        ph = null;

        long start, end, total;
        Individual mem1;
        Individual mem2;
                
        hs_ind = null;
        start = System.currentTimeMillis();
        for (IndividualPair itmp2 : hs_indpair1) {
             mem1 = itmp2.getMember1();
             mem2 = itmp2.getMember2();

            // Species I
             for (Attribute at : mem1.getAttributes()) {
                if (ph1 == null) {
                    ph1 = new HashSet<Pheno>();
                }
                //if Phenotype Set does not consist of the current phenotype
                //create a new <Pheno> Instance and add to the hashset.
                if (hm1.get(at.getId()) == null) {
                    hs_ind = new HashSet<Individual>();
                    hs_ind.add(new Individual(mem1.getId(), mem1.getLabel(), mem1.getOrthologs()));
                    ph = new Pheno(at.getId(), at.getLabel(), hs_ind);
                    ph1.add(ph);
                    hm1.put(at.getId(), ph);
                } //else if phonotype id already exists in the set
                else {
                    ph = hm1.get(at.getId());
                    hs_ind = (HashSet<Individual>) ph.getIndividuals();
                    g = new Individual(mem1.getId(), mem1.getLabel(), mem1.getOrthologs());                    
                    hs_ind.add(g);
                    ph.setIndividuals(hs_ind);
                }

            }//end of loop through Species-I Individual phenotypes


            // Species II
             for (Attribute at : mem2.getAttributes()) {
            	 if (ph2 == null) {
                    ph2 = new HashSet<Pheno>();
                }
                //if Phenotype Set does not consist of the current phenotype
                //create a new <Pheno> Instance and add to the hashset.
                if (hm2.get(at.getId()) == null) {
                    hs_ind = new HashSet<Individual>();
                    hs_ind.add(new Individual(mem2.getId(), mem2.getLabel(), mem2.getOrthologs()));
                    ph = new Pheno(at.getId(), at.getLabel(), hs_ind);
                    ph2.add(ph);
                    hm2.put(at.getId(), ph);
                } //else if phonotype id already exists in the set
                else {
                    ph = hm2.get(at.getId());
                    hs_ind = (HashSet<Individual>) ph.getIndividuals();
                    g = new Individual(mem2.getId(), mem2.getLabel(), mem2.getOrthologs());
                    hs_ind.add(g);
                    ph.setIndividuals(hs_ind);
                }

            }//end of loop through Species-II Individual phenotypes

        }//end of loop through Individual-Pairs

        end = System.currentTimeMillis();
        total = end - start;

        System.out.println("Total Time Taken: "+ total+" SP1 size="+ph1.size()+" , SP2 size="+ph2.size());
        System.out.println("Total orthologs: "+hs_indpair1.size());


        /*int chk_overlap = 0;
        chk_overlap = calculate_overlap(hm1.get("FBbt:00005089"), hm2.get("MP:0005621"), hm_indpair1);
        System.out.println("Overlap : "+chk_overlap);
        System.out.println("Lets check ortho. "+hm_indpair1.get("FBgn0020440"));*/


        try {
            ParserWrapper pw = new ParserWrapper();
            OWLGraphWrapper owlg = pw.parseToOWLGraph("http://purl.org/obo/obo/FBbt.obo");
            //OWLOntology ont = g.getOntology();

            OWLObject owl = null;
            Set<OWLObject> ancs = null;

            HashSet<Pheno> nph1 = (HashSet<Pheno>) ph1.clone();
            Pheno tmp = null;
            for ( Pheno p : nph1) {
                owl = owlg.getOWLObjectByIdentifier(p.getId());
                ancs = owlg.getAncestorsReflexive(owl);

                for (OWLObject c : ancs) {
                    if (owlg.getIdentifier(c).contains("FBbt:")) {
                        if (hm1.get(owlg.getIdentifier(c)) == null) {
                            tmp = new Pheno(owlg.getIdentifier(c), owlg.getLabel(c), p.getIndividuals());
                            nph1.add(tmp);
                            hm1.put(tmp.getId(), tmp);
                        } else {
                            tmp = hm1.get(owlg.getIdentifier(c));
                            tmp.getIndividuals().addAll(p.getIndividuals());
                        }
                    }
                }
            }

            ph1 = nph1;

            nph1 = null;
            nph1 = new HashSet<Pheno>();
            for ( Pheno p : ph1) {
                 if (((p.getIndividuals().size() / hm_indpair1.size()) <= 0.1) && (owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId())) != null)) {
                    nph1.add(p);
                } else {
                    if (owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId())) == null) {
                        System.out.println("SP1(Removing) No Ancestors: ID# " + p.getId() + " Label# " + p.getLabel());
                    } else {
                        System.out.println("SP1(Removing): ID# " + p.getId() + " Label# " + p.getLabel() + " Size# " + p.getIndividuals().size() + " Orthologs# " + hm_indpair1.size());
                    }
                }
            }

            ph1 = nph1;

        } catch (Exception e) {
            System.out.println("EXCEPTION SP1 GRAPH");
        }

        System.out.println("Done with PH1 Graph");

        try {
            ParserWrapper pw = new ParserWrapper();
            OWLGraphWrapper owlg = pw.parseToOWLGraph("http://purl.org/obo/obo/MP.obo");
            //OWLOntology ont = g.getOntology();

            Set<OWLObject> ancs = null;

            HashSet<Pheno> nph2 = (HashSet<Pheno>) ph2.clone();
            HashSet<Individual> tmpi = null;
            Pheno tmp = null;
            for (Pheno p : ph2) {
            	ancs = owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId()));

                for (OWLObject c : ancs) {
                    if (owlg.getIdentifier(c).contains("MP:") && !(owlg.getIdentifier(c).equals(p.getId()))) {
                    	
                       	Pheno testP = hm2.get("MP:0001293");
                       	if (hm2.get(owlg.getIdentifier(c)) == null) {
                            tmp = null;
                            tmp = new Pheno(owlg.getIdentifier(c), owlg.getLabel(c), p.getIndividuals());
                            nph2.add(tmp);
                            hm2.put(tmp.getId(), tmp);
                        } else {
                            tmp = null;
                            tmp = hm2.get(owlg.getIdentifier(c));
                          	if(owlg.getIdentifier(c).equals("MP:0000001") && p.getId().equals("MP:0001292")){
                                System.out.println("1292 and 0000001 is here :"+tmp.getId()+"size="+hm2.get("MP:0001293").getIndividuals().size());
                            }
                            tmpi = (HashSet<Individual>)tmp.getIndividuals();
                            tmpi.addAll(p.getIndividuals());
                            tmp.setIndividuals(tmpi);                            
                        }
                        System.out.println("Pheno: "+p.getId()+" ANC: "+owlg.getIdentifier(c)+" , "+hm2.get("MP:0001293").getIndividuals().size());
                    }
       //            if (p.getId().equals("MP:0001293")) {
       //                 System.out.println("After P: " + p.getIndividuals().size());
       //                 System.out.println("After P: HASHMAP: size: "+hm2.get("MP:0001293").getIndividuals().size());
       //            }
       //             if (owlg.getIdentifier(c).equals("MP:0001293")) {
       //                 tmp = hm2.get(owlg.getIdentifier(c));
       //                 System.out.println("After ANC: " + tmp.getIndividuals().size());
       //                 System.out.println("After ANC: HASHMAP: size: "+hm2.get("MP:0001293").getIndividuals().size());
       //             }
                    
                }
            }

            tmp = hm2.get("MP:0001293");
            System.out.println("FROM HASHMAP: ID: "+tmp.getId()+" ,Label: "+tmp.getLabel()+" Size: "+tmp.getIndividuals().size());

            ph2 = nph2;

            nph2 = null;
            nph2 = new HashSet<Pheno>();
            for (Pheno p : ph2) {
                if (((p.getIndividuals().size() / hm_indpair1.size()) <= 0.1) && (owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId())) != null)) {
                    nph2.add(p);
                } else {
                    if (owlg.getAncestorsReflexive(owlg.getOWLObjectByIdentifier(p.getId())) == null) {
                        System.out.println("SP2(Removing) No Ancestors: ID# " + p.getId() + " Label# " + p.getLabel());
                    } else {
                        System.out.println("SP2(Removing): ID# " + p.getId() + " Label# " + p.getLabel() + " Size# " + p.getIndividuals().size() + " Orthologs# " + hm_indpair1.size());
                    }
                }
            }

            ph2 = nph2;

        } catch (Exception e) {
            System.out.println("EXCEPTION SP2 GRAPH");
        }

        System.out.println("Done with PH2 Graph");



        if(1==1)
            return;

        System.out.println("SP1 size="+ph1.size()+" , SP2 size="+ph2.size());
        System.out.println("Total orthologs: "+hs_indpair1.size());


        //For each Phenotype in Species I
        //  For each Phenotype in Species II
        //    Calculate distance using hypergeometric probability

        Iterator sp1 = ph1.iterator();
        Iterator sp2;
        double pvalue = 0;
        Pheno t_ph1;
        Pheno t_ph2;
        HypergeometricDistributionImpl hg = new HypergeometricDistributionImpl(100, 20, 10);
        IndividualPair ip1 = new IndividualPair();
        HashSet<IndividualPair> clpair = null;

        t_ph1 = null;
        t_ph2 = null;
        int i;
        start = System.currentTimeMillis();
        while (sp1.hasNext()) {
            sp2 = ph2.iterator();
            t_ph1 = (Pheno) sp1.next();

            t_ph1.setClosest(null);
            t_ph1.setClosestDistance(1);

            while (sp2.hasNext()) {
                overlap = 0;
                clpair = null;
                t_ph2 = (Pheno) sp2.next();

                if ((t_ph1.getIndividuals() != null) && (t_ph2.getIndividuals() != null)) {
                    overlap = calculate_overlap(t_ph1, t_ph2, hm_indpair1).getClosestOverlap();
                    clpair = calculate_overlap(t_ph1, t_ph2, hm_indpair1).getClosestOverlapPairs();
                    //System.out.println("Ph1: "+t_ph1.getId()+"size="+t_ph1.getIndividuals().size()+", Ph2: "+t_ph2.getId()+"size="+t_ph2.getIndividuals().size()+" , Overlap = "+overlap);
                } else {
                    overlap = 0;
                }               

                if (overlap > 0) {
                    mval = t_ph1.getIndividuals().size();
                    nval = t_ph2.getIndividuals().size();

                    bigN = hs_indpair1.size();

                    hg.setPopulationSize(bigN);
                    hg.setNumberOfSuccesses(mval);
                    hg.setSampleSize(nval);


                    // Calculate HyperGeometric probability between t_ph1 and t_ph2
                    pvalue = 0;
                    for (i = overlap; i <= Math.min(mval, nval); i++) {
                        pvalue += hg.probability(overlap);
                    }
                    //pvalue = hg.probability(overlap);

                    if (pvalue < t_ph1.getClosestDistance()) {
                        t_ph1.setClosest(t_ph2);
                        t_ph1.setClosestDistance(pvalue);
                        t_ph1.setClosestOverlap(overlap);
                        t_ph1.setClosestOverlapPairs(clpair);
                    }
                    if (pvalue < t_ph2.getClosestDistance()) {
                        t_ph2.setClosest(t_ph1);
                        t_ph2.setClosestDistance(pvalue);
                        t_ph2.setClosestOverlap(overlap);
                        t_ph2.setClosestOverlapPairs(clpair);
                    }
                }
            }
        }

        end = System.currentTimeMillis();
        total = end - start;
        System.out.println("2) Total Time Taken: "+ total);

        ArrayList<Pheno> phenolist = new ArrayList<Pheno>(ph1);
        DistanceCompare distancecompare = new DistanceCompare();
        Collections.sort(phenolist, distancecompare);

        try {
            Iterator it = phenolist.iterator();
            Iterator pi;
            Pheno p;
            BufferedWriter out = new BufferedWriter(new FileWriter("result_sp1.csv"));
            out.write("Pheno1 ID(Label),Pheno2 ID(Label), p-Value, Overlap\n");
            while (it.hasNext()) {
                p = (Pheno) it.next();
                if (p.getClosest() != null) {
                    out.write(p.getId() + " (" + p.getLabel() + ") ," + p.getClosest().getId() + " (" + p.getClosest().getLabel() + ") ," + p.getClosestDistance() + "," + p.getClosestOverlap());
                    pi = p.getClosestOverlapPairs().iterator();
                    out.write("\n\nGene1 ID(Label),Gene2 ID(Label)");
                    while (pi.hasNext()) {
                    	IndividualPair indpair = (IndividualPair)pi.next();
                        out.write("\n"+indpair.getMember1().getId()+" ("+indpair.getMember1().getLabel()+") "+",");
                        out.write(indpair.getMember2().getId()+" ("+indpair.getMember2().getLabel()+") ");
                    }
                    out.write("\n\n\n\n\n");
                }
            }
            out.close();
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }


        phenolist = new ArrayList<Pheno>(ph2);
        distancecompare = new DistanceCompare();
        Collections.sort(phenolist, distancecompare);

        try {
            Iterator it = phenolist.iterator();
            Iterator pi;
            Pheno p;
            BufferedWriter out = new BufferedWriter(new FileWriter("result_sp2.csv"));
            out.write("Pheno1 ID(Label),Pheno2 ID(Label), p-Value, Overlap\n");
            while (it.hasNext()) {
                p = (Pheno) it.next();
                if (p.getClosest() != null) {
                    out.write(p.getId() + " (" + p.getLabel() + ") ," + p.getClosest().getId() + " (" + p.getClosest().getLabel() + ") ," + p.getClosestDistance() + "," + p.getClosestOverlap());
                    pi = p.getClosestOverlapPairs().iterator();
                    out.write("\n\nGene1 ID(Label),Gene2 ID(Label)");
                    while (pi.hasNext()) {
                    	IndividualPair indpair = (IndividualPair)pi.next();
                        out.write("\n"+indpair.getMember2().getId()+" ("+indpair.getMember2().getLabel()+") "+",");
                        out.write(indpair.getMember1().getId()+" ("+indpair.getMember1().getLabel()+") ");
                    }
                    out.write("\n\n\n\n\n");
                }
            }
            out.close();
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }

        if (1==1)
            return;


        /* CODE FOR BOOTSTRAPING */
        /* Get list of distinct genes from Individual pair : ls_ind1 ls_ind2 for Species1 and Species2 respectively */
        Iterator it = hs_indpair1.iterator();
        ind1 = new HashSet<Individual>();
        ind2 = new HashSet<Individual>();

        while (it.hasNext()) {
            ip1 = (IndividualPair) it.next();
            ind1.add(ip1.getMember1());
            ind2.add(ip1.getMember2());
        }

        ArrayList<Individual> ls_ind1 = new ArrayList<Individual>(ind1);
        ArrayList<Individual> ls_ind2 = new ArrayList<Individual>(ind2);

        for (int iter = 1; iter <= 2; iter++) {
            //For each Phenotype in Species I
            //  For each Phenotype in Species II
            //    Calculate distance using hypergeometric probability

            /* For each phenotype, permute the associated gene from the ortholog set */
            // Permute for Set 2 ;
            sp2 = ph2.iterator();
            while (sp2.hasNext()) {
                Pheno tmp2 = (Pheno) sp2.next();
                if (tmp2.getIndividuals() != null) {
                    HashSet<Individual> hsg = (HashSet<Individual>) getpermutedgenes(tmp2.getIndividuals().size(), ls_ind2.size(), ls_ind2);
                    tmp2.setIndividuals(hsg);
                }
            }

            HashSet<Individual> hsg = null;
            sp1 = ph1.iterator();
            pvalue = 0;
            int iteration = 0;
            while (sp1.hasNext()) {
                iteration++;
                sp2 = ph2.iterator();
                t_ph1 = (Pheno) sp1.next();

                t_ph1.setClosest(null);
                t_ph1.setClosestDistance(1);

                // Permute for Set 1 ;
                if (t_ph1.getIndividuals() != null) {
                    hsg = (HashSet<Individual>) getpermutedgenes(t_ph1.getIndividuals().size(), ls_ind1.size(), ls_ind1);
                    t_ph1.setIndividuals(hsg);
                }

                while (sp2.hasNext() && t_ph1.getIndividuals() != null) {
                    overlap = 0;
                    clpair = null;
                    t_ph2 = (Pheno) sp2.next();
                    if (iteration == 1) {
                        hsg = (HashSet<Individual>) getpermutedgenes(t_ph2.getIndividuals().size(), ls_ind2.size(), ls_ind2);
                        t_ph2.setIndividuals(hsg);
                    }

                    if ((t_ph1.getIndividuals() != null) && (t_ph2.getIndividuals() != null)) {
                        overlap = calculate_overlap(t_ph1, t_ph2, hm_indpair1).getClosestOverlap();
                        clpair = calculate_overlap(t_ph1, t_ph2, hm_indpair1).getClosestOverlapPairs();
                    } else {
                        overlap = 0;
                    }

                    if (overlap > 0) {
                        mval = t_ph1.getIndividuals().size();
                        nval = t_ph2.getIndividuals().size();

                        bigN = hs_indpair1.size();

                        hg.setPopulationSize(bigN);
                        hg.setNumberOfSuccesses(mval);
                        hg.setSampleSize(nval);

                        // Calculate HyperGeometric probability between t_ph1 and t_ph2
                        pvalue = 0;
                        for (i = overlap; i <= Math.min(mval, nval); i++) {
                            pvalue += hg.probability(overlap);
                        }

                        if (pvalue < t_ph1.getClosestDistance()) {
                            t_ph1.setClosest(t_ph2);
                            t_ph1.setClosestDistance(pvalue);
                            t_ph1.setClosestOverlap(overlap);
                            t_ph1.setClosestOverlapPairs(clpair);
                        }
                    } // end of (overlap > 0)
                } // end of looping through each phenotype of species 2
            } // end of looping through each phenotype of species 1


            phenolist = new ArrayList<Pheno>(ph1);
            distancecompare = new DistanceCompare();
            Collections.sort(phenolist, distancecompare);
            try {
                it = phenolist.iterator();
                BufferedWriter out = new BufferedWriter(new FileWriter("result" + Integer.toString(iter) + ".csv"));
                out.write("Pheno1 ID,Pheno1 Label, Pheno2 ID, Pheno2 Label, Distance, Overlap\n");
                while (it.hasNext()) {
                    Pheno p = (Pheno) it.next();
                    if (p.getClosest() != null) {
                        out.write(p.getId() + "," + p.getLabel() + "," + p.getClosest().getId() + "," + p.getClosest().getLabel() + "," + p.getClosestDistance() + "," + p.getClosestOverlap()+"\n");
                    }
                }
                out.close();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } // end of iterator for 1 to 1000 bootstrap samples
    } // end of main method
} // end of main clss
