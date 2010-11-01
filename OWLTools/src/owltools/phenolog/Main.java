package owltools.phenolog;


import java.io.*;
import java.util.*;
import org.apache.commons.math.distribution.*;

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

        PhenoTransitiveClosure ptc = new PhenoTransitiveClosure();
        HashSet<Pheno> oldph1 = new HashSet<Pheno>();
        HashSet<Pheno> oldph2 = new HashSet<Pheno>();

        /* Backup Pheno hashsets for each species to be used in bootstrap later on */
        oldph1.addAll(ph1);
        oldph2.addAll(ph2);
        ph1 = ptc.performtransiviteclosure("http://purl.org/obo/obo/FBbt.obo", "FBbt:", ph1, hm1, hm_indpair1);
        ph2 = ptc.performtransiviteclosure("http://purl.org/obo/obo/MP.obo", "MP:", ph2, hm2, hm_indpair1);

        System.out.println("Done with PH2 Graph");

        System.out.println("SP1 size="+ph1.size()+" , SP2 size="+ph2.size());
        System.out.println("Total orthologs: "+hs_indpair1.size());


        //For each Phenotype in Species I
        //  For each Phenotype in Species II
        //    Calculate distance using hypergeometric probability

        double pvalue = 0;

        HypergeometricDistributionImpl hg = new HypergeometricDistributionImpl(100, 20, 10);
        IndividualPair ip1 = new IndividualPair();
        HashSet<IndividualPair> clpair = null;

        int i;
        start = System.currentTimeMillis();
        for (Pheno t_ph1 : ph1) {
            t_ph1.setClosest(null);
            t_ph1.setClosestDistance(1);

            for (Pheno t_ph2 : ph2) {
                overlap = 0;
                clpair = null;
 
                if ((t_ph1.getIndividuals() != null) && (t_ph2.getIndividuals() != null)) {
                    overlap = calculate_overlap(t_ph1, t_ph2, hm_indpair1).getClosestOverlap();
                    clpair = calculate_overlap(t_ph1, t_ph2, hm_indpair1).getClosestOverlapPairs();
                } else {
                    overlap = 0;
                }               

                if (overlap > 1) {
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

        ArrayList<Pheno> overall_list1 = new ArrayList<Pheno>(ph1);
        DistanceCompare distancecompare = new DistanceCompare();
        Collections.sort(overall_list1, distancecompare);

        try {
            Iterator it = overall_list1.iterator();
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


        ArrayList<Pheno> overall_list2 = new ArrayList<Pheno>(ph2);
        distancecompare = new DistanceCompare();
        Collections.sort(overall_list2, distancecompare);

        try {
            Iterator it = overall_list2.iterator();
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


        /* CODE FOR BOOTSTRAPING */
        /* Get list of distinct genes from Individual pair : ls_ind1 ls_ind2 for Species1 and Species2 respectively */
        Iterator it = hs_indpair1.iterator();
        ind1 = new HashSet<Individual>();
        ind2 = new HashSet<Individual>();
        Pheno p = null;
        ArrayList<Double> phenolist1 = new ArrayList<Double>();
        ArrayList<Double> phenolist2 = new ArrayList<Double>();
        double[] cutoff1 = new double[1000];
        double[] cutoff2 = new double[1000];

        while (it.hasNext()) {
            ip1 = (IndividualPair) it.next();
            ind1.add(ip1.getMember1());
            ind2.add(ip1.getMember2());
        }

        ArrayList<Individual> ls_ind1 = new ArrayList<Individual>(ind1);
        ArrayList<Individual> ls_ind2 = new ArrayList<Individual>(ind2);
        HashSet<Individual> hsg;
        
        for (int iter = 1; iter <= 1000; iter++) {

            phenolist1.clear();
            phenolist2.clear();
            //For each Phenotype in Species I
            //  For each Phenotype in Species II
            //    Calculate distance using hypergeometric probability

            hsg = null;
            pvalue = 0;

            for (Pheno t_ph1 : oldph1) {
                // Permute Genese for Species I - Phenotypes
                if (t_ph1.getIndividuals() != null) {
                    hsg = (HashSet<Individual>) getpermutedgenes(t_ph1.getIndividuals().size(), ls_ind1.size(), ls_ind1);
                    t_ph1.setIndividuals(hsg);
                }
            }
            for (Pheno t_ph2 : oldph2) {
                // Permute Genese for Species II - Phenotypes
                if (t_ph2.getIndividuals() != null) {
                    hsg = (HashSet<Individual>) getpermutedgenes(t_ph2.getIndividuals().size(), ls_ind2.size(), ls_ind2);
                    t_ph2.setIndividuals(hsg);
                }
            }

            /* Perform transitive closure on the phenotypes for Species I & II (Genes were permuted in the previous step) */
            ph1 = ptc.performtransiviteclosure("http://purl.org/obo/obo/FBbt.obo", "FBbt:", oldph1, hm1, hm_indpair1);
            ph2 = ptc.performtransiviteclosure("http://purl.org/obo/obo/MP.obo", "MP:", oldph2, hm2, hm_indpair1);

            for (Pheno t_ph1 : ph1) {
                
                t_ph1.setClosest(null);
                t_ph1.setClosestDistance(1);

                if (t_ph1.getIndividuals().size() > 0) {
                    for (Pheno t_ph2 : oldph2) {
                        overlap = 0;
                        clpair = null;
                        t_ph2.setClosest(null);
                        t_ph2.setClosestDistance(1);
                        
                        if ((t_ph1.getIndividuals() != null) && (t_ph2.getIndividuals() != null)) {
                            p = calculate_overlap(t_ph1, t_ph2, hm_indpair1);
                            overlap = p.getClosestOverlap();
                            clpair = p.getClosestOverlapPairs();
                        } else {
                            overlap = 0;
                        }

                        if (overlap > 1) {
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
                            if (pvalue < t_ph2.getClosestDistance()) {
                                t_ph2.setClosest(t_ph1);
                                t_ph2.setClosestDistance(pvalue);
                                t_ph2.setClosestOverlap(overlap);
                                t_ph2.setClosestOverlapPairs(clpair);
                            }
                        } // end of (overlap > 0)
                    } // end of looping through each phenotype of species 2
                    if(t_ph1.getClosestOverlap() > 1)
                        phenolist1.add(t_ph1.getClosestDistance());
                }

                for (Pheno t_ph2 : ph2) {
                    if (t_ph2.getClosestOverlap() > 1) {
                        phenolist2.add(t_ph2.getClosestDistance());
                    }
                }
                                
                Collections.sort(phenolist1);
                Collections.sort(phenolist2);

                cutoff1[iter] = phenolist1.get((int)Math.round(phenolist1.size() * 0.05));
                cutoff2[iter] = phenolist2.get((int)Math.round(phenolist2.size() * 0.05));
            } // end of looping through each phenotype of species 1
        } // end of iterator for 1 to 1000 bootstrap samples

        double avgcutoff1, avgcutoff2;

        avgcutoff1 = 0;
        avgcutoff2 = 0;
        for(int iter=1;iter<=1000;iter++){
            avgcutoff1 = avgcutoff1 + cutoff1[iter];
            avgcutoff2 = avgcutoff2 + cutoff2[iter];
        }
        avgcutoff1 = avgcutoff1 / 1000;
        avgcutoff2 = avgcutoff2 / 1000;


        try {
            BufferedWriter out = new BufferedWriter(new FileWriter("mresult_sp1.csv"));
            out.write("Pheno1 ID(Label),Pheno2 ID(Label), p-Value, Overlap\n");
            for (Pheno mp : overall_list1) {
                if (mp.getClosest() != null && mp.getClosestDistance() <= avgcutoff1) {
                    out.write(mp.getId() + " (" + mp.getLabel() + ") ," + mp.getClosest().getId() + " (" + mp.getClosest().getLabel() + ") ," + mp.getClosestDistance() + "," + mp.getClosestOverlap());
                    
                    out.write("\n\nGene1 ID(Label),Gene2 ID(Label)");
                    for (IndividualPair indpair : mp.getClosestOverlapPairs()) {
                        out.write("\n" + indpair.getMember1().getId() + " (" + indpair.getMember1().getLabel() + ") " + ",");
                        out.write(indpair.getMember2().getId() + " (" + indpair.getMember2().getLabel() + ") ");
                    }
                    out.write("\n\n\n\n\n");
                }
            }
            out.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        try {
            BufferedWriter out = new BufferedWriter(new FileWriter("mresult_sp2.csv"));
            out.write("Pheno1 ID(Label),Pheno2 ID(Label), p-Value, Overlap\n");
            for (Pheno mp : overall_list2) {
                if (mp.getClosest() != null && mp.getClosestDistance() <= avgcutoff1) {
                    out.write(mp.getId() + " (" + mp.getLabel() + ") ," + mp.getClosest().getId() + " (" + mp.getClosest().getLabel() + ") ," + mp.getClosestDistance() + "," + mp.getClosestOverlap());

                    out.write("\n\nGene1 ID(Label),Gene2 ID(Label)");
                    for (IndividualPair indpair : mp.getClosestOverlapPairs()) {
                        out.write("\n" + indpair.getMember1().getId() + " (" + indpair.getMember1().getLabel() + ") " + ",");
                        out.write(indpair.getMember2().getId() + " (" + indpair.getMember2().getLabel() + ") ");
                    }
                    out.write("\n\n\n\n\n");
                }
            }
            out.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    } // end of main method
} // end of main class
