package owltools.phenolog;

import java.io.*;
import java.util.*;

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

    /*
     * The printhash() method takes argument as a HashSet of Type Individual
     * The method prints geneid, genelabel, and phenotype id and phenotype label
     * for each phenotype, the gene is related to.
     */
    private static void printhash(HashSet<Individual> ind){         
        Iterator it = ind.iterator();
        Iterator it_at;
        while(it.hasNext()){
            Individual tmp = (Individual)it.next();

            System.out.println("------------------------------");
            System.out.println("GENE INFO: " + tmp.getId() + " | " + tmp.getLabel());
            it_at = tmp.getAttributes().iterator();
            while(it_at.hasNext()){
                Attribute at = (Attribute)it_at.next();
                System.out.println(at.getId() + " | " + at.getLabel());
            }
                System.out.println("------------------------------");
        }
    }

    /*
     * The main() method is the first method that gets called.
     * All relevant programming blocks are either implemented inline within the method
     * or through functionality offered through methods residing within other classes.
     */
    public static void main(String[] args) {
        HashSet<GenePheno> gpset = new HashSet<GenePheno>();
        HashSet<Gene> gset = new HashSet<Gene>();
        HashMap<String, Individual> hm_ind;
        HashSet<IndividualPair> hs_indpair;
        // TODO code application logic here

        //STEP 1: Read fly_genepheno.tsv data
        try{
            File myFile = new File("fly_genepheno.tsv");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);            

            String geneid;
            String genelabel;
            String phenoid;
            String phenolabel;

            String line = null;
            while((line = reader.readLine()) != null){
                String[] result = line.split("\t");
                
                geneid = null;
                genelabel = null;
                phenoid = null;
                phenolabel = null;

                if (result.length >= 8){
                    geneid = result[0];
                    genelabel = result[1];
                    phenoid = result[6];
                    phenolabel = result[7];
                }

                if ((geneid != null) & (genelabel != null) & (phenoid != null) & (phenolabel != null)) {
                    if (phenoid.contains("FBbt")){
                        gpset.add(new GenePheno(geneid, phenoid, phenolabel));
                        gset.add(new Gene(geneid,genelabel));
                    }
                }
            }
            reader.close();

            //Output elements of hashset
            Iterator it = gpset.iterator();
            while(it.hasNext()){
                GenePheno tmp = (GenePheno)it.next();
                System.out.println(tmp.id + " " + tmp.phenoid + " " + tmp.phenolabel);
            }

        }
        catch(FileNotFoundException e) {
            System.out.println("Could not open Fly_GenePheno File");
        }
        catch(Exception ex){
            ex.printStackTrace();
        }


        // STEP2: Read through HashSet of GeneIDs read from fly_GenePheno.txt and compare with
        //        all the Gene-Phenotype associations read from HashSet

        String it_geneid;
        String it_genelabel;
        it_geneid = null;
        it_genelabel = null;

        Iterator gp_it;
        gp_it = null;

        HashSet<Attribute> gp_at;
        HashSet<Individual> ind1;

        ind1 = null;
        hm_ind = null;

        Iterator g_it = gset.iterator();
        while(g_it.hasNext()){ // For each Gene ID
            Gene tmp = (Gene)g_it.next();
            it_geneid = tmp.getid();
            it_genelabel = tmp.getlabel();

            gp_at = null;
            gp_it = gpset.iterator(); // For each Gene ID - Phenotype
            while(gp_it.hasNext()){
                GenePheno tmp2 = (GenePheno)gp_it.next();
                if (it_geneid.equals(tmp2.id)){
                    if (gp_at == null){
                        gp_at = new HashSet<Attribute>();
                    }
                    gp_at.add(new Attribute(tmp2.phenoid,tmp2.phenolabel));
                }
            }// End of Gene-Phenotype loop.

            if (ind1 == null){
                ind1 = new HashSet<Individual>();                
            }

            Individual ind = new Individual(it_geneid, it_genelabel, gp_at);
            ind1.add(ind);

            if (hm_ind == null){
                hm_ind = new HashMap<String, Individual>();
            }
            hm_ind.put(it_geneid, ind);
        }// End of Gene ID loop.


        //Output elements of Species-I Individual hashset
        printhash(ind1);




        //STEP 3: Read Mice Gene ID - Phenotype ID data
        gpset.clear();
        gset.clear();
        try{
            File myFile = new File("MGI_PhenoGenoMP.rpt");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String geneid;
            String phenoid;

            String line = null;
            while((line = reader.readLine()) != null){
                String[] result = line.split("\t");

                geneid = null;
                phenoid = null;

                if (result.length >= 6){
                    geneid = result[5];
                    phenoid = result[3];
                }

                if ((geneid != null) & (phenoid != null)) {
                    if (! (geneid.contains(","))){
                        gpset.add(new GenePheno(geneid, phenoid));
                        gset.add(new Gene(geneid));
                    }
                    
                }
            }
            reader.close();

            //Output elements of hashset
            Iterator it = gpset.iterator();
            while(it.hasNext()){
                GenePheno tmp = (GenePheno)it.next();
                System.out.println(tmp.id + " " + tmp.phenoid + " ");
            }

        }
        catch(FileNotFoundException e) {
            System.out.println("Could not open Fly_GenePheno File");
        }
        catch(Exception ex){
            ex.printStackTrace();
        }


        //STEP 4: Read Mice Gene ID - Gene Label data in a HashMap
        HashMap<String, String> hm_gene = new HashMap<String, String>();
        try{
            File myFile = new File("MGI_Coordinate.rpt");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String geneid;
            String genelabel;

            String line = null;
            while((line = reader.readLine()) != null){
                String[] result = line.split("\t");

                geneid = null;
                genelabel = null;

                if (result.length >= 3){
                    geneid = result[0];
                    genelabel = result[2];
                }

                if ((geneid != null) & (genelabel != null)) {
                    hm_gene.put(geneid, genelabel);
                }
            }
            reader.close();

            //Output elements of hashmap
            for (String s: hm_gene.keySet()){
                System.out.println(s + " : "+hm_gene.get(s));
            }
        }
        catch(FileNotFoundException e) {
            System.out.println("Could not open MGI_Coordinate.rpt File");
        }
        catch(Exception ex){
            ex.printStackTrace();
        }


        //STEP 5: Read Mice Phenotype ID - Phenotype Label data in a HashMap
        HashMap<String, String> hm_pheno = new HashMap<String, String>();
        try{
            File myFile = new File("VOC_MammalianPhenotype.rpt");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String phenoid;
            String phenolabel;

            String line = null;
            while((line = reader.readLine()) != null){
                String[] result = line.split("\t");

                phenoid = null;
                phenolabel = null;

                if (result.length >= 2){
                    phenoid = result[0];
                    phenolabel = result[1];
                }

                if ((phenoid != null) & (phenolabel != null)) {
                    hm_pheno.put(phenoid, phenolabel);
                }
            }
            reader.close();

            //Output elements of hashmap
            for (String s: hm_pheno.keySet()){
                System.out.println(s + " : "+hm_pheno.get(s));
            }
        }
        catch(FileNotFoundException e) {
            System.out.println("Could not open VOC_MammalianPhenotype.rpt File");
        }
        catch(Exception ex){
            ex.printStackTrace();
        }



        // STEP6: Read through HashSet of GeneID-Phenotype ID
        //        Assign the Gene Label and Phenotype Label to the corresponding ID
        //        Create one Individual at a time and collect it in the HashSet of Individuals.

        it_geneid = null;
        gp_it = null;

        HashSet<Individual> ind2;
        ind2 = null;

        g_it = gset.iterator();
        while(g_it.hasNext()){ // For each Gene ID
            Gene tmp = (Gene)g_it.next();
            it_geneid = tmp.getid();
            it_genelabel = hm_gene.get(it_geneid);

            gp_at = null;
            gp_it = gpset.iterator(); // For each Gene ID - Phenotype
            while(gp_it.hasNext()){
                GenePheno tmp2 = (GenePheno)gp_it.next();
                if (it_geneid.equals(tmp2.id)){
                    if (gp_at == null){
                        gp_at = new HashSet<Attribute>();
                    }
                    gp_at.add(new Attribute(tmp2.phenoid,hm_pheno.get(tmp2.phenoid)));
                }
            }// End of Gene-Phenotype loop.

            if (ind2 == null){
                ind2 = new HashSet<Individual>();
            }
            
            Individual ind = new Individual(it_geneid, it_genelabel, gp_at);
            ind2.add(ind);

            if (hm_ind == null){
                hm_ind = new HashMap<String, Individual>();
            }
            hm_ind.put(it_geneid, ind);
        }// End of Gene ID loop.


        //Output elements of Species-2 Individual hashset
        printhash(ind2);



        hs_indpair = null;
        //STEP 7: Read Ortholog Fly Gene ID - Mice Gene ID data
        try{
            File myFile = new File("Orthologs.tsv");
            FileReader fileReader = new FileReader(myFile);
            BufferedReader reader = new BufferedReader(fileReader);

            String geneid1;
            String geneid2;

            String line = null;
            while((line = reader.readLine()) != null){
                String[] result = line.split("\t");

                geneid1 = null;
                geneid2 = null;

                if (result.length >= 4){
                    geneid1 = result[0];
                    geneid2 = result[3];
                }

                if ((geneid1 != null) & (geneid2 != null)) {
                    if ((hm_ind.get(geneid1) != null) & (hm_ind.get(geneid2) != null)){
                        if (hs_indpair == null){
                            hs_indpair = new HashSet<IndividualPair>();
                        }
                        hs_indpair.add(new IndividualPair(hm_ind.get(geneid1), hm_ind.get(geneid2)));
                    }
                }
            }
            reader.close();

            // Print out Individual Ortholog Pairs
            Iterator it = hs_indpair.iterator();
            while(it.hasNext()){
                IndividualPair tmp = (IndividualPair)it.next();
                System.out.println(tmp.getMember1().getId() + " # " + tmp.getMember2().getId());
            }

        }
        catch(FileNotFoundException e) {
            System.out.println("Could not open Orthologs File");
        }
        catch(Exception ex){
            ex.printStackTrace();
        }
    }
}