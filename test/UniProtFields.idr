-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module UniprotFields

import Database.UniProt.Types
import Database.UniProt.Parser

-- %default total
%access private

%flag C "-O3"
%flag C "-g"

--}

----------------------------------------------------------------------------------------[ Entry 1 ]
--{1

entry1 : String
entry1 = """ID   PAP_YEAST               Reviewed;         568 AA.
AC   P29468; D6VXT8;
DT   01-APR-1993, integrated into UniProtKB/Swiss-Prot.
DT   01-APR-1993, sequence version 1.
DT   16-JAN-2019, entry version 179.
DE   RecName: Full=Poly(A) polymerase;
DE            Short=PAP;
DE            EC=2.7.7.19;
DE   AltName: Full=Polynucleotide adenylyltransferase;
GN   Name=PAP1; OrderedLocusNames=YKR002W;
OS   Saccharomyces cerevisiae (strain ATCC 204508 / S288c) (Baker's yeast).
OC   Eukaryota; Fungi; Dikarya; Ascomycota; Saccharomycotina;
OC   Saccharomycetes; Saccharomycetales; Saccharomycetaceae; Saccharomyces.
OX   NCBI_TaxID=559292;
RN   [1]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA], AND PARTIAL PROTEIN SEQUENCE.
RC   STRAIN=ATCC 204510 / AB320;
RX   PubMed=1840648; DOI=10.1038/354496a0;
RA   Lingner J., Kellermann J., Keller W.;
RT   "Cloning and expression of the essential gene for poly(A) polymerase
RT   from S. cerevisiae.";
RL   Nature 354:496-498(1991).
RN   [2]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA].
RC   STRAIN=ATCC 204508 / S288c;
RX   PubMed=1441752; DOI=10.1002/yea.320080908;
RA   Duesterhoeft A., Philippsen P.;
RT   "DNA sequencing and analysis of a 24.7 kb segment encompassing
RT   centromere CEN11 of Saccharomyces cerevisiae reveals nine previously
RT   unknown open reading frames.";
RL   Yeast 8:749-759(1992).
RN   [3]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE GENOMIC DNA].
RC   STRAIN=ATCC 204508 / S288c;
RX   PubMed=8196765; DOI=10.1038/369371a0;
RA   Dujon B., Alexandraki D., Andre B., Ansorge W., Baladron V.,
RA   Ballesta J.P.G., Banrevi A., Bolle P.-A., Bolotin-Fukuhara M.,
RA   Bossier P., Bou G., Boyer J., Buitrago M.J., Cheret G., Colleaux L.,
RA   Daignan-Fornier B., del Rey F., Dion C., Domdey H., Duesterhoeft A.,
RA   Duesterhus S., Entian K.-D., Erfle H., Esteban P.F., Feldmann H.,
RA   Fernandes L., Fobo G.M., Fritz C., Fukuhara H., Gabel C., Gaillon L.,
RA   Garcia-Cantalejo J.M., Garcia-Ramirez J.J., Gent M.E., Ghazvini M.,
RA   Goffeau A., Gonzalez A., Grothues D., Guerreiro P., Hegemann J.H.,
RA   Hewitt N., Hilger F., Hollenberg C.P., Horaitis O., Indge K.J.,
RA   Jacquier A., James C.M., Jauniaux J.-C., Jimenez A., Keuchel H.,
RA   Kirchrath L., Kleine K., Koetter P., Legrain P., Liebl S., Louis E.J.,
RA   Maia e Silva A., Marck C., Monnier A.-L., Moestl D., Mueller S.,
RA   Obermaier B., Oliver S.G., Pallier C., Pascolo S., Pfeiffer F.,
RA   Philippsen P., Planta R.J., Pohl F.M., Pohl T.M., Poehlmann R.,
RA   Portetelle D., Purnelle B., Puzos V., Ramezani Rad M., Rasmussen S.W.,
RA   Remacha M.A., Revuelta J.L., Richard G.-F., Rieger M.,
RA   Rodrigues-Pousada C., Rose M., Rupp T., Santos M.A., Schwager C.,
RA   Sensen C., Skala J., Soares H., Sor F., Stegemann J., Tettelin H.,
RA   Thierry A., Tzermia M., Urrestarazu L.A., van Dyck L.,
RA   van Vliet-Reedijk J.C., Valens M., Vandenbol M., Vilela C.,
RA   Vissers S., von Wettstein D., Voss H., Wiemann S., Xu G.,
RA   Zimmermann J., Haasemann M., Becker I., Mewes H.-W.;
RT   "Complete DNA sequence of yeast chromosome XI.";
RL   Nature 369:371-378(1994).
RN   [4]
RP   GENOME REANNOTATION.
RC   STRAIN=ATCC 204508 / S288c;
RX   PubMed=24374639; DOI=10.1534/g3.113.008995;
RA   Engel S.R., Dietrich F.S., Fisk D.G., Binkley G., Balakrishnan R.,
RA   Costanzo M.C., Dwight S.S., Hitz B.C., Karra K., Nash R.S., Weng S.,
RA   Wong E.D., Lloyd P., Skrzypek M.S., Miyasato S.R., Simison M.,
RA   Cherry J.M.;
RT   "The reference genome sequence of Saccharomyces cerevisiae: Then and
RT   now.";
RL   G3 (Bethesda) 4:389-398(2014).
RN   [5]
RP   INTERACTION WITH FIR1.
RX   PubMed=9236779; DOI=10.1007/s004380050491;
RA   del Olmo M., Mizrahi N., Gross S., Moore C.L.;
RT   "The Uba2 and Ufd1 proteins of Saccharomyces cerevisiae interact with
RT   poly(A) polymerase and affect the polyadenylation activity of cell
RT   extracts.";
RL   Mol. Gen. Genet. 255:209-218(1997).
RN   [6]
RP   INTERACTION WITH RRP6.
RX   PubMed=10611239; DOI=10.1128/MCB.20.2.604-616.2000;
RA   Burkard K.T.D., Butler J.S.;
RT   "A nuclear 3'-5' exonuclease involved in mRNA degradation interacts
RT   with Poly(A) polymerase and the hnRNA protein Npl3p.";
RL   Mol. Cell. Biol. 20:604-616(2000).
RN   [7]
RP   IDENTIFICATION IN THE CPF COMPLEX, SUBCELLULAR LOCATION, AND
RP   IDENTIFICATION BY MASS SPECTROMETRY.
RX   PubMed=12819204; DOI=10.1074/jbc.M304454200;
RA   Nedea E., He X., Kim M., Pootoolal J., Zhong G., Canadien V.,
RA   Hughes T., Buratowski S., Moore C.L., Greenblatt J.;
RT   "Organization and function of APT, a subcomplex of the yeast cleavage
RT   and polyadenylation factor involved in the formation of mRNA and small
RT   nucleolar RNA 3'-ends.";
RL   J. Biol. Chem. 278:33000-33010(2003).
RN   [8]
RP   SUBCELLULAR LOCATION [LARGE SCALE ANALYSIS].
RX   PubMed=14562095; DOI=10.1038/nature02026;
RA   Huh W.-K., Falvo J.V., Gerke L.C., Carroll A.S., Howson R.W.,
RA   Weissman J.S., O'Shea E.K.;
RT   "Global analysis of protein localization in budding yeast.";
RL   Nature 425:686-691(2003).
RN   [9]
RP   LEVEL OF PROTEIN EXPRESSION [LARGE SCALE ANALYSIS].
RX   PubMed=14562106; DOI=10.1038/nature02046;
RA   Ghaemmaghami S., Huh W.-K., Bower K., Howson R.W., Belle A.,
RA   Dephoure N., O'Shea E.K., Weissman J.S.;
RT   "Global analysis of protein expression in yeast.";
RL   Nature 425:737-741(2003).
RN   [10]
RP   PHOSPHORYLATION [LARGE SCALE ANALYSIS] AT SER-452, AND IDENTIFICATION
RP   BY MASS SPECTROMETRY [LARGE SCALE ANALYSIS].
RX   PubMed=18407956; DOI=10.1074/mcp.M700468-MCP200;
RA   Albuquerque C.P., Smolka M.B., Payne S.H., Bafna V., Eng J., Zhou H.;
RT   "A multidimensional chromatography technology for in-depth
RT   phosphoproteome analysis.";
RL   Mol. Cell. Proteomics 7:1389-1396(2008).
RN   [11]
RP   PHOSPHORYLATION [LARGE SCALE ANALYSIS] AT SER-550, AND IDENTIFICATION
RP   BY MASS SPECTROMETRY [LARGE SCALE ANALYSIS].
RX   PubMed=19779198; DOI=10.1126/science.1172867;
RA   Holt L.J., Tuch B.B., Villen J., Johnson A.D., Gygi S.P., Morgan D.O.;
RT   "Global analysis of Cdk1 substrate phosphorylation sites provides
RT   insights into evolution.";
RL   Science 325:1682-1686(2009).
RN   [12]
RP   X-RAY CRYSTALLOGRAPHY (2.6 ANGSTROMS) OF 1-537 IN COMPLEX WITH ATP
RP   ANALOG AND MANGANESE IONS.
RX   PubMed=10958780; DOI=10.1126/science.289.5483.1346;
RA   Bard J., Zhelkovsky A.M., Helmling S., Earnest T.N., Moore C.L.,
RA   Bohm A.;
RT   "Structure of yeast poly(A) polymerase alone and in complex with 3'-
RT   dATP.";
RL   Science 289:1346-1349(2000).
RN   [13]
RP   X-RAY CRYSTALLOGRAPHY (1.8 ANGSTROMS) OF 1-530.
RX   PubMed=17223131; DOI=10.1016/j.jmb.2006.12.030;
RA   Balbo P.B., Toth J., Bohm A.;
RT   "X-ray crystallographic and steady state fluorescence characterization
RT   of the protein dynamics of yeast polyadenylate polymerase.";
RL   J. Mol. Biol. 366:1401-1415(2007).
RN   [14]
RP   X-RAY CRYSTALLOGRAPHY (1.8 ANGSTROMS) OF 5-529 OF MUTANT ALA-154 IN
RP   COMPLEX WITH ATP; MAGNESIUM IONS AND OLIGONUCLEOTIDE, CATALYTIC
RP   ACTIVITY, FUNCTION, COFACTOR, AND MUTAGENESIS OF ASP-154; ASN-189;
RP   LYS-215 AND ASN-226.
RX   PubMed=17850751; DOI=10.1016/j.str.2007.07.010;
RA   Balbo P.B., Bohm A.;
RT   "Mechanism of poly(A) polymerase: structure of the enzyme-MgATP-RNA
RT   ternary complex and kinetic analysis.";
RL   Structure 15:1117-1131(2007).
RN   [15]
RP   X-RAY CRYSTALLOGRAPHY (2.6 ANGSTROMS) OF 1-537 IN COMPLEX WITH FIP1,
RP   FUNCTION, CATALYTIC ACTIVITY, INTERACTION WITH FIP1, AND MUTAGENESIS
RP   OF CYS-485 AND VAL-489.
RX   PubMed=18537269; DOI=10.1021/bi800204k;
RA   Meinke G., Ezeokonkwo C., Balbo P., Stafford W., Moore C., Bohm A.;
RT   "Structure of yeast poly(A) polymerase in complex with a peptide from
RT   Fip1, an intrinsically disordered protein.";
RL   Biochemistry 47:6859-6869(2008).
CC   -!- FUNCTION: Polymerase component of the cleavage and polyadenylation
CC       factor (CPF) complex, which plays a key role in polyadenylation-
CC       dependent pre-mRNA 3'-end formation and cooperates with cleavage
CC       factors including the CFIA complex and NAB4/CFIB.
CC       {ECO:0000269|PubMed:17850751, ECO:0000269|PubMed:18537269}.
CC   -!- CATALYTIC ACTIVITY:
CC       Reaction=ATP + RNA(n) = diphosphate + RNA(n)-3'-adenine
CC         ribonucleotide; Xref=Rhea:RHEA:11332, Rhea:RHEA-COMP:11128,
CC         Rhea:RHEA-COMP:14647, ChEBI:CHEBI:30616, ChEBI:CHEBI:33019,
CC         ChEBI:CHEBI:83400, ChEBI:CHEBI:140626; EC=2.7.7.19;
CC         Evidence={ECO:0000269|PubMed:17850751,
CC         ECO:0000269|PubMed:18537269};
CC   -!- COFACTOR:
CC       Name=Mg(2+); Xref=ChEBI:CHEBI:18420;
CC         Evidence={ECO:0000269|PubMed:17850751};
CC       Name=Mn(2+); Xref=ChEBI:CHEBI:29035;
CC         Evidence={ECO:0000269|PubMed:17850751};
CC       Note=Binds 2 magnesium ions. Also active with manganese.
CC       {ECO:0000269|PubMed:17850751};
CC   -!- SUBUNIT: Component of the cleavage and polyadenylation factor
CC       (CPF) complex, which is composed of PTI1, SYC1, SSU72, GLC7, MPE1,
CC       REF2, PFS2, PTA1, YSH1/BRR5, SWD2, CFT2/YDH1, YTH1, CFT1/YHH1,
CC       FIP1 and PAP1. Interacts with FIR1 and RRP6.
CC       {ECO:0000269|PubMed:10611239, ECO:0000269|PubMed:10958780,
CC       ECO:0000269|PubMed:12819204, ECO:0000269|PubMed:17850751,
CC       ECO:0000269|PubMed:18537269, ECO:0000269|PubMed:9236779}.
CC   -!- INTERACTION:
CC       Q06632:CFT1; NbExp=5; IntAct=EBI-12917, EBI-32872;
CC       P45976:FIP1; NbExp=9; IntAct=EBI-12917, EBI-6940;
CC       Q03735:NAB6; NbExp=2; IntAct=EBI-12917, EBI-27955;
CC       Q01329:PTA1; NbExp=8; IntAct=EBI-12917, EBI-14145;
CC   -!- SUBCELLULAR LOCATION: Nucleus {ECO:0000269|PubMed:12819204,
CC       ECO:0000269|PubMed:14562095}.
CC   -!- MISCELLANEOUS: Present with 17100 molecules/cell in log phase SD
CC       medium. {ECO:0000269|PubMed:14562106}.
CC   -!- SIMILARITY: Belongs to the poly(A) polymerase family.
CC       {ECO:0000305}.
CC   -----------------------------------------------------------------------
CC   Copyrighted by the UniProt Consortium, see https://www.uniprot.org/terms
CC   Distributed under the Creative Commons Attribution (CC BY 4.0) License
CC   -----------------------------------------------------------------------
DR   EMBL; X60307; CAA42852.1; -; Genomic_DNA.
DR   EMBL; X65124; CAA46250.1; -; Genomic_DNA.
DR   EMBL; Z28227; CAA82072.1; -; Genomic_DNA.
DR   EMBL; BK006944; DAA09158.1; -; Genomic_DNA.
DR   PIR; S19031; S19031.
DR   RefSeq; NP_012927.3; NM_001179792.3.
DR   PDB; 1FA0; X-ray; 2.60 A; A/B=1-537.
DR   PDB; 2HHP; X-ray; 1.80 A; A=1-530.
DR   PDB; 2O1P; X-ray; 2.70 A; A/B=1-538.
DR   PDB; 2Q66; X-ray; 1.80 A; A=5-529.
DR   PDB; 3C66; X-ray; 2.60 A; A/B=1-526.
DR   PDBsum; 1FA0; -.
DR   PDBsum; 2HHP; -.
DR   PDBsum; 2O1P; -.
DR   PDBsum; 2Q66; -.
DR   PDBsum; 3C66; -.
DR   ProteinModelPortal; P29468; -.
DR   SMR; P29468; -.
DR   BioGrid; 34134; 75.
DR   ComplexPortal; CPX-1053; Cleavage and polyadenylation specificity factor complex.
DR   DIP; DIP-2297N; -.
DR   IntAct; P29468; 30.
DR   MINT; P29468; -.
DR   STRING; 4932.YKR002W; -.
DR   iPTMnet; P29468; -.
DR   MaxQB; P29468; -.
DR   PaxDb; P29468; -.
DR   PRIDE; P29468; -.
DR   EnsemblFungi; YKR002W_mRNA; YKR002W_mRNA; YKR002W.
DR   GeneID; 853871; -.
DR   KEGG; sce:YKR002W; -.
DR   SGD; S000001710; PAP1.
DR   GeneTree; ENSGT00940000168779; -.
DR   HOGENOM; HOG000204376; -.
DR   InParanoid; P29468; -.
DR   KO; K14376; -.
DR   OMA; YQKVYGI; -.
DR   BioCyc; YEAST:G3O-31980-MONOMER; -.
DR   BRENDA; 2.7.7.19; 984.
DR   EvolutionaryTrace; P29468; -.
DR   PRO; PR:P29468; -.
DR   Proteomes; UP000002311; Chromosome XI.
DR   GO; GO:0005847; C:mRNA cleavage and polyadenylation specificity factor complex; IDA:SGD.
DR   GO; GO:0005654; C:nucleoplasm; IPI:SGD.
DR   GO; GO:0005634; C:nucleus; IBA:GO_Central.
DR   GO; GO:0005524; F:ATP binding; IDA:UniProtKB.
DR   GO; GO:0000287; F:magnesium ion binding; IDA:UniProtKB.
DR   GO; GO:0004652; F:polynucleotide adenylyltransferase activity; IDA:UniProtKB.
DR   GO; GO:0003723; F:RNA binding; IEA:UniProtKB-KW.
DR   GO; GO:0006378; P:mRNA polyadenylation; IDA:SGD.
DR   GO; GO:0043631; P:RNA polyadenylation; IDA:UniProtKB.
DR   GO; GO:0071050; P:snoRNA polyadenylation; IGI:SGD.
DR   InterPro; IPR011068; NuclTrfase_I-like_C.
DR   InterPro; IPR007012; PolA_pol_cen_dom.
DR   InterPro; IPR007010; PolA_pol_RNA-bd_dom.
DR   InterPro; IPR014492; PolyA_polymerase.
DR   InterPro; IPR002934; Polymerase_NTP_transf_dom.
DR   PANTHER; PTHR10682; PTHR10682; 1.
DR   Pfam; PF01909; NTP_transf_2; 1.
DR   Pfam; PF04928; PAP_central; 1.
DR   Pfam; PF04926; PAP_RNA-bind; 1.
DR   PIRSF; PIRSF018425; PolyA_polymerase; 1.
DR   SUPFAM; SSF55003; SSF55003; 1.
PE   1: Evidence at protein level;
KW   3D-structure; ATP-binding; Complete proteome;
KW   Direct protein sequencing; Magnesium; Manganese; Metal-binding;
KW   mRNA processing; Nucleotide-binding; Nucleus; Phosphoprotein;
KW   Reference proteome; RNA-binding; Transferase.
FT   CHAIN         1    568       Poly(A) polymerase.
FT                                /FTId=PRO_0000051621.
FT   NP_BIND      87     89       ATP. {ECO:0000269|PubMed:17850751}.
FT   NP_BIND      99    102       ATP. {ECO:0000269|PubMed:17850751}.
FT   NP_BIND     100    102       ATP. {ECO:0000269|PubMed:17850751}.
FT   NP_BIND     233    234       ATP. {ECO:0000269|PubMed:17850751}.
FT   METAL       100    100       Magnesium 1; catalytic.
FT   METAL       100    100       Magnesium 2; catalytic.
FT   METAL       102    102       Magnesium 1; catalytic.
FT   METAL       102    102       Magnesium 2; catalytic.
FT   METAL       154    154       Magnesium 2; catalytic.
FT   BINDING     154    154       ATP. {ECO:0000269|PubMed:17850751}.
FT   BINDING     215    215       ATP. {ECO:0000269|PubMed:17850751}.
FT   BINDING     224    224       ATP. {ECO:0000269|PubMed:17850751}.
FT   SITE        140    140       Interaction with RNA.
FT   SITE        145    145       Interaction with RNA.
FT   SITE        294    294       Interaction with RNA.
FT   SITE        314    314       Interaction with RNA.
FT   SITE        315    315       Interaction with RNA.
FT   SITE        387    387       Interaction with RNA.
FT   SITE        392    392       Interaction with RNA.
FT   SITE        487    487       Interaction with RNA.
FT   MOD_RES     452    452       Phosphoserine.
FT                                {ECO:0000244|PubMed:18407956}.
FT   MOD_RES     550    550       Phosphoserine.
FT                                {ECO:0000244|PubMed:19779198}.
FT   MUTAGEN     154    154       D->A: Loss of enzyme activity.
FT                                {ECO:0000269|PubMed:17850751}.
FT   MUTAGEN     189    189       N->A: Slightly reduced rate of
FT                                adenylyltransfer.
FT                                {ECO:0000269|PubMed:17850751}.
FT   MUTAGEN     215    215       K->A: Reduces rate of adenylyltransfer
FT                                about four-fold.
FT                                {ECO:0000269|PubMed:17850751}.
FT   MUTAGEN     226    226       N->A: Reduces rate of adenylyltransfer by
FT                                half. {ECO:0000269|PubMed:17850751}.
FT   MUTAGEN     485    485       C->R: Abolishes interaction with FIP1;
FT                                when associated with Y-489.
FT                                {ECO:0000269|PubMed:18537269}.
FT   MUTAGEN     489    489       V->Y: Abolishes interaction with FIP1;
FT                                when associated with R-485.
FT                                {ECO:0000269|PubMed:18537269}.
FT   HELIX         5      7       {ECO:0000244|PDB:2HHP}.
FT   STRAND        8     10       {ECO:0000244|PDB:2Q66}.
FT   HELIX        20     35       {ECO:0000244|PDB:2HHP}.
FT   HELIX        42     69       {ECO:0000244|PDB:2HHP}.
FT   HELIX        74     79       {ECO:0000244|PDB:2HHP}.
FT   STRAND       83     87       {ECO:0000244|PDB:2HHP}.
FT   HELIX        88     92       {ECO:0000244|PDB:2HHP}.
FT   STRAND      101    107       {ECO:0000244|PDB:2HHP}.
FT   HELIX       113    125       {ECO:0000244|PDB:2HHP}.
FT   STRAND      130    136       {ECO:0000244|PDB:2HHP}.
FT   STRAND      139    141       {ECO:0000244|PDB:2HHP}.
FT   STRAND      143    148       {ECO:0000244|PDB:2HHP}.
FT   STRAND      151    159       {ECO:0000244|PDB:2HHP}.
FT   STRAND      161    163       {ECO:0000244|PDB:2HHP}.
FT   HELIX       174    177       {ECO:0000244|PDB:2HHP}.
FT   HELIX       182    200       {ECO:0000244|PDB:2HHP}.
FT   HELIX       204    220       {ECO:0000244|PDB:2HHP}.
FT   HELIX       226    228       {ECO:0000244|PDB:2HHP}.
FT   HELIX       233    246       {ECO:0000244|PDB:2HHP}.
FT   HELIX       252    265       {ECO:0000244|PDB:2HHP}.
FT   STRAND      272    275       {ECO:0000244|PDB:2HHP}.
FT   STRAND      281    283       {ECO:0000244|PDB:1FA0}.
FT   TURN        289    291       {ECO:0000244|PDB:2HHP}.
FT   HELIX       293    296       {ECO:0000244|PDB:2HHP}.
FT   STRAND      305    308       {ECO:0000244|PDB:2HHP}.
FT   TURN        312    315       {ECO:0000244|PDB:2HHP}.
FT   HELIX       318    339       {ECO:0000244|PDB:2HHP}.
FT   HELIX       345    348       {ECO:0000244|PDB:2HHP}.
FT   HELIX       354    357       {ECO:0000244|PDB:2HHP}.
FT   STRAND      359    370       {ECO:0000244|PDB:2HHP}.
FT   HELIX       372    394       {ECO:0000244|PDB:2HHP}.
FT   STRAND      399    404       {ECO:0000244|PDB:2HHP}.
FT   STRAND      409    414       {ECO:0000244|PDB:2HHP}.
FT   STRAND      417    419       {ECO:0000244|PDB:2HHP}.
FT   HELIX       420    427       {ECO:0000244|PDB:2HHP}.
FT   STRAND      428    430       {ECO:0000244|PDB:1FA0}.
FT   HELIX       431    434       {ECO:0000244|PDB:2HHP}.
FT   HELIX       435    439       {ECO:0000244|PDB:1FA0}.
FT   TURN        444    446       {ECO:0000244|PDB:2Q66}.
FT   HELIX       451    455       {ECO:0000244|PDB:2Q66}.
FT   STRAND      458    470       {ECO:0000244|PDB:2HHP}.
FT   STRAND      475    477       {ECO:0000244|PDB:2O1P}.
FT   HELIX       482    494       {ECO:0000244|PDB:2HHP}.
FT   TURN        497    500       {ECO:0000244|PDB:2HHP}.
FT   STRAND      502    513       {ECO:0000244|PDB:2HHP}.
FT   HELIX       514    516       {ECO:0000244|PDB:2HHP}.
FT   HELIX       519    521       {ECO:0000244|PDB:2HHP}.
SQ   SEQUENCE   568 AA;  64552 MW;  759DE5210DC8D881 CRC64;
     MSSQKVFGIT GPVSTVGATA AENKLNDSLI QELKKEGSFE TEQETANRVQ VLKILQELAQ
     RFVYEVSKKK NMSDGMARDA GGKIFTYGSY RLGVHGPGSD IDTLVVVPKH VTREDFFTVF
     DSLLRERKEL DEIAPVPDAF VPIIKIKFSG ISIDLICARL DQPQVPLSLT LSDKNLLRNL
     DEKDLRALNG TRVTDEILEL VPKPNVFRIA LRAIKLWAQR RAVYANIFGF PGGVAWAMLV
     ARICQLYPNA CSAVILNRFF IILSEWNWPQ PVILKPIEDG PLQVRVWNPK IYAQDRSHRM
     PVITPAYPSM CATHNITEST KKVILQEFVR GVQITNDIFS NKKSWANLFE KNDFFFRYKF
     YLEITAYTRG SDEQHLKWSG LVESKVRLLV MKLEVLAGIK IAHPFTKPFE SSYCCPTEDD
     YEMIQDKYGS HKTETALNAL KLVTDENKEE ESIKDAPKAY LSTMYIGLDF NIENKKEKVD
     IHIPCTEFVN LCRSFNEDYG DHKVFNLALR FVKGYDLPDE VFDENEKRPS KKSKRKNLDA
     RHETVKRSKS DAASGDNING TTAAVDVN
//
"""

--}

-------------------------------------------------------------------------------------------[ Main ]
--{1

namespace Main

  main : IO ()
  main = do
    case parse parseUPEntry entry1 of
      Left err => "Error : " <+> err
      Right v  => show v

--}


