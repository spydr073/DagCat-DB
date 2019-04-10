-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Subject.Sequence

import Database.UniProt.Utils

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Data.String

%default partial
%access export

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ CC Field ]
--{1

--{2 Documentation
--
-- The 'CC' (Comment Block) fields contain categorized blocks of free text comments about the
-- protein or sequence entry. The format of a comment block is:
--
-- > CC   -!- TOPIC: First line of a comment block;
-- > CC       second and subsequent lines of a comment block.
--
-- Valid topics and their descriptions are as follows:
--
-- ALLERGEN                       Information relevant to allergenic proteins.
--
-- ALTERNATIVE PRODUCTS           Description of the existence of related protein sequence(s)
--                                produced by alternative splicing of the same gene, alternative
--                                promoter usage, ribosomal frameshifting or by the use of
--                                alternative initiation codons.
--
-- BIOPHYSICOCHEMICAL PROPERTIES  Description of the information relevant to biophysical and
--                                physicochemical data and information on pH dependence,
--                                temperature dependence, kinetic parameters, redox potentials,
--                                and maximal absorption.
--
-- BIOTECHNOLOGY                  Description of the use of a specific protein in a
--                                biotechnological process.
--
-- CATALYTIC ACTIVITY             Description of the reaction(s) catalyzed by an enzyme.
--                                Chemical reaction descriptions are from the Rhea database
--                                whenever possible.
--
-- CAUTION                        Warning about possible errors and/or grounds for confusion.
--
-- COFACTOR                       Description of any non-protein substance required by an enzyme
--                                for its catalytic activity.
--
-- DEVELOPMENTAL STAGE            Description of the developmentally-specific expression of mRNA
--                                or protein.
--
-- DISEASE                        Description of the disease(s) associated with a deficiency of
--                                a protein.
--
-- DISRUPTION PHENOTYPE           Description of the effects caused by the disruption of the gene
--                                coding for the protein.
--
-- DOMAIN                         Description of the domain structure of a protein.
--
-- ACTIVITY REGULATION            Description of the regulatory mechanism of an enzyme,
--                                transporter, microbial transcription factor.
--
-- FUNCTION                       General description of the function(s) of a protein.
--
-- INDUCTION                      Description of the compound(s) or condition(s) that regulate
--                                gene expression.
--
-- INTERACTION                    Conveys information relevant to binary protein-protein
--                                interaction.
--
-- MASS SPECTROMETRY              Reports the exact molecular weight of a protein or part of a
--                                protein as determined by mass spectrometric methods.
--
-- MISCELLANEOUS                  Any comment which does not belong to any of the other defined
--                                topics.
--
-- PATHWAY                        Description of the metabolic pathway(s) with which a protein
--                                is associated.
--
-- PHARMACEUTICAL                 Description of the use of a protein as a pharmaceutical drug.
--
-- POLYMORPHISM                   Description of polymorphism(s).
--
-- PTM                            Description of any chemical alternation of a polypeptide
--                                (proteolytic cleavage, amino acid modifications including
--                                crosslinks). This topic complements information given in the
--                                feature table or indicates polypeptide modifications for which
--                                position-specific data is not available.
--
-- RNA EDITING                    Description of any type of RNA editing that leads to one or
--                                more amino acid changes.
--
-- SEQUENCE CAUTION               Description of protein sequence reports that differ from the
--                                sequence that is shown in UniProtKB due to conflicts that are
--                                not described in FT CONFLICT lines, such as frameshifts,
--                                erroneous gene model predictions.
--
-- SIMILARITY                     Description of the similaritie(s) (sequence or structural)
--                                of a protein with other proteins.
--
-- SUBCELLULAR LOCATION           Description of the subcellular location of the
--                                chain/peptide/isoform.
--
-- SUBUNIT                        Description of the quaternary structure of a protein and any
--                                kind of interactions with other proteins or protein complexes;
--                                except for receptor-ligand interactions, which are described
--                                in the topic FUNCTION.
--
-- TISSUE SPECIFICITY             Description of the tissue-specific expression of mRNA or protein.
--
-- TOXIC DOSE                     Description of the lethal dose (LD), paralytic dose (PD) or
--                                effective dose of a protein.
--
-- WEB RESOURCE                   Description of a cross-reference to a network database/resource
--                                for a specific protein.
--
--}

--{2 BIOPHYSICOCHEMICAL PROPERTIES Documentation
--
-- A BIOPHYSICOCHEMICAL PROPERTIES block must contain at least one of the properties Absorption,
-- Kinetic parameters, pH dependence, Redox potential, Temperature dependence and may have any
-- combination of these properties (ordered as listed).
--
-- Absorption               Indicates the wavelength at which photoreactive proteins such as
--                          opsins and DNA photolyases show maximal absorption.
--
-- Kinetic parameters       Mentions the Michaelis-Menten constant (KM) and maximal velocity
--                          (Vmax) of enzymes.
--
-- pH dependence            Describes the optimum pH for enzyme activity and/or the variation of
--                          enzyme activity with pH variation.
--
-- Redox potential          Reports the value of the standard (midpoint) oxido-reduction
--                          potential(s) for electron transport proteins.
--
-- Temperature dependence   Indicates the optimum temperature for enzyme activity and/or the
--                          variation of enzyme activity with temperature variation; the
--                          thermostability/thermolability of the enzyme is also mentioned when
--                          it is known.
--
-- This field has the following format:
--
-- > CC   -!- BIOPHYSICOCHEMICAL PROPERTIES:
-- > CC       Absorption:
-- > CC         Abs(max)=xx nm;
-- > CC         Note=free_text;
-- > CC       Kinetic parameters:
-- > CC         KM=xx unit for substrate [(free_text)];
-- > CC         Vmax=xx unit enzyme [free_text];
-- > CC         Note=free_text;
-- > CC       pH dependence:
-- > CC         free_text;
-- > CC       Redox potential:
-- > CC         free_text;
-- > CC       Temperature dependence:
-- > CC         free_text;
--
--}

--{2 INTERACTION Documentation
--
-- INTERACTION conveys information relevant to binary protein-protein interaction. It is
-- automatically derived from the IntAct database and is updated on a monthly basis. The
-- occurrence is one INTERACTION topic per entry, with each binary interaction being presented
-- in a separate line. Each data line can be longer than 75 characters.
--
-- The format of the CC line topic INTERACTION is:
--
-- > CC   -!- INTERACTION:
-- > CC       {{AC:GN[ (Xeno)]}|Self}; NbExp=n; IntAct=IntAct_Protein_Ac, IntAct_Protein_Ac;
--
-- Where
--
-- AC                 The Swiss-Prot or TrEMBL accession number of the interacting protein. If
--                    appropriate, the IsoId is used instead to specify the relevant interacting
--                    protein isoform.
--
-- GN                 Serves to describe the interacting protein. It is derived from the Swiss-Prot
--                    or TrEMBL GN line and thus presents either a "gene name", a "ordered locus
--                    name" or a "ORF name". When no GN line is available a dash is indicated
--                    instead.
--
-- Xeno               Is an optional qualifier indicating that the interacting proteins are derived
--                    from different species. This may be due to the experimental set-up or may
--                    reflect a pathogen-host interaction.
--
-- Self               Reflects a self-association; the corresponding current entry's SP_Ac and
--                    'identifier' are not given/repeated.
--
-- NbExp=n            Refers to the number of experiments in IntAct supporting the interaction.
--
-- IntAct_Protein_Ac  The IntAct accession number of a interacting protein. The first
--                    IntAct_Protein_Ac refers to the protein or an isoform of the current entry,
--                    the second refers to the interacting protein or isoform.
--
--}

--{2 SUBCELLULAR LOCATION Documentation
--
-- The format of SUBCELLULAR LOCATION is:
--
-- > CC   -!- SUBCELLULAR LOCATION:(( Molecule:)?( Location\.)+)?( Note=Free_text( Flag)?\.)?
--
-- Where:
--
-- Molecule: Isoform, chain or peptide name
-- Location = Subcellular_location( Flag)?(; Topology( Flag)?)?(; Orientation( Flag)?)?
--     Subcellular_location: SL-line of subcell.txt ID-record
--     Topology: SL-line of subcell.txt IT-record
--     Orientation: SL-line of subcell.txt IO-record
--
-- Note: Perl-style multipliers indicate whether a pattern (as delimited by parentheses)
-- is optional (?) or may occur 1 or more times (+).
--
--}

--{2 ALTERNATIVE PRODUCTS Documentation
--
-- Event           Biological process that results in the production of the alternative forms. It
--                 lists one or a combination of the following values (Alternative promoter usage,
--                 Alternative splicing, Alternative initiation, Ribosomal frameshifting).
--                 Format: Event=controlled vocabulary;
--
-- Named isoforms  Number of isoforms listed in the topics 'Name' currently only for
--                 'Event=Alternative splicing'.
--                 Format: Named isoforms=number;
--
-- Comment         Any comments concerning one or more isoforms; optional;
--                 Format: Comment=free text;
--
-- Name            A common name for an isoform used in the literature or assigned by Swiss-Prot;
--                 currenty only available for spliced isoforms.
--                 Format: Name=common name;
--
-- Synonyms        Synonyms for an isoform as used in the literature; optional; currently only
--                 available for spliced isoforms.
--                 Format: Synonyms=Synonym_1[, Synonym_n];
--
-- IsoId           Unique identifier for an isoform, consisting of the Swiss-Prot accession number,
--                 followed by a dash and a number.
--                 Format: IsoId=acc#-isoform_number[, acc#-isoform_number];
--
-- Sequence        Information on the isoform sequence; the term 'Displayed' indicates, that the
--                 sequence is shown in the entry; a lists of feature identifiers (VSP_#) indicates
--                 that the isoform is annotated in the feature table; the FTIds enable programs to
--                 create the sequence of a splice variant; if the accession number of the IsoId
--                 does not correspond to the accession number of the current entry, this topic
--                 contains the term 'External'; 'Not described' points out that the sequence of
--                 the isoform is unknown.
--                 Format: Sequence=VSP_#[, VSP_#]|Displayed|External|Not described;
--
-- Note            Lists isoform-specific information; optional. It may specify the event(s), if
--                 there are several.
--                 Format: Note=Free text;
--
-- The format of the CC line topic ALTERNATIVE PRODUCTS is:
--
-- >  CC   -!- ALTERNATIVE PRODUCTS:
-- >  CC       Event=Event(, Event)*; Named isoforms=Number_of_isoforms;
-- > (CC         Comment=Free_text;)?
-- > (CC       Name=Isoform_name;( Synonyms=Synonym(, Synonym)*;)?
-- >  CC         IsoId=Isoform_identifier(, Isoform_identifer)*;
-- >  CC         Sequence=(Displayed|External|Not described|Feature_id(, Feature_id)*);
-- > (CC         Note=Free_text;)?)+
--
-- Note: Variable values are represented in italics. Perl-style multipliers indicate whether a
-- pattern (as delimited by parentheses) is optional (?), may occur 0 or more times (*), or 1 or
-- more times (+). Alternative values are separated by a pipe symbol (|).
--
--}

--{2 MASS SPECTROMETRY Documentation
--
--
-- > CC   -!- MASS SPECTROMETRY: Mass=XX(; Mass_error=XX)?; Method=XX; Range=XX( (IsoformID))?
-- (cont)                        (; Note=free_text)?; Source=XX;
--
-- Where:
--
--    'Mass=XXX' is the determined molecular weight (MW);
--
--    'Mass_error=XX' (optional) is the accuracy or error range of the MW measurement;
--
--    'Method=XX' is the ionization method;
--
--    'Range=XX-XX[ (Name)]' is used to indicate what part of the protein sequence entry
--    corresponds to the molecular weight. In case of multiple products, the name of the relevant
--    isoform is enclosed;
--
--    'Note={Free text}'. Comment in free text format;
--
--    'Source=PubMed:/Ref.n' indicates the relevant reference'.
--
--}

--{2 SEQUENCE CAUTION Documentation
--
--
--
--}

--{2 BIOPHYSICOCHEMICAL PROPERTIES Parser


--}

--{2 INTERACTION Parser



--}

--{2 SUBCELLULAR LOCATION Parser



--}

--{2 ALTERNATIVE PRODUCTS Parser



--}

--{2 MASS SPECTROMETRY Parser



--}

--{2 SEQUENCE CAUTION Parser



--}

--{2 CC

record CC where
  constructor MkCC
  topic   : String
  comment : String

Eq CC where
  (==) (MkCC t1 c1) (MkCC t2 c2) = (t1 == t2) && (c1 == c2)

Show CC where
  show (MkCC t c) = t ++ "=" ++ c

--}

--{2 Parser

parseCC : Parser (List CC)
parseCC = many (MkCC <$> topic <*!> comment)
  where
    topic : Parser String
    topic = (tok $ string "CC") *!> (tok $ string "-!-")
         *!> (pack <$> many (noneOf ":"))
         <*! (tok $ char ':')

    comment : Parser String
    comment = (\x,y => concat $ intersperse " " (x::y))
          <$> ((opt (endOfLine *> (tok $ string "CC"))) *> (pack <$> many (noneOf "\n")))
          <*!> many (endOfLine *> string "CC" *> whitespace
                     *> requireFailure (string "-!-")
                     *!> (pack <$> many (noneOf "\n"))) <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ DR Field ]
--{1

--{2 DR

record DR where
  constructor MkDR
  abbr : String
  uid  : String
  info : List String

Eq DR where
  (==) (MkDR a1 u1 i1) (MkDR a2 u2 i2) = (a1 == a2) && (u1 == u2) && (i1 == i2)

Show DR where
  show (MkDR a u i) = (concat $ List.intersperse " "
       [ "(db : " ++ a
       , "id: "   ++ u
       , "info: " ++ show i ++ ")"
       ])

--}

--{2 Parser

parseDR : Parser (List DR)
parseDR = many (MkDR <$> abbr <*!> uid <*!> info)
  where
    optBracket : Parser ()
    optBracket = whitespace <*! char '[' <*! many (noneOf "]") <*! char ']'

    abbr : Parser String
    abbr = string "DR" *!> whitespace
        *!> (pack <$> many (noneOf ";")) <*! tok (char ';')

    uid : Parser String
    uid = pack <$> many (noneOf ";") <*! tok (char ';')

    clean : List String -> List String
    clean = foldr (\x,acc => if x == "-" || x == "-."
                               then acc
                               else x::acc
                  ) []

    info : Parser (List String)
    info = let str = with List (((trimBy (=='.')) . pack) <$> (many (noneOf ";\n")))
           in (\x,y => clean (x::y))
              <$> tok str
              <*> many (char ';' *> opt (endOfLine *> string "RP") *> whitespace *> str)
              <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ PE Field ]
--{1

--{2 PE

public export
data PE = EvidenceAtProteinLevel
        | EvidenceAtTranscriptLevel
        | InferredFromHomology
        | Predicted
        | Uncertain

Eq PE where
  (==) x y with (x,y)
    | (EvidenceAtProteinLevel    , EvidenceAtProteinLevel)    = True
    | (EvidenceAtTranscriptLevel , EvidenceAtTranscriptLevel) = True
    | (InferredFromHomology      , InferredFromHomology)      = True
    | (Predicted                 , Predicted)                 = True
    | (Uncertain                 , Uncertain)                 = True
    | _                                                       = False

Show PE where
  show x with (x)
    | EvidenceAtProteinLevel    = "EvidenceAtProteinLevel"
    | EvidenceAtTranscriptLevel = "EvidenceAtTranscriptLevel"
    | InferredFromHomology      = "InferredFromHomology"
    | Predicted                 = "Predicted"
    | Uncertain                 = "Uncertain"

--}

--{2 Parser

parsePE : Parser PE
parsePE = string "PE" *!> whitespace *!>
   (    ((string "1: Evidence at protein level;")
          *!> pure EvidenceAtProteinLevel)
   <|>| ((string "2: Evidence at transcript level;")
          *!> pure EvidenceAtTranscriptLevel)
   <|>| ((string "3: Inferred from homology;")
          *!> pure InferredFromHomology)
   <|>| ((string "4: Predicted;")
          *!> pure Predicted)
   <|>| ((string "5: Uncertain;")
          *!> pure Uncertain)
   ) <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ KW Field ]
--{1

--{2 KW

record KW where
  constructor MkKW
  keywords : List String

Eq KW where
  (==) (MkKW kw1) (MkKW kw2) = kw1 == kw2

Show KW where
  show (MkKW x) = "keywords: " ++ show x

--}

--{2 Parser

parseKW : Parser KW
parseKW = MkKW <$> (mllst "KW" ';' '.') <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ FT Field ]
--{1

--{2 FT

record FT where
  constructor MkFT
  name : String
  from : String
  to   : String
  desc : List String

Eq FT where
  (==) (MkFT n1 f1 t1 d1) (MkFT n2 f2 t2 d2) = (n1 == n2) && (f1 == f2)
                                            && (t1 == t2) && (d1 == d2)

Show FT where
  show (MkFT n f t d) = (concat $ List.intersperse " "
       [ "(name : " ++ n
       , "from : "  ++ f
       , "to : "    ++ t
       , "desc: "   ++ show d ++ ")"
       ])

--}

--{2 Parser

-- Whitespace is important here!
--   cols    field
--   ---------------------
--   1-2     FT
--   6-13    Key name
--   15-20   From endpoint
--   22-27   To endpoint
--   35-75   Description

parseFT : Parser (List FT)
parseFT = many (MkFT <$> name <*!> from <*!> to <*!> desc)
  where
    name : Parser String
    name = string "FT   " *!> (trim . pack) <$> ntimes 8 anyChar <*! char ' '

    from : Parser String
    from = ((trim . pack) <$> ntimes 6 anyChar <* char ' ')
      <|>| pure ""

    to : Parser String
    to = ((trim . pack) <$> ntimes 6 anyChar <* char ' ')
      <|>| pure ""

    cleanFT : List String -> List String
    cleanFT lst with (lst)
      | Nil        = Nil
      | [x]        = [x]
      | (x::y::xs) = case last' (unpack x) of
                       Nothing => cleanFT (y::xs)
                       Just c => if c /= '.'
                                   then cleanFT ((x++y)::xs)
                                   else (x :: cleanFT (y::xs))

    desc : Parser (List String)
    desc = ((\x,y => cleanFT (x::y))
            <$> ((trim . pack) <$> many (noneOf "\n"))
            <*> (many (endOfLine *> string "FT    " *> whitespaces
                       *> ((trim . pack) <$> many (noneOf "\n")))) <* endOfLine)
      <|>| pure []

--}

--}

---------------------------------------------------------------------------------------[ SQ Field ]
--{1

--{2 SQ

record SQ where
  constructor MkSQ
  length : Int
  mass   : Int
  crc64  : String
  seq    : String

Eq SQ where
  (==) (MkSQ l1 m1 c1 s1) (MkSQ l2 m2 c2 s2) = (l1 == l2) && (m1 == m2)
                                            && (c1 == c2) && (s1 == s2)

Show SQ where
  show (MkSQ l m c s) = (concat $ List.intersperse " "
       [ "(len : "  ++ show l
       , "mass : "  ++ show m
       , "crc64 : " ++ c
       , "seq: "    ++ s ++ ")"
       ])

--}

--{2 Parser

parseSQ : Parser SQ
parseSQ = string "SQ" *!> whitespaces *!> string "SEQUENCE" *!> whitespace
       *> MkSQ <$> len <*!> mass <*!> crc64 <*!> seq <*! endOfLine
  where
    len : Parser Int
    len = ((fromJust 0) . parseInteger . pack)
      <$> ((tok $ many (satisfy isDigit)) <*! tok (string "AA;"))


    mass : Parser Int
    mass  = ((fromJust 0) . parseInteger . pack)
        <$> ((tok $ many (satisfy isDigit)) <*! tok (string "MW;"))

    crc64 : Parser String
    crc64 = pack <$> (many $ noneOf " ") <*! whitespace <*! string "CRC64;" <*! endOfLine

    seq : Parser String
    seq = concat <$> some ((opt endOfLine) *> whitespace *> (pack <$> some upper))

--}

--}


