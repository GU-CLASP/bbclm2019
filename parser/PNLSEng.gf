concrete PNLSEng of PNLS = open MorphoEng, SyntaxEng, ParadigmsEng, NumeralEng, ConstructionEng in {
  lincat
    S = SyntaxEng.S ;
    NP = SyntaxEng.NP ;
    CN = SyntaxEng.CN ;
    Quant = Det ;
    ModalAdv = AdV;
    VP = SyntaxEng.VP;
    A = SyntaxEng.A;
    AVP = SyntaxEng.VP;
    Operator = CAdv;
    Pol = SyntaxEng.Pol;
    Cl = SyntaxEng.Cl;
    RS = SyntaxEng.RS;
    RCl = SyntaxEng.RCl;
    RP =  SyntaxEng.RP;

    Card = SyntaxEng.Card;
    Digits = SyntaxEng.Digits;
    Di = NumeralEng.Dig;
    Unit = N;
  lin
    AtLeast n = mkDet (mkCard at_least_AdN n);
    MoreThan n = mkDet (mkCard (SyntaxEng.mkAdN more_CAdv) n);
    Exactly n = mkDet (mkCard (ParadigmsEng.mkAdN "exactly") n);
    Every = every_Det;
    Many = many_Det;
    GenericPl = aPl_Det;
    Few = few_Det;
    AFew = mkDeterminer plural ["a few"] ;
    Most = mkDeterminer plural "most" ;
    All = mkDeterminer plural "all" ;
    Some = somePl_Det;

    If cl1 cl2 = mkS if_then_Conj cl1 cl2;
    But cl1 cl2 = mkS (mkConj "but") cl1 cl2;
    And cl1 cl2 = mkS (mkConj "and") cl1 cl2;
    Or cl1 cl2 = mkS (mkConj "or") cl1 cl2;
    Not cl = mkS (ParadigmsEng.mkAdv (["it is not the case that"])) cl;
    Parens cl = lin S { s = "(" ++ cl.s ++ ")" };


    Neg = negativePol;
    Pos = positivePol;

    CltoS p c = mkS presentTense simultaneousAnt p c;

    THAT = which_RP;

    MakeRCL rp vp = mkRCl rp vp;

    RelativiseCN cn rs = mkCN cn rs;

    MakepolarRS p r =  mkRS presentTense simultaneousAnt p r;

    -- MakeoutofRSNP np rs = mkNP np rs;

    the = the_Det;
    a = a_Det;

    IsA cn =  mkVP cn ;

    QNP q cn = mkNP q cn ;

    S1 np vp = mkCl np vp;
    ComparVP op a np = mkVP (mkComp (mkAP op (mkAP a) np));
    MoreVP a np = mkVP (mkComp (mkAP a np));

    TestVP a = mkVP (mkComp (mkAP a));

    Qual ap cn = mkCN ap cn;
    Non cn = mkCN (mkAP (ParadigmsEng.mkA "non")) cn ;

    Equal = as_CAdv;
    More = more_CAdv;
    Less = less_CAdv;

    Modify adv vp = mkVP adv vp;
    Bare vp = vp;


    Always = always_AdV;
    Never = ParadigmsEng.mkAdV "never";
    Generally = ParadigmsEng.mkAdV "generally";
    Probably = ParadigmsEng.mkAdV "probably";
    Occasionally = ParadigmsEng.mkAdV "occasionally";
    Usually = ParadigmsEng.mkAdV "usually";
    Rarely = ParadigmsEng.mkAdV "rarely";
    Definitely = ParadigmsEng.mkAdV "definitely";
    Often = ParadigmsEng.mkAdV "often";
    Frequently = ParadigmsEng.mkAdV "frequently";
    Regularly = ParadigmsEng.mkAdV "regularly";
    Also = ParadigmsEng.mkAdV "also";


    KnowFLT = mkVP (mkV2 "know") (mkNP (mkN ["formal language theory"]));
    SupportFreeUniversityEducation  = mkVP (mkV2 "support") (mkNP (mkN ["free university education"]));
    DislikeExperimentalWork = mkVP (mkV2 "dislike") (mkNP (mkN ["experimental work"]));
    ListenToOudMusic = mkVP (mkV2 "listen") (mkNP (mkN ["to classical oud music"]));
    EnjoyArak = mkVP (mkV2 "enjoy") (mkNP (mkN ["a shot of arak"]));
    PlayForTheLeafs = mkVP (mkV2 "play") (mkNP (mkN ["for the leafs"]));
    PlayInMF = mkVP (mkV2 "play") (mkNP (mkN ["in the montreal forum"]));
    PreferTheDoorsToTheBeatles = mkVP (mkV2 "prefer") (mkNP (mkN ["the doors to the beatles"]));
    TryHairTransplantTreatment = mkVP (mkV2 "try") (mkNP (mkN ["hair transplant treatment"]));
    ReadMusic = mkVP (mkV2 "read") (mkNP (mkCN (mkN "music")));
    HitRun = mkVP (mkV2 "hit") (mkNP a_Det (mkCN (mkN "home run")) );
    CanProveFOL = mkVP can_VV (mkVP (mkV2 "prove") theCompletenessOfFol);
    UseTailRec = mkVP (mkV2 "use") (mkNP (mkN ["tail recursion"]));
    EatHumus = mkVP (mkV2 "eat") (mkNP (mkN ["humus"]));
    EnjoyTabouli = mkVP (mkV2 "enjoy") (mkNP (mkN ["tabouli"]));
    InsistOnMintTeaWithFood = mkVP (mkV2 "insist") (mkNP (mkN ["on having mint tea with food"]));
    PlayTheStones = mkVP (mkV2 "play") (mkNP (mkN ["the stones"]));

    Like np = mkVP (mkV2 "like") np;
    CanPlayChoords op n = mkVP can_VV (mkVP (mkV2 "play") (mkNP (mkCard (SyntaxEng.mkAdN op) n) (mkCN (mkN "chord"))));

    Linguist = mkCN (mkN "linguist") ;
    NeuralNetwork = mkCN (ParadigmsEng.mkA "neural") (mkN "network") ;
    PrologProgrammer = mkCN (mkN ["prolog programmer"]) ;

    BasketballPlayer = mkCN (mkN ["basketball player"]) ;
    BaseballPlayer = mkCN (mkN ["baseball player"]) ;
    CricketPlayer = mkCN (mkN ["cricket player"]) ;

    StonesFan = mkCN (mkN ["stones fan"]) ;
    IntroLogicStudent = mkCN (mkN ["introductory logic student"]) ;
    InterLogicStudent = mkCN (mkN ["intermediate logic student"]) ;
    AdvanLogicStudent = mkCN (mkN ["advanced logic student"]) ;
    JazzGuitarist = mkCN (mkN ["jazz guitarist"]) ;
    RockGuitarist = mkCN (mkN ["rock guitarist"]) ;
    Guitarist = mkCN (mkN ["guitarist"]) ;
    Physicist = mkCN (mkN ["physicist"]) ;
    Logician = mkCN (mkN ["logician"]) ;
    Conservative = mkCN (mkN ["conservative"]) ;
    TurkishCoffeeDrinker = mkCN (mkN ["turkish coffee drinker"]) ;
    HomeRun = mkCN (mkN ["home run"]) ;
    BaldMan = mkCN (mkAP (ParadigmsEng.mkA "bald")) (mkCN (mkN "man" "men")) ;
    ToupeeWearer = mkCN (mkAP (ParadigmsEng.mkA "toupee")) (mkCN (mkN "wearer")) ;

 
    

    -- CompletenessofFOL = mkCN (mkN ["completeness of FOL"]) ; 


    -- Neural networks are usually implemented in Python.

    TradedFromTheCanadiens = (ParadigmsEng.mkA "traded from the canadiens");
    Punctual = (ParadigmsEng.mkA "punctual");
    Tall = (ParadigmsEng.mkA "tall");
    Short = (ParadigmsEng.mkA "short");

    Mary = mkNP (mkPN "mary");
    John = mkNP (mkPN "john");
    Sam = mkNP (mkPN "sam");
    Molly = mkNP (mkPN "molly");

    Artemis = mkNP (mkPN "artemis");
    Joanna = mkNP (mkPN "joanna");
    Christine = mkNP (mkPN "christine");
    Ruth = mkNP (mkPN "ruth");
    Kate = mkNP (mkPN "kate");
    Athena = mkNP (mkPN "athena");
    Helen = mkNP (mkPN "helen");
    Kate = mkNP (mkPN "kate");
    
    GenericYou = you_NP;

    Violinist = mkCN (mkN "violinist") ;
    Musician = mkCN (mkN "musician") ;
    Person = mkCN (mkN "person" "people") ;


    -- FortyPercentOf = mkDeterminer plural ("40 % of"); 
    PercentOf digits cn = mkNP (mkNP digits (mkCN (mkN "percent" "percent"))) (myAdv part_Prep (mkNP aPl_Det cn));



    n0_Dig = n0_Dig;
    n1_Dig = n1_Dig;
    n2_Dig = n2_Dig;
    n3_Dig = n3_Dig;
    n4_Dig = n4_Dig;
    n5_Dig = n5_Dig;
    n6_Dig = n6_Dig;
    n7_Dig = n7_Dig;
    n8_Dig = n8_Dig;
    n9_Dig = n9_Dig;
    addDigit = mkDigits;
    oneDigit = mkDigits;
    digitsToCard = mkCard;

    Measure n u a = mkVP (mkComp (n_units_AP n (mkCN u) a));

    Centimeter = mkN "centimeter";

    Foot = mkN "foot" "foot";
    
oper
    myAdv : SyntaxEng.Prep -> SyntaxEng.NP -> SyntaxEng.Adv;
    myAdv p np = SyntaxEng.mkAdv p np;

    theCompletenessOfFol : SyntaxEng.NP;
    theCompletenessOfFol = mkNP the_Det completenessofFOL;
    
    completenessofFOL : SyntaxEng.CN;
    completenessofFOL = mkCN  (mkN2  (mkN ("completeness"))) (mkNP (mkPN "fol"));

    -- complN2 n2 np =  mkCN n2 np;
    -- Modality modal vp = mkVP modal vp;
    -- Prove = mkVP (mkV "prove");
    -- completeness : SyntaxEng.N2
    -- completeness =  (mkN2 (mkN "completeness")) ;

}

