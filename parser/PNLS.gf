abstract PNLS = {
  flags startcat = S ;
  cat
    Quant ; CN ; NP ; S; ModalAdv; VP; A; Operator;
    AVP; Pol; Cl; RS; RP; RCl;
    Card; Digits; Di; Unit;
  fun
    Modify : ModalAdv -> VP -> AVP ;
    Bare   :             VP -> AVP ;

    IsA : CN -> VP ;               -- Example: to be a violinist
    S1  : NP -> AVP -> Cl;                -- Example: John reads music
    If  : S -> S -> S;            -- Example: If John generally reads music, then Mary is a violinist
    But : S -> S -> S;
    And : S -> S -> S;
    Or : S -> S -> S;
    Not : S -> S;
    Parens : S -> S;

    CltoS : Pol -> Cl -> S;

    Neg: Pol;
    Pos: Pol;

    THAT : RP;

    MakeRCL: RP -> VP -> RCl;

    MakepolarRS: Pol -> RCl -> RS;

    -- MakeoutofRSNP: NP -> RS -> NP ;

    RelativiseCN : CN -> RS -> CN;

    All,Most,Few,AFew,Every,GenericPl, Many, the, a , Some : Quant;

    PercentOf : Card -> CN -> NP;
    Exactly, AtLeast, MoreThan : Card -> Quant;

    Non : CN -> CN;
    Qual : A -> CN -> CN;                  -- Example: short musician

    QNP : Quant -> CN -> NP ;                -- Example: every musician

    Never, Always,
    Also, Rarely, Probably, Occasionally, Generally, Usually, Definitely, Often, Frequently, Regularly : ModalAdv; 

    KnowFLT : VP;
    DislikeExperimentalWork : VP;
    ListenToOudMusic : VP;
    EnjoyArak : VP;
    PlayInMF : VP;
    PlayForTheLeafs : VP;
    PreferTheDoorsToTheBeatles : VP;
    PlayTheStones : VP;
    TryHairTransplantTreatment : VP;
    ReadMusic, HitRun, CanProveFOL : VP;
    UseTailRec: VP;
    EatHumus, EnjoyTabouli, InsistOnMintTeaWithFood : VP;
    SupportFreeUniversityEducation : VP;

    Like : NP -> VP;
    CanPlayChoords : Operator -> Card -> VP;

    ComparVP : Operator -> A -> NP -> VP;   -- Example: "is as punctual as Mary"
    MoreVP : A -> NP -> VP;   -- Example: "taller than Mary"

    TestVP : A -> VP;   -- Example: "is punctual"

    Equal, More, Less : Operator;

    Linguist, BaldMan, ToupeeWearer : CN;
    Person, Violinist, Musician, NeuralNetwork, BasketballPlayer, CricketPlayer, BaseballPlayer : CN;
    StonesFan, InterLogicStudent, IntroLogicStudent, AdvanLogicStudent : CN;
    JazzGuitarist, Guitarist, RockGuitarist, TurkishCoffeeDrinker, HomeRun , PrologProgrammer, Physicist , Logician, Conservative : CN;

    Mary, John, Sam, Molly, Kate, Athena, Helen, Artemis, Joanna, Christine, Ruth : NP;
    GenericYou : NP ;

    Punctual, Tall, Short, TradedFromTheCanadiens : A;


    n0_Dig : Di;
    n1_Dig : Di;
    n2_Dig : Di;
    n3_Dig : Di;
    n4_Dig : Di;
    n5_Dig : Di;
    n6_Dig : Di;
    n7_Dig : Di;
    n8_Dig : Di;
    n9_Dig : Di;
    addDigit : Di -> Digits -> Digits;
    oneDigit : Di -> Digits;
    digitsToCard : Digits -> Card;


    Centimeter, Foot : Unit;

    Measure : Card -> Unit -> A -> VP;
}
