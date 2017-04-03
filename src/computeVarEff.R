library(VarEff)

# sim 1
project         <- "Scenario8"
number_of_locus <- 10
model           <- "G 0.22"
DMAXPLUS        <- 25

# sim 2
project         <- "Scenario20"
number_of_locus <- 10
model           <- "G 0.22"
DMAXPLUS        <- 5

# sim 3
project         <- "Scenario26"
number_of_locus <- 10
model           <- "G 0.22"
DMAXPLUS        <- 20

# black and white colobus
project         <- "BWC_CNP"
number_of_locus <- 14
model           <- "G 0.37"
DMAXPLUS        <- 6

# red colobus
project         <- "RC_CNP"
number_of_locus <- 13
model           <- "G 0.215"
DMAXPLUS        <- 9

# whale shark
project         <- "Requin"
number_of_locus <- 14
model           <- "G 0.55"
DMAXPLUS        <- 14

# turtle
project         <- "Leatherback"
number_of_locus <- 10
model           <- "G 0.56"
DMAXPLUS        <- 12

VarEff(parafile = project,
       infile = paste0("data/",project,".VarEff"),
       NBLOC = number_of_locus,
       JMAX = 3,
       MODEL = model,
       MUTAT = 0.001,
       NBAR = 1000,
       VARP1 = 3,
       RHOCORN  = 0.5,
       GBAR = 4000,
       VARP2 = 3,
       DMAXPLUS = DMAXPLUS,
       Diagonale = 0.5,
       NumberBatch = 10000,
       LengthBatch = 1,
       SpaceBatch = 100)

NTdist(NameBATCH = paste0(project,".Batch"),
       MUTAT=0,
       TMAX=4)
NatSizeDist(NameBATCH = paste0(project,".Batch"),
            MUTAT     = 0,
            TMAX      = 4,
            NBT       = 20)
