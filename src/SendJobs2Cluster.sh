#!/bin/bash
for p in 00 22 74
do
  for i in {1..27};
  do
     echo "#!/bin/bash"                             > ABCskysim.$p.$i.sh
     echo "#$ -S /bin/bash"                         >> ABCskysim.$p.$i.sh;
     echo "#$ -N sky.$p.$i"                         >> ABCskysim.$p.$i.sh;
     echo "#$ -q mem.q"                             >> ABCskysim.$p.$i.sh;
     echo "#$ -cwd"                                 >> ABCskysim.$p.$i.sh;
     echo 'workingDir=$PWD'                         >> ABCskysim.$p.$i.sh;
     echo 'mkdir $TMPDIR/Scenari/'                  >> ABCskysim.$p.$i.sh;
     echo 'cp -rp Scenari/* $TMPDIR/Scenari/'       >> ABCskysim.$p.$i.sh;
     echo 'cp -rp *.R $TMPDIR/'                     >> ABCskysim.$p.$i.sh;
     echo 'cp -rp diyabc-comput-linux-x64 $TMPDIR/' >> ABCskysim.$p.$i.sh;
     echo 'cp -rp fastsimcoal $TMPDIR/'             >> ABCskysim.$p.$i.sh;
     echo 'cd $TMPDIR'                              >> ABCskysim.$p.$i.sh;
     echo 'set -x'                                  >> ABCskysim.$p.$i.sh;
     echo "R --no-save --args directory \$TMPDIR seed $i$p DIYABC_exe_name \"/home/bin/Diyabc/2.1.0/x64/bin/general\" run_in_cluster \"c(T)\" project \"P$p\" simulated_target_data \"c(T)\" num_of_sims 1000000 proportion_of_sims_kept 0.01 num_of_points 50 prior_TAU_max 4 number_of_replicates 1000 true_gsm 0.$p prior_GSM_min 0.0 prior_GSM_max 1.0 prior_SNI_min 0.0 prior_SNI_max 0.0 motif 1 scenarios_number $i < ABCskylineplot.R" >> ABCskysim.$p.$i.sh;
     echo 'set +x'                                  >> ABCskysim.$p.$i.sh;
     echo 'cp -rp $TMPDIR/* $workingDir/'           >> ABCskysim.$p.$i.sh;
     qsub ABCskysim.$p.$i.sh
  done
done
