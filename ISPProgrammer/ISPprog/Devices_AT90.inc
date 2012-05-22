    (b0:$1E; b1:$90; b2:$01;
    name:'AT90S1200';
    proctype:PROC_TYPE_AVR;
    fsize:1024; esize:64; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_STD;
    algo_busy:ALGO_BUSY_POLL_00FF;
    prog_time:9;
    lockbits:('','LB1','LB2','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$91; b2:$01;
    name:'AT90S2313';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_STD;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB1','LB2','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$91; b2:$02;
    name:'AT90S2323';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_2323;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('FSTRT','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$91; b2:$03;
    name:'AT90S2343';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_2323;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('RCEN','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$91; b2:$05;
    name:'AT90S2333';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_2333;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','BODEN','BODLVL','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$92; b2:$01;
    name:'AT90S4414';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_STD;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$92; b2:$02;
    name:'AT90S4433';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_2333;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','BODEN','BODLVL','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$92; b2:$03;
    name:'AT90S4434';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_STD;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$93; b2:$01;
    name:'AT90S8515';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_STD;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$93; b2:$03;
    name:'AT90S8535';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_STD;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB2','LB1','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$93; b2:$81;
    name:'AT90PWM2/3';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('BOOTRST','BOOTSZ0','BOOTSZ1','','PSCRV','PSC0RB','PSC1RB','PSC2RB')),

    (b0:$1E; b1:$93; b2:$83;
    name:'AT90PWM2B/3B';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('BOOTRST','BOOTSZ0','BOOTSZ1','','PSCRV','PSC0RB','','PSC2RB')),

    (b0:$1E; b1:$94; b2:$82;
    name:'AT90USB82/162';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:9;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','RSTDSBL','DWEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','HWBE','','','','')),

    (b0:$1E; b1:$95; b2:$81;
    name:'AT90CAN32';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('TA0SEL','BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','')),

    (b0:$1E; b1:$96; b2:$81;
    name:'AT90CAN64';
    proctype:PROC_TYPE_AVR;
    fsize:65536; esize:2048; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('TA0SEL','BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','')),

    (b0:$1E; b1:$96; b2:$82;
    name:'AT90USB646/647';
    proctype:PROC_TYPE_AVR;
    fsize:65536; esize:2048; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','HWBE','','','','')),

    (b0:$1E; b1:$97; b2:$81;
    name:'AT90CAN128';
    proctype:PROC_TYPE_AVR;
    fsize:131072; esize:4096; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('TA0SEL','BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','')),

    (b0:$1E; b1:$97; b2:$82;
    name:'AT90USB1286/1287';
    proctype:PROC_TYPE_AVR;
    fsize:131072; esize:4096; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','HWBE','','','',''))

