    { only high voltage programming
    (b0:$1E; b1:$90; b2:$04;
    name:'ATtiny11';
    proctype:PROC_TYPE_AVR;
    fsize:1024; esize:0; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_TINY;
    algo_busy:ALGO_BUSY_WAIT;
    prog_time:16;
    lockbits:('','LB1','LB2','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','RSTDISBL','FSTRT','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),
    }

    (b0:$1E; b1:$90; b2:$05;
    name:'ATtiny12';
    proctype:PROC_TYPE_AVR;
    fsize:1024; esize:64; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_TWICE;
    algo_lb:ALGO_LB_TINY;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:7;
    lockbits:('','LB1','LB2','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','RSTDISBL','SPIEN','BODEN','BODLEVEL');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$90; b2:$06;
    name:'ATtiny15';
    proctype:PROC_TYPE_AVR;
    fsize:1024; esize:64; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_TINY;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('','LB1','LB2','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','','','RSTDISBL','SPIEN','BODEN','BODLEVEL');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$90; b2:$07;
    name:'ATtiny13';
    proctype:PROC_TYPE_AVR;
    fsize:1024; esize:64; usigsize:0; fpage:4+1; fpagesize:32; epage:0; epagesize:0;
    osccal:2;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','SUT0','SUT1','CKDIV8','WDTON','EESAVE','');
    fusebitshi:('RSTDISBL','BODLEVEL0','BODLEVEL1','DWEN','SELFPRGEN','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$91; b2:$08;
    name:'ATtiny25';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:4+1; fpagesize:32; epage:0; epagesize:0;
    osccal:2;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$91; b2:$09;
    name:'ATtiny26';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:4+1; fpagesize:32; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOPT','PLLCK');
    fusebitshi:('BODEN','BODLEVEL','EESAVE','','RSTDISBL','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$91; b2:$0a;
    name:'ATtiny2313';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:4+1; fpagesize:32; epage:0; epagesize:0;
    osccal:2;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('RSTDISBL','BODLEVEL0','BODLEVEL1','BODLEVEL2','WDTON','','EESAVE','DWEN');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$91; b2:$0b;
    name:'ATtiny24';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:4+1; fpagesize:32; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$91; b2:$0c;
    name:'ATtiny261';
    proctype:PROC_TYPE_AVR;
    fsize:2048; esize:128; usigsize:0; fpage:4+1; fpagesize:32; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$92; b2:$06;
    name:'ATtiny45';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:2;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$92; b2:$07;
    name:'ATtiny44';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$92; b2:$08;
    name:'ATtiny461';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$92; b2:$09;
    name:'ATtiny48';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:64; usigsize:0; fpage:5+1; fpagesize:64; epage:2; epagesize:4;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:9;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','','','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$93; b2:$0b;
    name:'ATtiny85';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:2;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$93; b2:$0c;
    name:'ATtiny84';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$93; b2:$0d;
    name:'ATtiny861';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$93; b2:$11;
    name:'ATtiny88';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:64; usigsize:0; fpage:5+1; fpagesize:64; epage:2; epagesize:4;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:9;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','','','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','',''))

