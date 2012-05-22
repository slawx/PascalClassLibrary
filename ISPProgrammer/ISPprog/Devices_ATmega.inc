    (b0:$1E; b1:$92; b2:$05;
    name:'ATmega48';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$92; b2:$0a;
    name:'ATmega48P';
    proctype:PROC_TYPE_AVR;
    fsize:4096; esize:256; usigsize:0; fpage:5+1; fpagesize:64; epage:2; epagesize:4;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:5;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('SELFPRGEN','','','','','','','')),

    (b0:$1E; b1:$93; b2:$06;
    name:'ATmega8515';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','CKOPT','','WDTON','S8515C');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$93; b2:$07;
    name:'ATmega8';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','CKOPT','','WDTON','RSTDISBL');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$93; b2:$08;
    name:'ATmega8535';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','CKOPT','','WDTON','S8535C');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$93; b2:$0a;
    name:'ATmega88';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('BOOTRST','BOOTSZ0','BOOTSZ1','','','','','')),

    (b0:$1E; b1:$93; b2:$0f;
    name:'ATmega88P';
    proctype:PROC_TYPE_AVR;
    fsize:8192; esize:512; usigsize:0; fpage:5+1; fpagesize:64; epage:2; epagesize:4;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:5;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('BOOTRST','BOOTSZ0','BOOTSZ1','','','','','')),

    (b0:$1E; b1:$94; b2:$01;
    name:'ATmega161';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','','SUT','','BOOTRST','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$94; b2:$02;
    name:'ATmega163';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','','','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$94; b2:$03;
    name:'ATmega16';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','CKOPT','','JTAGEN','OCDEN');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$94; b2:$04;
    name:'ATmega162';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('','BODLEVEL0','BODLEVEL1','BODLEVEL2','M161C','','','')),

    (b0:$1E; b1:$94; b2:$05;
    name:'ATmega165/169';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('','BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','')),

    (b0:$1E; b1:$94; b2:$06;
    name:'ATmega168';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('BOOTRST','BOOTSZ0','BOOTSZ1','','','','','')),

    (b0:$1E; b1:$94; b2:$07;
    name:'ATmega165P';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','')),

    (b0:$1E; b1:$94; b2:$0a;
    name:'ATmega164P';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$94; b2:$0b;
    name:'ATmega168P';
    proctype:PROC_TYPE_AVR;
    fsize:16384; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:2; epagesize:4;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:5;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('BOOTRST','BOOTSZ0','BOOTSZ1','','','','','')),

    (b0:$1E; b1:$95; b2:$01;
    name:'ATmega323';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','','','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','','','JTAGEN','OCDEN');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$95; b2:$02;
    name:'ATmega32';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','CKOPT','','JTAGEN','OCDEN');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$95; b2:$03;
    name:'ATmega329';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    (b0:$1E; b1:$95; b2:$04;
    name:'ATmega3290';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    (b0:$1E; b1:$95; b2:$05;
    name:'ATmega325';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    (b0:$1E; b1:$95; b2:$06;
    name:'ATmega3250';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    { only parallel and JTAG programming
    (b0:$1E; b1:$95; b2:$07;
    name:'ATmega406';
    proctype:PROC_TYPE_AVR;
    fsize:40960; esize:512; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL','SUT0','SUT1','BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON');
    fusebitshi:('JTAGEN','OCDEN','','','','','','');
    fusebitsext:('','','','','','','','')),
    }

    (b0:$1E; b1:$95; b2:$08;
    name:'ATmega324P';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$95; b2:$0f;
    name:'ATmega328P';
    proctype:PROC_TYPE_AVR;
    fsize:32768; esize:1024; usigsize:0; fpage:6+1; fpagesize:128; epage:2; epagesize:4;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:5;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BODLEVEL0','BODLEVEL1','BODLEVEL2','EESAVE','WDTON','','DWEN','RSTDISBL');
    fusebitsext:('BOOTRST','BOOTSZ0','BOOTSZ1','','','','','')),

    (b0:$1E; b1:$96; b2:$02;
    name:'ATmega64';
    proctype:PROC_TYPE_AVR;
    fsize:65536; esize:2048; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','CKOPT','','JTAGEN','OCDEN');
    fusebitsext:('WDTON','M103C','','','','','','')),

    (b0:$1E; b1:$96; b2:$03;
    name:'ATmega649';
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
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    (b0:$1E; b1:$96; b2:$04;
    name:'ATmega6490';
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
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    (b0:$1E; b1:$96; b2:$05;
    name:'ATmega645';
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
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    (b0:$1E; b1:$96; b2:$06;
    name:'ATmega6450';
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
    fusebitsext:('RSTDISBL','BODLEVEL0','BODLEVEL1','','','','','')),

    (b0:$1E; b1:$96; b2:$08;
    name:'ATmega640';
    proctype:PROC_TYPE_AVR;
    fsize:65536; esize:4096; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$96; b2:$09;
    name:'ATmega644';
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
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$96; b2:$0a;
    name:'ATmega644P';
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
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$97; b2:$02;
    name:'ATmega128';
    proctype:PROC_TYPE_AVR;
    fsize:131072; esize:4096; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:4;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_FF;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','BODEN','BODLEVEL');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','CKOPT','','JTAGEN','OCDEN');
    fusebitsext:('WDTON','M103C','','','','','','')),

    (b0:$1E; b1:$97; b2:$03;
    name:'ATmega1280';
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
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$97; b2:$04;
    name:'ATmega1281';
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
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$97; b2:$05;
    name:'ATmega1284P';
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
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$98; b2:$01;
    name:'ATmega2560';
    proctype:PROC_TYPE_AVR;
    fsize:262144; esize:4096; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','','')),

    (b0:$1E; b1:$98; b2:$02;
    name:'ATmega2561';
    proctype:PROC_TYPE_AVR;
    fsize:262144; esize:4096; usigsize:0; fpage:7+1; fpagesize:256; epage:0; epagesize:0;
    osccal:1;
    algo:ALGO_MEGA;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_MEGA;
    algo_busy:ALGO_BUSY_POLL_RDYBSY;
    prog_time:16;
    lockbits:('LB1','LB2','BLB01','BLB02','BLB11','BLB12','','');
    fusebitslo:('CKSEL0','CKSEL1','CKSEL2','CKSEL3','SUT0','SUT1','CKOUT','CKDIV8');
    fusebitshi:('BOOTRST','BOOTSZ0','BOOTSZ1','EESAVE','WDTON','','JTAGEN','OCDEN');
    fusebitsext:('BODLEVEL0','BODLEVEL1','BODLEVEL2','','','','',''))
