    // AT25F serial flash memories

    (b0:$25; b1:$1F; b2:$65;
    name:'AT25F512';
    proctype:PROC_TYPE_SERIALFLASH;
    fsize:65536; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:2200;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$25; b1:$1F; b2:$60;
    name:'AT25F1024';
    proctype:PROC_TYPE_SERIALFLASH;
    fsize:131072; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:4400;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$25; b1:$1F; b2:$63;
    name:'AT25F2048';
    proctype:PROC_TYPE_SERIALFLASH;
    fsize:262144; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:4000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$25; b1:$1F; b2:$64;
    name:'AT25F4096';
    proctype:PROC_TYPE_SERIALFLASH;
    fsize:524288; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:8000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','',''))
