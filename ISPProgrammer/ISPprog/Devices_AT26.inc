    // AT26F serial flash memories

    (b0:$3D; b1:$1F; b2:$04;
    name:'AT26F004';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:524288; esize:0; usigsize:0; fpage:0; fpagesize:1; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:10000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','',''))
