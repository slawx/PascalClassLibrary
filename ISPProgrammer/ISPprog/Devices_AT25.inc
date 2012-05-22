    // AT25DF/AT26DF/AT25FS serial flash memories

    (b0:$3D; b1:$1F; b2:$43;
    name:'AT25DF021';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:262144; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:3500;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$3D; b1:$1F; b2:$44;
    name:'AT25DF041A';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:524288; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:7000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$3D; b1:$1F; b2:$45;
    name:'AT25/26DF081';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:1048576; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:14000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$3D; b1:$1F; b2:$46;
    name:'AT25/26DF161';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:2097152; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:28000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$3D; b1:$1F; b2:$47;
    name:'AT25DF321';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:4194304; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:56000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$3D; b1:$1F; b2:$48;
    name:'AT25DF641';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:8388608; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:112000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$3D; b1:$1F; b2:$66;
    name:'AT25FS010';
    proctype:PROC_TYPE_DATAFLASH;
    fsize:131072; esize:0; usigsize:0; fpage:8; fpagesize:1024; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_SERIALFLASH;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_SERIALFLASH;
    prog_time:4000;  // max. chip erase time
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','',''))
