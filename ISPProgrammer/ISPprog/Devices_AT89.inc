    (b0:$1E; b1:$73; b2:$ff;
    name:'AT89S8253';
    proctype:PROC_TYPE_S8253;
    fsize:12288; esize:2048; usigsize:64; fpage:6; fpagesize:64; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89S8253;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:5;
    lockbits:('LB1','LB2','LB3','','','','','');
    fusebitslo:('SerPrgDis','x2 ClkDis','UsrPrgDis','CrystalEn','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$23; b2:$ff;
    name:'AT89S2051';
    proctype:PROC_TYPE_S2051;
    fsize:2048; esize:0; usigsize:32; fpage:5; fpagesize:32; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89S2051;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:5;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('ISP Enable','x2 Clock','UserRowPrg','XTALBypass','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$43; b2:$ff;
    name:'AT89S4051';
    proctype:PROC_TYPE_S2051;
    fsize:4096; esize:0; usigsize:32; fpage:5; fpagesize:32; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89S2051;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:5;
    lockbits:('LB1','LB2','','','','','','');
    fusebitslo:('ISP Enable','x2 Clock','UserRowPrg','XTALBypass','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$51; b2:$06;
    name:'AT89S51';
    proctype:PROC_TYPE_NEW51;
    fsize:4096; esize:0; usigsize:0; fpage:8; fpagesize:256; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89S51;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:5;
    lockbits:('','','LB1','LB2','LB3','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$52; b2:$06;
    name:'AT89S52';
    proctype:PROC_TYPE_NEW51;
    fsize:8192; esize:0; usigsize:0; fpage:8; fpagesize:256; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89S51;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:5;
    lockbits:('','','LB1','LB2','LB3','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$61; b2:$06;
    name:'AT89LS51';
    proctype:PROC_TYPE_NEW51;
    fsize:4096; esize:0; usigsize:0; fpage:8; fpagesize:256; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89S51;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:5;
    lockbits:('','','LB1','LB2','LB3','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$1E; b1:$62; b2:$06;
    name:'AT89LS52';
    proctype:PROC_TYPE_NEW51;
    fsize:8192; esize:0; usigsize:0; fpage:8; fpagesize:256; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89S51;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:5;
    lockbits:('','','LB1','LB2','LB3','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','',''))
