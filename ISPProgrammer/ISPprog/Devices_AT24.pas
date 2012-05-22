    // AT24Cxx serial I2C EEPROM memories

    (b0:$a0; b1:$24; b2:$01;
    name:'AT24C01';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:128; usigsize:0; fpage:0; fpagesize:0; epage:3; epagesize:8;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$02;
    name:'AT24C02';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:256; usigsize:0; fpage:0; fpagesize:0; epage:3; epagesize:8;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$04;
    name:'AT24C04';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:512; usigsize:0; fpage:0; fpagesize:0; epage:4; epagesize:16;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$08;
    name:'AT24C08';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:1024; usigsize:0; fpage:0; fpagesize:0; epage:4; epagesize:16;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$16;
    name:'AT24C16';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:2048; usigsize:0; fpage:0; fpagesize:0; epage:4; epagesize:16;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$32;
    name:'AT24C32';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:4096; usigsize:0; fpage:0; fpagesize:0; epage:5; epagesize:32;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$64;
    name:'AT24C64';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:8192; usigsize:0; fpage:0; fpagesize:0; epage:5; epagesize:32;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$28;
    name:'AT24C128';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:16384; usigsize:0; fpage:0; fpagesize:0; epage:6; epagesize:64;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$56;
    name:'AT24C256';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:32768; usigsize:0; fpage:0; fpagesize:0; epage:6; epagesize:64;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$12;
    name:'AT24C512';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:65536; usigsize:0; fpage:0; fpagesize:0; epage:7; epagesize:128;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','','')),

    (b0:$a0; b1:$24; b2:$24;
    name:'AT24C1024';
    proctype:PROC_TYPE_I2C_BUS;
    fsize:0; esize:131072; usigsize:0; fpage:0; fpagesize:0; epage:8; epagesize:256;
    osccal:0;
    algo:ALGO_AT24_EEPROM;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_NONE;
    algo_busy:ALGO_BUSY_AT24_EEPROM;
    prog_time:5;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','',''))
