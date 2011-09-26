: ec@
  dup 65 ic!
  8 >> 66 ic! ( little-endian)
  1 63 ic! ( start the eeprom read)
  64 ic@ ( read the eeprom)
;
