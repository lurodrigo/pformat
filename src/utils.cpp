
char count_utf8(const char *str) {
  unsigned char mask[] = {192, 224, 240};
  char i = 1;

  if ((*str & mask[0]) == mask[0]) i++;
  if ((*str & mask[1]) == mask[1]) i++;
  if ((*str & mask[2]) == mask[2]) i++;
  
  return i;
}