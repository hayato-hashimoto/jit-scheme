main() {
int num=1;
int succ_bin[] = {
 195
};
  int (*succ)();
  succ = &succ_bin;
  succ (&num);
  printf("%d\n", num);
}
