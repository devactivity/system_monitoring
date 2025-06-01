#include <stdio.h>
#include <string.h>

void get_disk_io(double *read, double *write) {
  FILE *fp = fopen("/proc/diskstats", "r");
  if (!fp) return;

  char line[256];
  double total_read = 0, total_write = 0;

  while (fgets(line, sizeof(line), fp)) {
    if (strstr(line, "nvme0n1 ")) {
      unsigned long long reads, writes;
      sscanf(line, "%*d %*d %*s %llu %*u %*u %*u %llu", &reads, &writes);
      total_read += reads;
      total_write += writes;
    }
  }

  fclose(fp);
  *read = total_read;
  *write = total_write;
}
