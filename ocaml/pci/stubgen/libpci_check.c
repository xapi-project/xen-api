#include <pci/pci.h>

int main() {
  /* Check we have >= 3.2.0 */
  return !(PCI_LIB_VERSION >= 0x030200);
}
