qemu-system-aarch64 \
  -m 4096 \
  -cpu cortex-a72 -smp 2,cores=2,threads=1,sockets=1 -M virt \
  -bios ./QEMU_EFI.fd \
  -device VGA -device nec-usb-xhci -device usb-mouse -device usb-kbd \
  -net nic -net tap \
  -boot d ./kylin-v10-aarch64.qcow2

