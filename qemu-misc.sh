## Create qcow2 image
qemu-img create -f qcow2 kylin-v10-aarch64.qcow2 50G


## NAT: https://wiki.qemu.org/Documentation/Networking/NAT

## Install OS from ISO

qemu-system-aarch64 \
  -m 4096 \
  -cpu cortex-a72 -smp 2,cores=2,threads=1,sockets=1 -M virt \
  -bios ./QEMU_EFI.fd \
  -device VGA -device nec-usb-xhci -device usb-mouse -device usb-kbd \
  -net nic -net tap \
  -cdrom /home/wallace/Downloads/Kylin-Server-10-SP2-Release-Build09-20210524-arm64.iso \
  -boot d ./kylin-v10-aarch64.qcow2


## Start the machine

qemu-system-aarch64 \
  -m 4096 \
  -cpu cortex-a72 -smp 2,cores=2,threads=1,sockets=1 -M virt \
  -bios ./QEMU_EFI.fd \
  -device VGA -device nec-usb-xhci -device usb-mouse -device usb-kbd \
  -net nic -net tap \
  -boot d ./kylin-v10-aarch64.qcow2

