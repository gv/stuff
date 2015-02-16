fdisk /dev/sda
mkfs.ext4 /dev/sda1
mount /dev/sda1 /mnt
pacstrap /mnt base
echo ball > /mnt/etc/hostname
ln -sf /usr/share/zoneinfo/Asia/Baghdad /mnt/etc/localtime
arch-chroot /mnt
