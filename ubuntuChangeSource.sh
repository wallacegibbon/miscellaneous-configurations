sudo cp /etc/apt/sources.list /etc/apt/sources.list.bak
sudo sed -i s/security.ubuntu/mirrors.aliyun/g /etc/apt/sources.list
sudo sed -i s/archive.ubuntu/mirrors.aliyun/g /etc/apt/sources.list

