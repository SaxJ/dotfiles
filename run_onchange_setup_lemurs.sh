#! /bin/sh
sudo mkdir -p /etc/lemurs/wms
printf '#! /bin/sh\nexec spectrwm' | sudo tee /etc/lemurs/wms/spectrwm
sudo chmod +x /etc/lemurs/wms/spectrwm
