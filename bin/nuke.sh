#!/bin/bash

# Script to remove everything that Minikube adds to a system when it sets the hypervisor = 'none'

if [[ $UID != 0 ]]
then
        echo Must run as root
        exit 1
fi

# Point home at the user's home area if they are using sudo so the files there are processed.
[[ -n $SUDO_USER ]] && HOME=$(eval echo "~$SUDO_USER")

minikube delete
kubectl config delete-cluster minikube
kubectl config delete-context minikube
systemctl stop kubelet.service
mount | awk '/kubelet/ {print $3}' | xargs -i -n 1 umount {}

rm -rf \
        "$HOME/.minikube" \
        /bin/kubeadm \
        /bin/kubelet \
        /etc/kubernetes \
        /etc/systemd/system/multi-user.target.wants/kubelet.service \
        /etc/systemd/system/kubelet.service.d \
        /lib/systemd/system/kubelet.service \
        /var/lib/minikube \
        /var/lib/kubelet \
        /tmp/juju-* \
        /tmp/hostpath-provisioner/*

systemctl daemon-reload
