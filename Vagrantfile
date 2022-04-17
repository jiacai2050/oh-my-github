# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.ssh.username = "root"
  config.ssh.insert_key = false
  config.ssh.extra_args = ["-o", "ControlMaster=auto",
                           "-o", "ControlPath=~/.ssh/vagrant-%C",
                           "-o", "ControlPersist=1h"]

  config.vm.provision :shell, inline: "sudo apt install -y libcurl4-openssl-dev pkg-config libjansson-dev libsqlite3-dev valgrind"

  config.vm.define :dev, primary: true do |node|
    node.vm.network :private_network, ip: "192.168.33.11"
    node.vm.provider :docker do |d|
      d.name = "c-github-client"
      d.image = "devbox:v2"
      d.has_ssh = true
    end
  end
end
