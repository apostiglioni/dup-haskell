# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure(2) do |config|
  config.vm.provider "virtualbox" do |vb|
    vb.memory = "2048"    # Customize the amount of memory on the VM
  end

  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  config.vm.box = 'ubuntu/trusty64'

  config.vm.synced_folder "provision", "/vagrant/provision"
  config.vm.synced_folder "workspace", "/home/vagrant/workspace"

  config.vm.provision "haskell stack", type: "shell", privileged: true, inline: %(
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442

    echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list
    apt-get update && apt-get install -y stack
  )

  config.vm.provision "dev tools", type: "shell", privileged: false, inline: %(
    stack setup
    stack install hlint ghc-mod
    grep -qF 'export PATH=$PATH:~/.local/bin' ~/.bashrc || echo 'export PATH=$PATH:~/.local/bin' >> ~/.bashrc

    sudo apt-get install -y git vim
    git clone --recursive https://github.com/sdiehl/haskell-vim-proto.git 
    cd haskell-vim-proto/vim/bundle/vimproc.vim && make && cd - 
    ln -sf haskell-vim-proto/vimrc ~/.vimrc 
    ln -sf haskell-vim-proto/vim ~/.vim

    ## Install Undo plugin
    cd ~/.vim/bundle && git clone http://github.com/sjl/gundo.vim.git && cd -
    grep -qF 'nnoremap <F5> :GundoToggle<CR>' ~/.vimrc || echo 'nnoremap <F5> :GundoToggle<CR>' >> ~/.vimrc

    ## Install Airline plugin
    cd ~/.vim/bundle && git clone https://github.com/vim-airline/vim-airline && vim -u NONE -c "helptags vim-airline/doc" -c q &>/dev/null && cd -
    # configure airline plugin
    grep -qF '"Enable vim-airline' ~/.vimrc || echo '"Enable vim-airline' >> ~/.vimrc
    grep -qF 'let g:airline#extensions#tabline#enabled = 1' ~/.vimrc || echo 'let g:airline#extensions#tabline#enabled = 1' >> ~/.vimrc
    grep -qF 'set laststatus=2' ~/.vimrc || echo 'set laststatus=2' >> ~/.vimrc

    ## Install Vim fugitive 
    cd ~/.vim/bundle && git clone git://github.com/tpope/vim-fugitive.git && vim -u NONE -c "helptags vim-fugitive/doc" -c q &>/dev/null && cd -

    ## Install Git Gutter
    cd ~/.vim/bundle && git clone git://github.com/airblade/vim-gitgutter.git && cd -

    ## Install Vim Better Whitespace Plugin
    git clone git://github.com/ntpeters/vim-better-whitespace.git ~/.vim/bundle/vim-better-whitespace 

    ## Unset mouse
    sed -i '/set mouse/d' ~/.vimrc

    grep -qF 'export VISUAL=vim' ~/.bashrc || echo 'export VISUAL=vim' >> ~/.bashrc
    grep -qF 'cd workspace/dup' ~/.bashrc || echo 'cd workspace/dup' >> ~/.bashrc
  )

#  config.vm.provision "haskell-platform", type: "shell", privileged: true, inline: "apt-get install -y haskell-platform"
#  config.vm.provision "dev-tools", type: "shell", privileged: true, inline: %(
#    apt-get install -y git
#   
#     ## IDE (Vim + plugins)
#    apt-get install vim
#    curl -L -o /tmp/haskellmode.vba http://projects.haskell.org/haskellmode-vim/vimfiles/haskellmode-20101118.vba 2> /dev/null
#    vim -c 'so % | q' /tmp/haskellmode.vba &>/dev/null
#  )
#  config.vm.provision "haskell-stack", type: "shell", privileged: true, inline: %(
#    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
##    echo 'deb http://download.fpcomplete.com/ubuntu xenial main'|sudo tee /etc/apt/sources.list.d/fpco.list
##    echo 'deb http://download.fpcomplete.com/ubuntu wily main'|sudo tee /etc/apt/sources.list.d/fpco.list
#    echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list
#    sudo apt-get update && sudo apt-get install stack -y
#  )
#
#  config.vm.provision "dev-tools", type: "shell", privileged: true, inline: %(
#    apt-get install -y git vim exuberant-ctags libcurl4-gnutls-dev
#    bash <(curl -sL https://git.io/haskell-vim-now)
#  )


  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  # config.vm.provider "virtualbox" do |vb|
  #   # Display the VirtualBox GUI when booting the machine
  #   vb.gui = true
  #
  #   # Customize the amount of memory on the VM:
  #   vb.memory = "1024"
  # end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  # config.vm.provision "shell", inline: <<-SHELL
  #   sudo apt-get update
  #   sudo apt-get install -y apache2
  # SHELL
end