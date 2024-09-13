# Proxy Proposal   

Proxy Proposal is an open-source smart contract solution on the Cardano blockchain that allows users to submit governance proposals without needing the full 100k ADA deposit. Instead, proposers can lease the deposit from others who have locked their ADA in the contract, making the proposal submission process more accessible and affordable.

Key Features
Deposit Locking: Users with 100k ADA can lock their ADA in the smart contract and set a price for leasing it.
Proposal Leasing: Individuals who want to submit governance proposals, but lack the 100k ADA deposit, can pay a fee to lease the locked deposit.
Secure Refunds: After the proposal is submitted and the governance process is complete (whether the proposal passes or fails), the original deposit of 100k ADA is returned to the user who locked it, while they keep the leasing fee.

# Set up nix config 
Put the following lines in your nix configuration file (usually located at /etc/nix/nix.conf)

extra-experimental-features = nix-command flakes ca-derivations
extra-trusted-substituters = https://cache.iog.io https://cache.nixos.org/ https://public-plutonomicon.cachix.org https://cache.zw3rk
extra-trusted-public-keys = cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=

# Installation 
After setting up nix config, restart your computer or VM. 
Then run:
    nix develop 

# License
See the [LICENSE](LICENSE) file for license rights and limitations (MIT).