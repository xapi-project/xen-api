# SUMMARY:
# Run python unittests using pytest

set -uex

sudo add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu/ trusty main universe"
sudo apt-get update
sudo apt-get install -y python3-mock python3-pytest
pip3 install mock

pytest-3 scripts
