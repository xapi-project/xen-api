# SUMMARY:
# Run python 2 unittests using pytest

set -uex

sudo add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu/ trusty main universe"
sudo apt-get update
sudo apt-get install -y python-mock python-pytest
curl https://bootstrap.pypa.io/pip/2.7/get-pip.py |sudo python2
pip install enum

pytest scripts
