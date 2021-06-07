# SUMMARY:
# Run python unittests using nose

set -uex

sudo add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu/ trusty main universe"
sudo apt-get update
sudo apt-get install -y python3-mock python3-nose
pip3 install mock

nosetests3 scripts scripts/examples scripts/examples/python
PYTHONPATH=scripts/plugins python3 -m unittest -v test_extauth_hook_AD
