# SUMMARY:
# Run python unittests using nose

set -uex

sudo add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu/ trusty main universe"
sudo apt-get update
sudo apt-get install -y python-mock python-nose
curl https://bootstrap.pypa.io/pip/2.7/get-pip.py |sudo python2
pip install enum

nosetests scripts scripts/examples scripts/examples/python
PYTHONPATH=scripts/plugins python2 -m unittest -v test_extauth_hook_AD
