# SUMMARY:
# Run python unittests using nose

set -uex

sudo apt-get update
sudo apt-get install python2
curl https://bootstrap.pypa.io/pip/2.7/get-pip.py | sudo python2
pip install enum nose mock

nosetests scripts scripts/examples scripts/examples/python
PYTHONPATH=scripts/plugins python2 -m unittest -v test_extauth_hook_AD
