# SUMMARY:
# Run python unittests using nose

set -uex

sudo add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu/ trusty main universe"
sudo apt-get update
sudo apt-get install -y python-mock python-nose

nosetests scripts 
