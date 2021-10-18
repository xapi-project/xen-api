# SUMMARY:
# Run python unittests using nose

set -uex

sudo apt-get install -y python-mock python-nose

nosetests scripts
