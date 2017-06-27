from setuptools import setup
from setuptools.command.sdist import sdist

setup(name='XenAPI',
      version='1.2',
      description='XenAPI SDK, for communication with Citrix XenServer and Xen Cloud Platform.',
      author='Citrix Systems, Inc.',
      author_email='discard@citrix.com',
      url='http://community.citrix.com/display/xs/Download+SDKs',
      py_modules=['XenAPI'],
      classifiers=[
          'License :: OSI Approved :: GNU Library or Lesser General Public License (LGPL)',
          'Development Status :: 6 - Mature',
          'Topic :: Software Development :: Libraries :: Python Modules',
          ])
