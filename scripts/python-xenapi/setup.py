from distutils.core import setup

setup(name = "XenAPI",
    version = "1.0",
    description = "Python Bindings for the XenAPI",
    author = "Ewan Mellor",
    author_email = "firstname.lastname@citrix.com",
    url = "http://www.citrix.com",
    packages = [''],
    package_data = {'package' : ["XenAPI.py"] },
    long_description = """Python Bindings for the XenAPI""" ,
    classifiers = [
	'Development Status :: 5 - Production/Stable',
	'Environment :: Console',
	'Intended Audience :: Developers',
	'License :: OSI Approved :: GNU General Public License (GPL)',
	'Operating System :: POSIX :: Linux',
	'Programming Language :: Python',
	'Topic :: Software Development :: Object Brokering'
    ]
) 
