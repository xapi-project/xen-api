from distutils.core import setup

setup(name = "XenAPI-utils",
    version = "1.0",
    description = "Python helper libraries for the XenAPI server",
    url = "http://www.citrix.com",
    py_modules = ['XenAPIPlugin','inventory'],
    long_description = "Python Bindings for the XenAPI" ,
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
