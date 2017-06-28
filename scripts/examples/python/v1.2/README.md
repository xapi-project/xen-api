# Provides a python3 XenAPI (v1.2)

Original source
* XenAPI.py: from `scripts/examples/python/XenAPI.py`
* setup.py: from official v1.2 [package](https://pypi.python.org/pypi/XenAPI)

To use, update your `requirements.txt` with something like this:
```
# Python 2.x can use the official version
XenAPI>=1.2; python_version < '3.0'
# Grab equivalent that is py35 compatible
git+git://github.com/timkuik/xen-api@master#egg=xen-api&subdirectory=scripts/examples/python/v1.2; python_version >= '3.0'
```
