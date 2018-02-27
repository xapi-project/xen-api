from distutils.core import setup
setup(name="xapi-storage",
      version='0.1',
      description='Xapi storage interface',
      author='David Scott',
      author_email='dave@recoil.org',
      url='https://github.com/xapi-project/xapi-storage/',
      packages=['xapi', 'xapi.storage', 'xapi.storage.api',
                'xapi.storage.api.v4'],
      )
