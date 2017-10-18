from setuptools import setup

setup(name='hlt_client',
      version='1.0',
      description='Client for interacting with Halite II',
      author='Two Sigma',
      author_email='halite@halite.io',
      license='MIT',
      packages=['hlt_client'],
	  install_requires=[
		  'requests',
          'zstd',
	  ],
      zip_safe=False)
