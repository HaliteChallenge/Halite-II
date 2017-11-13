from setuptools import setup

setup(name='hlt_client',
      version='1.0.2',
      description='Client for interacting with Halite II',
      author='Two Sigma',
      author_email='halite@halite.io',
      url='https://github.com/HaliteChallenge/Halite-II',
      license='MIT',
      packages=['hlt_client'],
      scripts=['bin/hlt'],
      classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'Topic :: Software Development :: Build Tools',
        'Programming Language :: Python :: 3',
      ],
      python_requires='>=3',
      install_requires=[
        'requests',
        'zstd',
      ],
      zip_safe=False)
