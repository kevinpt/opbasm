import sys, io

try:
  from setuptools import setup
except ImportError:
	sys.exit('ERROR: setuptools is required.\nTry using "pip install setuptools".')


# use README.rst for the long description
with io.open('README.rst', encoding='utf-8') as fh:
    long_description = fh.read()
    
# Scan the main package for the version string
version_file = 'opbasm/opbasm.py'
version = None
with io.open(version_file, encoding='utf-8') as fh:
    try:
        version = [line.split('=')[1].strip().strip("'") for line in fh if \
            line.startswith('__version__')][0]
    except IndexError:
        pass

if version is None:
    raise RuntimeError('Unable to find version string in file: {0}'.format(version_file))


setup(name='opbasm',
    version=version,
    author='Kevin Thibedeau',
    author_email='kevin.thibedeau@gmail.com',
    url='http://kevinpt.github.io/opbasm',
    download_url='http://kevinpt.github.io/opbasm',
    description='Open Picoblaze Assembler',
    long_description=long_description,
    platforms = ['Any'],
    install_requires = [],
    packages = ['opbasm', 'templates', 'test'],
    py_modules = ['pb_update'],
    entry_points = {
        'console_scripts': ['opbasm = opbasm.__main__:main', 'pb_update = pb_update:main']
    },
    include_package_data = True,

    keywords='Picoblaze assembler',
    license='MIT',
    classifiers=['Development Status :: 5 - Production/Stable',
        'Operating System :: OS Independent',
        'Intended Audience :: Developers',
        'Topic :: Software Development :: Assemblers',
        'Topic :: Software Development :: Embedded Systems',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Assembly',
        'License :: OSI Approved :: MIT License'
        ]
    )
