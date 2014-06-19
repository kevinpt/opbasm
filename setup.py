#import setuptools
from setuptools import setup

# use README.txt for the long description
with open('README.txt') as fh:
    long_description = fh.read()
    
# scan the script for the version string
version_file = 'opbasm.py'
version = None
with open(version_file) as fh:
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
    url='http://code.google.com/p/opbasm/',
    download_url='http://code.google.com/p/opbasm/',
    description='Open Picoblaze Assembler',
    long_description=long_description,
    install_requires = ['pyparsing >= 1.5.6'],
    packages = ['opbasm_lib', 'templates'],
    py_modules = ['opbasm', 'pb_update'],
    entry_points = {
        'console_scripts': ['opbasm = opbasm:main', 'pb_update = pb_update:main']
    },
    include_package_data = True,
    
    keywords='Picoblaze assembler',
    license='LGPLv3',
    classifiers=['Development Status :: 5 - Production/Stable',
        'Operating System :: OS Independent',
        'Intended Audience :: Developers',
        'Natural Language :: English',
        'Programming Language :: Python :: 2',
        'Programming Language :: Assembly',
        'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
        'Topic :: Utilities'
        ]
    )
