from setuptools import setup

VERSION="0.0.1"
setup(
    name='maro',
    version=VERSION,
    description='ML Automated Remediation Oracle',
    url='https://github.ibm.com/ai-debugging/maro',
    license='IBM',
    packages=['maro'],
    install_requires=[
        'lale',
        'jsonschema',
        'numpy',
        'ipython',
        'requests'
    ],
    zip_safe=False
)
