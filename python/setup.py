from setuptools import setup

setup(
    name="nlcor",
    version="0.0.1",
    author="Chitta Ranjan, Vahab Najari",
    author_email="cranjan@processminer.com, vnajari@processminer.com",
    description="Nlcor v2.x uses a dynamic partitioning approach with adaptive segmentation for a more precise nonlinear correlation estimation.",
    long_description_content_type="text/markdown",
    url="https://github.com/ProcessMiner/nlcor",
    classifiers=[
        "Programming Language :: Python :: 3.6",
        "License :: OSI Approved :: GNU General Public License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
    install_requires = [
        "numpy ~= 1.20",
        "pandas ~= 1.2",
        "scipy ~= 1.62",
        ],
   )

