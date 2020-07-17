import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="PyTEESD",
    version="0.0.1",
    author="Mike Campbell",
    author_email="mtcampbe@illinois.edu",
    description="Run integrated testing on HPC platforms.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/illinois-ceesd/teesd",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
