# Creating Docker Container for Developement Environment.

We propose a docker container to be used for package developement. The built VMs are distributed through Docker Hub ([dncr/rstudio-server](https://hub.docker.com/repository/docker/dncr/rstudio-server/general)) and source codes are available through GitHub ([https://github.com/dncR/rstudio-server](https://github.com/dncR/rstudio-server)). The docker container is built upon **Linux Operating System** (particularly, Ubuntu Jammy) with pre-installed **R and RStudio Server**, and **TeX (using Tex Live distribution)**. All the arguments to be used while *pulling*, *building*, or *creating* docker image/container are given with an environment file **.env**. One may change the value of corresponding arguments to customize Docker Environment. See below for more details on **.env** file.

## Working with .env file

This file includes argument used to customize Docker images and/or containers. The container comes with **Ubuntu Jammy** distribution, supporting for both platform **ARM (e.g., Apple's silicon chips)** and **AMD (e.g., Intel-based chips)**. 

* **ARCH**: The architecture of Ubuntu installation, e.g., `arm64`, `amd64`, etc.
* **UBUNTU_VERSION**: The codename of Ubuntu distro. Default is `jammy`.
* **TEX_SUFFIX**: The installation type of TeX distribution. Set '-texlive' or '-texlive-full' for base and full installation of Tex Live. Note that the text should start with a dash '-'.
* **R_VERSION**: THe version of base R installation. Default is `latest`.
* **RSTUDIO_VERSION**: The version of RStudio Server installation. Default is `2023.12.0+369`. This option does not have an effect while running pre-built containers. It is used while creating the Docker image from sctracth.

For other arguments, see **.env** file.

## Running the Docker Container

One may run the Docker Container using the codes below:

1. Pull pre-built container (using AMD-64 platform) from Docker Hub.

```
ARCH=amd64 TEX_SUFFIX='-texlive-full' docker compose -f rstudio-server.yml pull
```

2. Run container from pulled image.

```
ARCH=amd64 TEX_SUFFIX='-texlive-full' docker compose -f rstudio-server.yml up -d
```

One may change the **rstudio-server.yml** file to customize the docker container, e.g., binding volumes, adding services, changing the localhost ports, etc. See Docker Compose file documentation for more details on how to edit YAML file to customize Docker Containers.

