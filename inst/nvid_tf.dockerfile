FROM nvcr.io/nvidia/tensorflow:23.04-tf2-py3

#
# start checking scvi
#
RUN pip install scvi-tools==0.18.0
#
# get R 4.2.2
#
RUN apt update
RUN apt install --yes ca-certificates
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y tzdata
RUN apt install --yes --no-install-recommends wget  # to add the key
RUN wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc     | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
RUN echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu focal main"     > /etc/apt/sources.list.d/cranapt.list
RUN apt update
RUN wget -q -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc     | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/"     > /etc/apt/sources.list.d/cran-ubuntu.list
RUN apt update
RUN apt install -y r-base-core
#
# get deps for scvi-tools
#
RUN pip install scanpy==1.9.1
RUN pip install scikit-misc==0.1.4
RUN pip install matplotlib==3.6.3
RUN pip install numpy==1.23.1
RUN apt install -y libssl-dev
RUN apt install -y libharfbuzz-dev libfribidi-dev
RUN apt install -y libxml2-dev libfontconfig1-dev
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
RUN Rscript -e "install.packages('BiocManager')"
RUN Rscript -e "BiocManager::install(c('remotes', 'devtools', 'basilisk'), Ncpus=2)"
RUN Rscript -e "BiocManager::install(c('vjcitn/littleDeep', 'tensorflow', 'keras'))"

# docker  run --gpus all --ipc=host --ulimit memlock=-1 --ulimit stack=67108864  -ti vjcitn/bioctfml:0.0.1 bash
