
Start new instance



==================================
# Shiny Proxy
https://www.shinyproxy.io/getting-started/

## install java 8>
```
sudo apt-get install openjdk-8-jre-headless
```

## install docker
```
 sudo apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

sudo apt-key fingerprint 0EBFCD88

sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
   
   
sudo apt-get install docker-ce docker-ce-cli containerd.io

sudo docker run hello-world
```

### config docker

Use DOCKER_OPTS to modify the daemon startup options.                         
```
sudo mkdir -p /etc/systemd/system/docker.service.d/
sudo cat << EOF >  override.conf
[Service]
ExecStart=
ExecStart=/usr/bin/dockerd -H unix:// -D -H tcp://127.0.0.1:2375
EOF
sudo mv override.conf /etc/systemd/system/docker.service.d/override.conf

sudo systemctl daemon-reload
sudo systemctl restart docker
```


## get shinyproxy
```
mkdir ~/shinyproxy
cd ~/shinyproxy
wget https://www.shinyproxy.io/downloads/shinyproxy-2.3.0.jar
```


## Edit config settings

```
cd ~/shinyproxy
sudo cat << EOF > application.yml
proxy:
  title: Open Analytics Shiny Proxy
  logo-url: http://www.openanalytics.eu/sites/www.openanalytics.eu/themes/oa/logo.png
  landing-page: /
  heartbeat-rate: 10000
  heartbeat-timeout: 60000
  port: 3838
  authentication: none
  admin-groups: scientists
  hide-navbar: true
  # Example: 'simple' authentication configuration
  users:
  - name: jack
    password: password
    groups: scientists
  - name: jeff
    password: password
    groups: mathematicians
  # Example: 'ldap' authentication configuration
  ldap:
    url: ldap://ldap.forumsys.com:389/dc=example,dc=com
    user-dn-pattern: uid={0}
    group-search-base:
    group-search-filter: (uniqueMember={0})
    manager-dn: cn=read-only-admin,dc=example,dc=com
    manager-password: password
  # Docker configuration
  docker:
    cert-path: /home/none
    url: http://localhost:2375
    port-range-start: 20000
  specs:
  - id: 01_hello
    display-name: Hello Application
    description: Application which demonstrates the basics of a Shiny app
    container-cmd: ["R", "-e", "shinyproxy::run_01_hello()"]
    container-image: openanalytics/shinyproxy-demo
    access-groups: [scientists, mathematicians]
  - id: 06_tabsets
    container-cmd: ["R", "-e", "shinyproxy::run_06_tabsets()"]
    container-image: openanalytics/shinyproxy-demo
    access-groups: scientists
  - id: webdart
    display-name: Web Dart
    container-cmd:  ["R", "-e", "shiny::runApp('/root/webDART')"]
    container-image: docker-webdart
    container-volumes: ["/home:/home"]
    access-groups: scientists

logging:
  file:
    shinyproxy.log
EOF
```

## generate docker image

```
sudo docker pull openanalytics/r-base



cd ~/webDART
cat << EOF > Dockerfile 
FROM openanalytics/r-base

LABEL maintainer "Tobias Verbeke <tobias.verbeke@openanalytics.eu>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libgdal-dev \ 
    libproj-dev \
    libudunits2-dev

# system library dependency for the euler app
#RUN apt-get update && apt-get install -y \
#    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('shinyjs'), repos='https://cloud.r-project.org/')"

# install dependencies of the app
RUN R -e "install.packages(c('raster', 'leaflet', 'rgdal'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('rgeos'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('gower'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('data.table'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('rpart'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('ggplot2'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('gridExtra'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('ggridges'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('visNetwork'), repos='https://cloud.r-project.org/')"
#RUN R -e "install.packages(c('mapview'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('CausalImpact'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('sparkline'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('writexl'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('units'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('sf'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('leafem'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('leafpop'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('mapview'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('shinyjs'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/webDART
COPY app /root/webDART

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/webDART')"]
EOF


cat << EOF > Rprofile.site
local({ 
     options(shiny.port = 3838, shiny.host = "0.0.0.0")
     })
EOF


sudo docker build -t docker-webdart .
# test
# sudo docker run -p 3838:3838 -v /data:/data docker-webdart
#   


## to run for testing
cd ~/shinyproxy
java -jar shinyproxy-2.3.0.jar

## to deploy
sudo cp application.yml /etc/shinyproxy/application.yml

```
