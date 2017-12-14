FROM docker.elastic.co/elasticsearch/elasticsearch:5.6.4

# build git so we get v2.x
USER root
RUN yum groupinstall -y --nogpgcheck "Development Tools"
RUN yum install -y --nogpgcheck curl-devel gettext-devel make openssl-devel perl-CPAN perl-devel zlib-devel && yum clean all
RUN wget https://github.com/git/git/archive/v2.15.1.tar.gz -O git.tar.gz
RUN tar xzvf git.tar.gz
WORKDIR /usr/share/elasticsearch/git-2.15.1
RUN make configure
RUN ./configure --prefix=/usr/local
RUN make install

# fetch the 1.8 jdk
RUN rm -rf /etc/yum.conf
RUN touch /etc/yum.conf
RUN yum install -y --nogpgcheck java-1.8.0-openjdk-devel

# fetch other runbld deps
WORKDIR /usr/share/elasticsearch
RUN rpm -Uvh https://yum.puppet.com/puppet5/puppet5-release-el-7.noarch.rpm
RUN yum install -y puppet-agent maven-compiler-plugin
RUN wget https://bootstrap.pypa.io/get-pip.py && python get-pip.py && pip install nose

WORKDIR /opt
RUN wget http://mirrors.ocf.berkeley.edu/apache/maven/maven-3/3.5.2/binaries/apache-maven-3.5.2-bin.tar.gz
RUN tar xvzf apache-maven-3.5.2-bin.tar.gz
RUN ln -s /opt/apache-maven-3.5.2/bin/mvn /usr/local/bin/mvn

WORKDIR /usr/local
RUN wget https://redirector.gvt1.com/edgedl/go/go1.9.2.linux-amd64.tar.gz
RUN tar xzvf go1.9.2.linux-amd64.tar.gz

WORKDIR /usr/local/bin
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && chmod a+x lein && lein

WORKDIR /usr/share/elasticsearch
USER elasticsearch
RUN mkdir /usr/share/elasticsearch/go
RUN export PATH=$PATH:/usr/local/go/bin && go get -u github.com/jstemmer/go-junit-report

# setup the symlinks we'll need
USER root
RUN ln -s /usr/local/go/bin/go /usr/local/bin/go
RUN ln -s /usr/share/elasticsearch/go/bin/go-junit-report /usr/local/bin/go-junit-report
RUN ln -s /opt/puppetlabs/bin/facter /usr/local/bin/facter

# push runbld code into docker image
COPY . /usr/share/elasticsearch/runbld/
RUN chown -R elasticsearch /usr/share/elasticsearch/runbld

USER elasticsearch

# setup git config
RUN git config --global user.name "Docker"
RUN git config --global user.email "docker@elastic.co"

# fetch runbld's clojure deps
WORKDIR /usr/share/elasticsearch/runbld
RUN lein deps

WORKDIR /usr/share/elasticsearch

ENV http.host 0.0.0.0
ENV transport.host 127.0.0.1
ENV xpack.security.enabled false

EXPOSE 9200
