FROM fedora
RUN curl -o /etc/yum.repos.d/bintray-reznikmm-matreshka.repo \
 https://bintray.com/reznikmm/matreshka/rpm && \
 dnf --assumeyes install gcc-gnat && \
 dnf --assumeyes install gprbuild && \
 dnf --assumeyes install make && \
 dnf --assumeyes install gdb && \
 dnf --assumeyes install glew-devel && \
 dnf --assumeyes install glfw-devel && \
 dnf --assumeyes install --repo bintray--reznikmm-matreshka matreshka-devel &&\
 dnf --assumeyes install --repo bintray--reznikmm-matreshka libgnatutil &&\
 dnf --assumeyes install --repo bintray--reznikmm-matreshka asis &&\
 dnf --assumeyes install --repo bintray--reznikmm-matreshka matreshka-a2sj &&\
useradd max
COPY adagl.tar.gz /src/

