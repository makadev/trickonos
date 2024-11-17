ARG WORKSPACE=/app
ARG FPC_VARIANT=3.2.2-bookworm-full
#ARG DEBIAN_VARIANT=focal

## stage for fpc
FROM freepascal/fpc:${FPC_VARIANT} AS fpc-stage

## setup default user, sudo and install needed packages
ARG USERNAME=developer
ARG USER_UID=1000
ARG USER_GID=1000
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
        ## because
        nano \
        ## may be needed
        sudo \
        ## needed for source code checkout and commits from within container
        ## f.e. https, git+ssh, signing
        git ca-certificates openssl openssh-client gnupg2 \
        ## might be needed here and there
        python-is-python3 \
        python3-pygments \
        ## general usefull tools which might be needed here and there
        wget \
        xz-utils \
        unzip \
        ## good to have, other locale
        locales-all \
        ## extra for freepascal vscode plugin
        global exuberant-ctags pip \
    && groupadd --gid ${USER_GID} ${USERNAME} \
    && useradd --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME} \
    && echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME} \
    && chmod 0440 /etc/sudoers.d/${USERNAME} \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

ENV GTAGSCONF=/usr/share/global/gtags/gtags.conf
ENV GTAGSLABEL=pygments

USER $USERNAME

WORKDIR ${WORKSPACE}