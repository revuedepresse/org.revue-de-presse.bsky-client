# https://hub.docker.com/r/arm64v8/rust/tags?name=slim-bulls
FROM arm64v8/rust:1.85-slim-bullseye

COPY ./scryer-prolog /opt/local/scryer-prolog
COPY ./ /var/www/org.revue-de-presse.bsky

WORKDIR /opt/local/scryer-prolog

RUN apt-get update && \
    apt-get install -y \
    build-essential \
    libssl-dev musl-tools \
    neovim \
    pkg-config  \
    systemtap-sdt-dev \
    vim && \
    cargo build --bin scryer-prolog --release && \
    mv /opt/local/scryer-prolog/target/release/scryer-prolog /usr/local/bin/scryer-prolog

WORKDIR /var/www/org.revue-de-presse.bsky

CMD ["sh", "-c", "/usr/local/bin/scryer-prolog"]
