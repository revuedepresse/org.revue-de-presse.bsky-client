# BlueSky client for Revue-de-presse.org

:uk: [Revue-de-presse.org](https://github.com/revuedepresse) serves press titles curated daily from official French Media Bluesky accounts.

All development is delivered under free and open-source software licence.

:fr: [Revue-de-presse.org](https://github.com/revuedepresse) est un projet citoyen indépendant qui s'adresse à toute personne curieuse de l'actualité et de l'influence des médias sur l'opinion.

## Requirements

 - [jq](https://jqlang.org/)
 - [psql](https://www.postgresql.org/docs/current/app-psql.html)
 - [scryer-prolog](https://scryer.pl)

## Documentation

```shell
make help
```

## Build

With [rustup](https://github.com/mthom/scryer-prolog/blob/4fc4152eaca6b53947f7fdb564d1734296e7a3b8/README.md#installing-scryer-prolog):

```shell
cargo build --bin scryer-prolog --release
```

With docker:

```shell
docker build -t org.revue-de-presse.bsky.stable .
```

## License

GNU General Public License v3.0 or later

See [COPYING](./COPYING) to see the full text.


## Acknowledgment

We're grateful towards all amazing contributors involved in the following  
communities, organizations and projects (in lexicographic order):

  - [Debian](https://www.debian.org/)
  - [Docker](docker.com)
  - [GitHub](https://github.com/)
  - [Scryer Prolog](https://www.scryer.pl/)
  - [Ubuntu](https://ubuntu.com/)
