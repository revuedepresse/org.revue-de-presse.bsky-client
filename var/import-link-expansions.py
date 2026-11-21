"""Turn a resolver's findings into ledger upserts.

The collector never resolves a shortened link itself (see
src/infrastructure/link/unfurl.pl): it records what it has not seen as
`pending`. A resolver driving a real browser answers that worklist out of
band and leaves a file behind — either a JSON object of
{short_url: expanded_url}, or JSON lines carrying `url` plus `status` and the
address found, named `final` by a resolver that followed the redirect or
`target` by one that read the address the page declares for itself (what
src/link_shortener.pl's destination_url/2 does). This turns such a file into SQL for `publication_link`.

  python3 -I var/import-link-expansions.py findings.jsonl > upserts.sql

Reads nothing but the files named on the command line, and writes SQL to
stdout: applying it is the operator's call, on whichever database.
"""
import json
import sys
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit

# Campaign parameters belong to the redirect, not to the article: two posts
# linking the same piece through different campaigns would otherwise be
# stored as two addresses. `v` (YouTube) and the like are left alone, since
# without them the address stops working.
TRACKING_PREFIXES = ('utm_', 'at_', 'xtor', 'xts', 'mc_', 'pk_', 'piwik_')
TRACKING_EXACT = {
    'fbclid', 'gclid', 'gbraid', 'wbraid', 'igshid', 'ref_src', 'ref_url',
    'cmpid', 'ecid', 'feature', 's_kwcid', 'sr_share', 'WT.mc_id', 'spm',
    'twclid', 'ttclid', 'msclkid', 'vgo_ee',
}


def tracking(name):
    return name in TRACKING_EXACT or name.startswith(TRACKING_PREFIXES)


def permalink(url):
    """The address of the article itself: campaign noise removed, order kept."""
    parts = urlsplit(url)
    kept = [(k, v) for k, v in parse_qsl(parts.query, keep_blank_values=True)
            if not tracking(k)]
    fragment = '' if tracking(parts.fragment.split('=')[0]) else parts.fragment
    return urlunsplit((parts.scheme, parts.netloc, parts.path,
                       urlencode(kept), fragment))


# A path a shortener writes when its own template had nothing to interpolate.
# `https://www.youtube.com/undefined` is not an address that ever worked; it is
# a JavaScript value that reached a URL. Eight l.humanite.fr links declare
# exactly that, and a redirect landing there has lost its article as surely as
# one landing on a home page.
PLACEHOLDER_SEGMENTS = {'undefined', 'null', 'NaN', 'none', '[object Object]'}


def leads_nowhere(short, final):
    """A redirect that has lost the article it pointed to: it lands on a home
    page, or on a path the shortener failed to fill in. Storing either would
    replace a link that may yet be fixed upstream with one that is certainly
    broken."""
    if urlsplit(short).path in ('', '/'):
        return False
    path = urlsplit(final).path
    if path in ('', '/'):
        return True
    return any(segment in PLACEHOLDER_SEGMENTS
               for segment in path.strip('/').split('/'))


def quote(value):
    return "'" + str(value).replace("'", "''") + "'"


def rows(path):
    with open(path, encoding='utf-8') as handle:
        text = handle.read()
    # A map and JSON lines both start with '{', so tell them apart by
    # parsing: only a whole-file object is a map.
    try:
        body = json.loads(text)
    except ValueError:
        body = None
    if isinstance(body, dict):
        for short, expanded in body.items():
            yield {'url': short, 'final': expanded, 'status': 200}
        return
    for line in text.split('\n'):
            line = line.strip()
            if not line:
                continue
            try:
                yield json.loads(line)
            except ValueError:
                continue


def statement(row):
    short = row.get('url')
    if not short:
        return None
    # `final` from a resolver that followed the redirect, `target` from one
    # that read what the page declares for itself. A record carrying neither
    # says nothing about the link.
    final = row.get('final') or row.get('target')
    code = row.get('status') if isinstance(row.get('status'), int) else None
    if final:
        final = permalink(final)
    resolved = bool(final) and final != short and code == 200
    if resolved and leads_nowhere(short, final):
        resolved, code = False, 404
    if resolved:
        status = 'resolved'
    elif code in (404, 410):
        status = 'dead'
    elif code is not None:
        status = 'refused'
    else:
        return None  # a transport error says nothing about the link
    return (
        "INSERT INTO public.publication_link "
        "(short_url, expanded_url, status, http_status, attempts, first_seen_at, last_attempt_at) "
        "VALUES ({short}, {final}, {status}, {code}, 1, now()::text, now()::text) "
        "ON CONFLICT (short_url) DO UPDATE SET "
        "expanded_url = EXCLUDED.expanded_url, "
        "status = EXCLUDED.status, "
        "http_status = EXCLUDED.http_status, "
        "attempts = public.publication_link.attempts + 1, "
        "last_attempt_at = EXCLUDED.last_attempt_at;"
    ).format(
        short=quote(short),
        final=quote(final) if resolved else 'NULL',
        status=quote(status),
        code='NULL' if code is None else str(code),
    )


def main(paths):
    seen, counts = set(), {'resolved': 0, 'refused': 0, 'dead': 0, 'skipped': 0}
    print('BEGIN;')
    for path in paths:
        for row in rows(path):
            short = row.get('url')
            if not short or short in seen:
                continue
            sql = statement(row)
            if sql is None:
                counts['skipped'] += 1
                continue
            seen.add(short)
            print(sql)
            counts["resolved" if "'resolved'" in sql else ("dead" if "'dead'" in sql else "refused")] += 1
    print('COMMIT;')
    print(
        '-- {resolved} resolved, {refused} refused, {dead} dead, '
        '{skipped} without an answer'.format(**counts),
        file=sys.stderr,
    )


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(__doc__, file=sys.stderr)
        raise SystemExit(2)
    main(sys.argv[1:])
