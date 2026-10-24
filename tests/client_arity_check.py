"""Static check: every predicate a source file imports or calls from
src/infrastructure/pg/client.pl exists with that arity.

Scryer only reports an unknown predicate when the call runs, so after the
pg_session refactor (b782453) changed the client's arities, the list import
kept calling value/3, execute/2 and friends and failed only in production.
This check catches that at test time:

  1. every `use_module('<file>', [name/N, ...])` in src/ must name
     predicates that <file> exports;
  2. every call to a client.pl export must use an exported arity.

Usage: python3 -I tests/client_arity_check.py [repository root]
Exits 1 and prints file:line for each mismatch.
"""
import pathlib
import re
import sys

ROOT = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else ".").resolve()
SRC = ROOT / "src"
CLIENT = SRC / "infrastructure" / "pg" / "client.pl"

MODULE_DECL = re.compile(r":-\s*module\(\s*'?[\w]+'?\s*,\s*\[(.*?)\]\s*\)\s*\.", re.S)
USE_MODULE = re.compile(r":-\s*use_module\(\s*'([^']+)'\s*,\s*\[(.*?)\]\s*\)\s*\.", re.S)
INDICATOR = re.compile(r"([a-z][A-Za-z0-9_]*)\s*/\s*(\d+)")


def strip_comments(text):
    """Blank out % line comments and /* */ blocks, keeping line numbers."""
    text = re.sub(r"/\*.*?\*/", lambda m: re.sub(r"[^\n]", " ", m.group(0)), text, flags=re.S)
    return "\n".join(re.sub(r"(^|[^0-9'\"\\])%.*$", r"\1", line) for line in text.split("\n"))


def exports(path):
    match = MODULE_DECL.search(strip_comments(path.read_text()))
    if not match:
        return None
    found = {}
    for name, arity in INDICATOR.findall(match.group(1)):
        found.setdefault(name, set()).add(int(arity))
    return found


def count_args(text, start):
    """Number of top-level arguments of the call whose '(' ends at start."""
    depth, quote, args, i = 0, None, 1, start
    while i < len(text):
        c = text[i]
        if quote:
            if c == "\\":
                i += 1
            elif c == quote:
                quote = None
        elif c in "\"'`":
            quote = c
        elif c in "([{":
            depth += 1
        elif c in ")]}":
            if depth == 0:
                return args
            depth -= 1
        elif c == "," and depth == 0:
            args += 1
        i += 1
    return None


def line_of(text, offset):
    return text.count("\n", 0, offset) + 1


problems = []
client_exports = exports(CLIENT)

for path in sorted(SRC.rglob("*.pl")):
    text = strip_comments(path.read_text())
    rel = path.relative_to(ROOT)

    for match in USE_MODULE.finditer(text):
        target = (path.parent / (match.group(1) + ".pl")).resolve()
        if not target.is_file():
            continue
        target_exports = exports(target)
        if target_exports is None:
            continue
        for name, arity in INDICATOR.findall(match.group(2)):
            if int(arity) not in target_exports.get(name, set()):
                problems.append(
                    f"{rel}:{line_of(text, match.start())}: imports {name}/{arity}, "
                    f"which {target.relative_to(ROOT)} does not export"
                )

    if path == CLIENT:
        continue
    for name, arities in client_exports.items():
        for call in re.finditer(r"(?<![A-Za-z0-9_:])" + re.escape(name) + r"\(", text):
            arity = count_args(text, call.end())
            if arity is not None and arity not in arities:
                problems.append(
                    f"{rel}:{line_of(text, call.start())}: calls {name}/{arity}, "
                    f"client.pl exports {', '.join(f'{name}/{a}' for a in sorted(arities))}"
                )

for problem in problems:
    print(f"[KO] {problem}")
if problems:
    print(f"[KO] client_arity_check: {len(problems)} mismatch(es)")
    sys.exit(1)
print("[OK] client_arity_check: imports and client.pl calls match exported arities")
