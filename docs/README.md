# chimp documentation

Source for the chimp documentation site, built with Sphinx + MyST and hosted on Read the Docs.

## Run locally

From this folder:

```
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
./watch.sh
```

Open <http://127.0.0.1:8000>. Edits to `.md` files live-reload in the browser.

Next time, just:

```
source .venv/bin/activate
./watch.sh
```

## Notes

- `@VERSION@` and other mdoc variables are **not** substituted in this mode. For a fully-rendered preview, run `sbt 'docs/mdoc'` from the repo root and serve `generated-docs/out/` instead.
- Scala code snippets are verified by `sbt compileDocs` (also runs in CI).
