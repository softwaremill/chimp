# chimp documentation

Source for the chimp documentation site, built with Sphinx + MyST and hosted on Read the Docs.

## Setup

From this folder, once:

```
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Making changes

1. **Update the docs** — edit the `.md` source files under `docs/`.
2. **Build & watch to preview** — run `./watch.sh` (after `source .venv/bin/activate`) and open <http://127.0.0.1:8000>; edits live-reload in the browser.
3. **Commit only `docs/`** — do **not** commit `generated-docs/`. Read the Docs builds from `generated-docs/out/`, but that mdoc output is regenerated and committed automatically as part of the release.
4. **Verify** — run `sbt compileDocs` from the repo root to check that the Scala snippets in your docs compile.

## Notes

- `0.4.0` and other mdoc variables are **not** substituted in the local watch mode. For a fully-rendered preview, run `sbt docs/mdoc` from the repo root and serve `generated-docs/out/` (don't commit the result).
