#!/usr/bin/env python3
"""Preserve a public Mitton source and its download provenance.

Data: Original article, replication code, or source documentation for Todd
Mitton, Methodological Variation in Empirical Corporate Finance (2022),
DOI 10.1093/rfs/hhab030. Supply the observed source URL and local destination.
Access date, original/final URL, content type, size, and SHA-256 are recorded
beside the unmodified download. Requires Python 3 and curl. This script does not acquire licensed data,
execute downloaded code, or transform empirical variables.
"""

import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile
import zipfile


def acquire(url, destination, expected_kind, source_title):
    destination = Path(destination).expanduser().resolve()
    if not url.startswith("https://"):
        raise ValueError("Use an observed HTTPS source URL.")
    metadata_path = destination.with_name(destination.name + ".provenance.json")
    if destination.exists():
        if not metadata_path.exists():
            raise ValueError("Existing raw file has no provenance; preserve it and use a new destination.")
        metadata = json.loads(metadata_path.read_text())
        digest = hashlib.sha256(destination.read_bytes()).hexdigest()
        if metadata.get("requested_url") != url or metadata.get("sha256") != digest:
            raise ValueError("Existing raw source differs from its provenance; use a new version.")
        print(f"Verified existing source: {destination.name} ({digest})")
        return metadata
    if metadata_path.exists():
        raise ValueError("Provenance exists without its raw file; preserve the record and use a new destination.")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=destination.parent, delete=False, suffix=".partial") as output:
        temporary = Path(output.name)
    try:
        transfer = subprocess.run(
            ["curl", "--fail", "--silent", "--show-error", "--location",
             "--proto", "=https", "--proto-redir", "=https", "--max-time", "90",
             "--output", str(temporary), "--write-out", "%{url_effective}\n%{content_type}\n%{http_code}",
             "--url", url], capture_output=True, text=True, check=False,
        )
        if transfer.returncode:
            raise ValueError(f"Download failed: {transfer.stderr.strip()}")
        final_url, content_type, http_status = transfer.stdout.splitlines()
        content = temporary.read_bytes()
        if not content:
            raise ValueError("Source returned an empty file.")
        if expected_kind == "pdf" and not content.startswith(b"%PDF-"):
            raise ValueError("Source did not return a PDF; no raw file was accepted.")
        if expected_kind == "zip" and not zipfile.is_zipfile(temporary):
            raise ValueError("Source did not return a ZIP archive; no raw file was accepted.")
        metadata = {
            "source_title": source_title,
            "article_doi": "10.1093/rfs/hhab030",
            "requested_url": url,
            "final_url": final_url,
            "accessed_at_utc": datetime.now(timezone.utc).isoformat(),
            "content_type": content_type,
            "http_status": int(http_status),
            "transport": "curl",
            "kind": expected_kind,
            "size_bytes": len(content),
            "sha256": hashlib.sha256(content).hexdigest(),
            "rights": "Source rights apply; availability does not establish redistribution permission.",
        }
        temporary.replace(destination)
        metadata_path.write_text(json.dumps(metadata, indent=2) + "\n")
        print(f"Preserved source: {destination.name} ({metadata['size_bytes']} bytes; {metadata['sha256']})")
        return metadata
    finally:
        temporary.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", required=True)
    parser.add_argument("--destination", required=True)
    parser.add_argument("--kind", choices=("pdf", "zip", "html", "file"), required=True)
    parser.add_argument("--source-title", required=True)
    args = parser.parse_args()
    try:
        acquire(args.url, args.destination, args.kind, args.source_title)
    except (OSError, ValueError) as error:
        parser.exit(1, f"Acquisition failed: {error}\n")


if __name__ == "__main__":
    main()
